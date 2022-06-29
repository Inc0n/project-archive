
#include "BitSocket.hpp"
#include <cmath>
#include <cstdio>

#ifndef debug
#define debugLog(...)
#endif

BitSocket::BitSocket(int txDPin, int txFPin, int txLPin, int rxDPin, int rxFPin, int rxLPin) :
    QObject(nullptr),
    gpio(GPio::getInstance()),
    txDataPin(txDPin),
    txFlagPin(txFPin),
    txLockPin(txLPin),
    rxDataPin(rxDPin),
    rxFlagPin(rxFPin),
    rxLockPin(rxLPin)
{
    mx_sendQueue = new std::deque<QVector<char>>();

    tim = (timespec*)calloc(1, sizeof(timespec));
    tim->tv_nsec = 5000; // in nanoseconds, which 1us

    // printf()
    // start thread
    int rc = pthread_create(&this->recvThread, NULL, &BitSocket::recvData_cAux, (void *)this);
    if (rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
    }
    rc = pthread_create(&this->sendThread, NULL, &BitSocket::sendData_cAux, (void *)this);
    if (rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
    }
}


BitSocket::~BitSocket() {
    free(this->tim);
    delete this->mx_sendQueue;
}

#define BYTE_SIZE 8

void BitSocket::sendByte(char abyte)
{
    for (uint i = 0; i < BYTE_SIZE; ++i) {
        char bit = (abyte & (1 << 7)); // get most significant
        bit >>= 7;                     // transform into bit
        gpio->setPinTo(txDataPin, bit); // set data bit
        set_pin(gpio, txFlagPin);       // inform new data
        loop_until_cleared(gpio, txFlagPin); // make sure data is read
        //
        abyte <<= 1; // cycle new bits
    }
}

void BitSocket::sendData(const char *data, uint size)
{
    // mutex
    loop_until_cleared(gpio, txLockPin) {
        gpio->sleep(); // prevent 100% cpu usage
    };
    set_pin(gpio, txLockPin);
    // handshake
    set_pin(gpio, txFlagPin);
    loop_until_cleared(gpio, txFlagPin);
    // send data in packets - size followed by data
    sendByte(size);
    for (uint i = 0; i < size; ++i) {
        sendByte(data[i]);
    }
    // clear mutex
    clear_pin(gpio, txLockPin);
    // debug log
    debugLog("done sending: ");
    debugLog("{");
    for (uint i = 0; i < size; ++i) {
        debugLog("%#x ", data[i]);
    }
    debugLog("}\n");
}

bool BitSocket::hasDataToSend() {
    mx_queue.lock();
    debugLog("here\n");
    bool is_empty = mx_sendQueue->empty();
    debugLog("here again\n");
    mx_queue.unlock();
    return !is_empty;
}

// public method for queueing data to send
void BitSocket::queueSendData(const char *data, uint size)
{
    mx_queue.lock();
    auto vec_data = QVector<char>(data, data + size);
    mx_sendQueue->push_back(vec_data);
    mx_queue.unlock();
}

// private method for sending queued data
void BitSocket::sendQueuedData()
{
    // mutex access
    mx_queue.lock();
    auto vec_data = mx_sendQueue->front();
    mx_sendQueue->pop_front();
    mx_queue.unlock();
    // send
    sendData(&vec_data[0], vec_data.size());
}

//

char BitSocket::readByte()
{
    char abyte = 0;
    // wait for recv read flag
    loop_until_set(gpio, rxFlagPin) {
        // prevent deadlock
        if (is_pin_clear(gpio, rxLockPin)) {
            printf("flag cleared while reading ERROR\n");
            return '\0';
        }
    }
    for (uint i = BYTE_SIZE; i > 0; ) {
        loop_until_set(gpio, rxFlagPin);
        abyte += (gpio->getPin(rxDataPin) << (--i));
        clear_pin(gpio, rxFlagPin); // inform bit read

    }
    // debugLog("read byte: %#x\n", abyte);
    return abyte;
}

bool BitSocket::hasDataToRecv() {
    return is_pin_set(gpio, rxLockPin);
}

QVector<char> BitSocket::recvData(uint size)
{
    auto data = QVector<char>();
    data.reserve(size);

    // mutex
    loop_until_set(gpio, rxLockPin) {
        // gpio->sleep();
    }
    // handshake
    loop_until_set(gpio, rxFlagPin);
    clear_pin(gpio, rxFlagPin);
    // read bytes
    size = readByte();
    for (uint i = 0; i < size; ++i) {
        // if (is_pin_clear(gpio, rxLockPin)) break;
        data.push_back(readByte());
    }
    // debug log
    debugLog("received {");
    for (uint i = 0; i < data.size(); ++i) {
        debugLog("%#x ", data[i]);
    }
    debugLog("}\n");
    return data;
}
#ifndef _BITSOCKET_H
#define _BITSOCKET_H

// #include <gpio/gpio.hpp>
#include "../gpio/gpio.hpp"
#include <QObject>
#include <QVector>
#include <tuple>
#include <deque>
#include <ctime>
#include <pthread.h>

#include <chrono>

typedef QVector<char> Bytes_t;

#define TIME_OUT 1000 // 1us in nanoseconds

class BitSocket : public QObject
{
    Q_OBJECT

public:
    BitSocket(QObject *parent = nullptr) :
        QObject(parent), gpio(GPio::getInstance()) {};
    BitSocket(int txDPin, int txFPin, int txLPin, int rxDPin, int rxFPin, int rxLPin);
    ~BitSocket();
    /**
       Send the data specified by the size

       @param data,
              size: number of bytes in the data to be sent
    */
    void sendData(const char *data, uint size);
    /**
       Returns the data received, blocks until data received

       @param size: initial size of the data to be received
    */
    QVector<char> recvData(uint size);
    //
    void queueSendData(const char *data, uint size);

signals:
    void dataReceived(Bytes_t data);

private:
    //
    // static function helper for pthread
    static void *recvData_cAux(void *this_instance) {
        BitSocket *self = (BitSocket *)this_instance;

        // note: commented code for data rate measurements
        while (true) {
            bool recved = false;
            if (self->hasDataToRecv()) {
                // arbitrary number 64 used
                // but 38 bytes at least
                auto data = self->recvData(64);
                self->dataReceived(data); // emit signal
                recved = true;
            }
            if (!recved) nanosleep(self->tim, NULL);
        }
    }
    static void *sendData_cAux(void *this_instance) {
        BitSocket *self = (BitSocket *)this_instance;

        // note: commented code for data rate measurements
        while (true) {
            bool sent = false;
            typedef std::chrono::high_resolution_clock Clock;
            typedef std::chrono::nanoseconds nanoseconds;
            Clock::time_point t0 = Clock::now();
            while (self->hasDataToSend()) {
                if (!sent) sent = true;
                self->sendQueuedData();
            }
            Clock::time_point t1 = Clock::now();
            if (!sent) nanosleep(self->tim, NULL);
            else {
                nanoseconds ms = std::chrono::duration_cast<nanoseconds>(t1 - t0);
                printf("%ld ns\n", ms.count());
            }
        }
    }
    struct timespec *tim;
    //
    pthread_t recvThread;
    pthread_t sendThread;
    GPio *gpio;
    std::deque<QVector<char>> *mx_sendQueue;
    std::mutex mx_queue;
    // gpio pins
    int txDataPin = -1; // send data pin channel
    int txFlagPin = -1;
    int txLockPin = -1;
    int rxDataPin = -1;
    int rxFlagPin = -1;
    int rxLockPin = -1;
    //
    void sendByte(char abyte);
    char readByte();
    //
    void sendQueuedData();
    bool hasDataToSend();
    bool hasDataToRecv();
};


#endif // _BITSOCKET_H
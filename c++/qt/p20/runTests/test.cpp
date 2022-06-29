

#include <thread>

typedef double qreal;

struct QPointF {
    QPointF() : QPointF(0.0, 0.0) {}
    QPointF(qreal p1, qreal p2) : p1(p1), p2(p2) {}
    qreal p1, p2;
    qreal x() { return p1; }
    qreal y() { return p2; }
};

// #include <gpio/gpio.hpp>
#include "../gpio/gpio.hpp"
#include "../gui-bitstream/BitSocket.hpp"
#include "../gui-bitstream/DrawCmd.h"

void print_list(byte_t *data, uint size)
{
    printf("{");
    for (uint i = 0; i < size; ++i) {
        printf("%#x ", data[i]);
    }
    printf("}\n");
}

int main(int argc, char *argv[])
{
    BitSocket *sock1 = new BitSocket(0, 1, 2, 3, 4, 5);
    BitSocket *sock2 = new BitSocket(3, 4, 5, 0, 1, 2);

    DrawCmd cmd(4, QPointF(0.2, 0.3), QPointF(0.2, 0.4));
    //
    uint size = 0;
    byte_t* data = cmd.serialize(&size);

    print_list(data, size);

    std::thread threadObj([sock2]() {
                              auto data = sock2->recvData(256);
                              DrawCmd cmd(data);
                              cmd.print();
                          });
    sock1->sendData(data, size);

    threadObj.join();
    //
    return 0;
}
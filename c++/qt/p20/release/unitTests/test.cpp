

#include <QtTest/QtTest>

#include <thread>

#include "../gui-bitstream/DrawCmd.h"
#include "../gui/DrawVectorCmd.h"
#include "../gui-bitstream/BitSocket.hpp"
#include "../gpio/gpio.hpp"

void print_list(byte_t *data, uint size)
{
    printf("{");
    for (uint i = 0; i < size; ++i) {
        printf("%#x ", data[i]);
    }
    printf("}\n");
}


class UnitTests: public QObject
{
    Q_OBJECT

// private:

private slots:
    void initTestCase() {
        // qDebug("Called before everything else.");
    }

    void vectorSerialization() {
        QVector<QPointF> points = QVector<QPointF>({
                QPointF(0.1, 0.2),
                QPointF(0.2, 0.3),
                QPointF(0.3, 0.4)
            });
        DrawCollectionCmd cmd(points);
        //
        uint size = 0;
        byte_t* data = cmd.serialize(&size);

        DrawCollectionCmd deserialized(data);

        // testing
        QVERIFY(deserialized.getMode() == 0);
        QVERIFY(deserialized.startPoint().x() == 0.1);
        QVERIFY(deserialized.startPoint().y() == 0.2);
        QVERIFY(deserialized.endPoint().x() == 0.3);
        QVERIFY(deserialized.endPoint().y() == 0.4);
        //
        free(data);
    }
    void serialization() {
        DrawCmd cmd(QPointF(0.2, 0.3), QPointF(0.2, 0.4));
        //
        uint size = 0;
        byte_t* data = cmd.serialize(&size);

        DrawCmd deserialized = DrawCmd(data);

        // testing
        QVERIFY(deserialized.getMode() == 0);
        QVERIFY(deserialized.startPoint().x() == 0.2);
        QVERIFY(deserialized.startPoint().y() == 0.3);
        QVERIFY(deserialized.endPoint().x() == 0.2);
        QVERIFY(deserialized.endPoint().y() == 0.4);
        //
        free(data);
    }
    void bitSokcet() {
        BitSocket *sock1 = new BitSocket(0, 1, 2, 3, 4, 5);
        BitSocket *sock2 = new BitSocket(3, 4, 5, 0, 1, 2);

        DrawCmd cmd(4, QPointF(0.2, 0.3), QPointF(0.2, 0.4));
        //
        uint size = 0;
        byte_t* data = cmd.serialize(&size);

        print_list(data, size);

        std::thread threadObj([sock2]() {
                                  auto data = sock2->recvData(256);
                                  DrawCmd cmd(data.data());
                                  cmd.print();
                              });
        sock1->sendData(data, size);

        threadObj.join();
    }

    void cleanupTestCase()
    {
        // qDebug("Called after myFirstTest and mySecondTest.");
    }
};

QTEST_MAIN(UnitTests)
#include "test.moc"
#include "DrawThread.hpp"

DrawThread::DrawThread(QTcpSocket *clientConnection, QObject *parent)
    : QThread(parent), client(clientConnection)
{
}

void sendData(uint8_t *data)
{
}

void DrawThread::run()
{
    while (!shouldQuit) {
        QByteArray block;
        QDataStream out(&block, QIODevice::WriteOnly);
        out.setVersion(QDataStream::Qt_5_0);
        out << "test";

        tcpSocket.write(block);
        // tcpSocket.disconnectFromHost();
        // tcpSocket.waitForDisconnected(); // block thread until socket disconnected
    }
}
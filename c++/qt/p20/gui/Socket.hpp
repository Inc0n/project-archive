
#ifndef SOCKET_H
#define SOCKET_H

#include <QTcpSocket>
#include <QTcpServer>
#include <QHostAddress>
#include <QMessageBox>
#include <QByteArray>
#include <QWidget>

#include "DrawCmd.h"

class Socket : public QObject
{
    Q_OBJECT

public:
    Socket(QObject *parent = nullptr);
    ~Socket();
    void sendDrawCmd(DrawCmd cmd);

signals:
    void newDrawCmdReceived(DrawCmd cmd);

public slots:
    void onNewConnection();
    void readDrawCmd();

private:
    QTcpSocket *clientSock; // also used for serevr's client connection
    QTcpServer *serverSock;

    bool createTcpServer();
    void createTcpClient();
};

#endif // SOCKET_H
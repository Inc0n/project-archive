
#include "Socket.hpp"

Socket::Socket(QObject *parent) :
    QObject(parent), clientSock(nullptr), serverSock(nullptr)
{
    if (!createTcpServer()) {
        createTcpClient();
    }

    printf("[!] debug socket done init\n");
}

Socket::~Socket()
{
    if (clientSock->isOpen()) {
        clientSock->close();
        clientSock->deleteLater();
    }
    if (serverSock) {
        serverSock->close();
    }
}

#define PORT 8080

bool Socket::createTcpServer() {
    serverSock = new QTcpServer();
    //
    if (!serverSock->listen(QHostAddress::LocalHost, PORT)) {
        printf("unable to start the server %s\n", serverSock->errorString().data());
        // QMessageBox::critical(this,
        //                       tr("Comm Server"),
        //                       tr("Unable to start the server: %1.")
        //                       .arg(serverSock->errorString()));
        serverSock->close();
        serverSock = nullptr;
        return false;
    }
    QString ipAddress = "127.0.0.1";
    printf("The server is running on\n\nIP: %s\nport: %ld\n\nRun the Fortune Client example now.", ipAddress.data(), serverSock->serverPort());

    connect(serverSock,
            &QTcpServer::newConnection,
            this,
            &Socket::onNewConnection);
    return true;
}

void Socket::onNewConnection()
{
    clientSock = serverSock->nextPendingConnection();
    connect(clientSock, &QAbstractSocket::disconnected,
            clientSock, &QObject::deleteLater);
    connect(clientSock, &QIODevice::readyRead, this, &Socket::readDrawCmd);
}

#define MAXSIZE 64

void Socket::readDrawCmd() {

    char *raw = new char[MAXSIZE];
    int size = clientSock->read(raw, MAXSIZE);
    if (size < 0) {
        printf("more than 64 bytes of data sent\n");
    }

    DrawCmd cmd(raw);
    printf("\n");
    for (int i = 0; i < size; ++i) {
        printf("%#x ", raw[i]);
    }
;
    printf(" received data: ");
    cmd.print();
    // while (!in.atEnd())
    // {
    //     QString receiveString;
    //     in >> receiveString;
    //     receiveString.prepend(QString("%1 :: ").arg(socket->socketDescriptor()));
    //     emit newMessage(receiveString);
    // }
    emit newDrawCmdReceived(cmd);
    delete [] raw;
}

void Socket::sendDrawCmd(DrawCmd cmd) {
    uint size = 0;
    char *raw = cmd.serialize(&size);

    printf("\n%d ", size);
    for (int i = 0; i < size; ++i) {
        printf("%#x ", raw[i]);
    }
    printf("\n");
    cmd.print();
    printf("\n");
    if (clientSock && clientSock->isOpen()) {
        clientSock->write(raw, size);
    } else {
        printf("no connection socket is open\n");
    }
}

void Socket::createTcpClient() {
    clientSock = new QTcpSocket();

    printf("[!] debug create client sock\n");

    connect(clientSock, &QIODevice::readyRead, this, &Socket::readDrawCmd);
    // connect(clientSock, &QTcpSocket::error, this, [](socketError) { printf("socket ERROR\n"); });
    connect(clientSock, &QTcpSocket::disconnected, clientSock, &QTcpSocket::deleteLater);

    clientSock->connectToHost(QHostAddress(QHostAddress::LocalHost), PORT);

}
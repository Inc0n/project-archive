
#include <QThread>
#include <QTcpSocket>

class DrawThread : public QThread
{
    Q_OBJECT

public:
    DrawThread(QTcpSocket *clientConnection, QObject *parent);

    void run() override;

signals:
    void error(QTcpSocket::SocketError socketError);

private:
    QTcpSocket *client;
    bool shouldQuit = false;
};
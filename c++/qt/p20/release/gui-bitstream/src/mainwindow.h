#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QMenuBar>
#include <QList>
#include <QByteArray>
#include <QAction>
#include <QCloseEvent>

#include <thread>

#include "scribble-area.h"
#include "BitSocket.hpp"

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    //  = nullptr
    MainWindow(QWidget *parent, BitSocket *sock);
    ~MainWindow();

private slots:
    void setPenColorUI();
    void setPenWidthUI();
    void about();

    void onNewDiagramDrawn(DrawCmd cmd);
    void onDataReceived(Bytes_t data);

private:
    std::thread *thread;
    // sockets
    BitSocket *bitsock;
    QVector<QGraphicsItem *> items;
    //
    void createActions();
    void createMenus();

    ScribbleArea *scribbleArea;

    QMenu *optionMenu;
    QMenu *helpMenu;

    QAction *penColorAct;
    QAction *penWidthAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
};
#endif // MAINWINDOW_H

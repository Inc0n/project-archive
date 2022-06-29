
#include "mainwindow.h"
// #include "testWindow.h"
#include "BitSocket.hpp"

#include <QApplication>
#include <QHBoxLayout>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    // MainWindow w;
    // w.show();

    auto mainWindow = new QMainWindow;
    // setup ui
    QHBoxLayout *layout = new QHBoxLayout;

    MainWindow *subWindow1 =
        new MainWindow(mainWindow, new BitSocket(0, 1, 2, 3, 4, 5));
    MainWindow *subWindow2 =
        new MainWindow(mainWindow, new BitSocket(3, 4, 5, 0, 1, 2));
    printf("[!] debug done init\n");
    layout->addWidget(subWindow1);
    layout->addWidget(subWindow2);

    QWidget *window = new QWidget();
    window->setLayout(layout);
    mainWindow->setCentralWidget(window);

    //
    mainWindow->show();

    return a.exec();
}
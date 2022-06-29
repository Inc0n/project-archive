
#include "mainwindow.h"
#include "testWindow.h"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    // MainWindow w;
    // w.show();

    TestWindow *mainWindow = new TestWindow;
    mainWindow->show();

    return a.exec();
}

#include "mainwindow.h"
#include <QApplication>
#include <qtwebengineglobal.h>

int main(int argc, char *argv[])
{
    QCoreApplication::setOrganizationName("QtExamples");
    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts);
    QtWebEngine::initialize();
    QApplication a(argc, argv);

    MainWindow mainwindow;
    mainwindow.show();

    return a.exec();
}
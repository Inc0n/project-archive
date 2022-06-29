
#include "testWindow.h"
#include <QHBoxLayout>

TestWindow::TestWindow(QWidget *parent) :
    QMainWindow(parent)
{
    this->setupUI();
}

TestWindow::~TestWindow()
{
    // delete ui;
}

//

void TestWindow::setupUI() {
    QHBoxLayout *layout = new QHBoxLayout;

    MainWindow *subWindow1 = new MainWindow(this);
    MainWindow *subWindow2 = new MainWindow(this);
    printf("[!] debug done init\n");
    layout->addWidget(subWindow1);
    layout->addWidget(subWindow2);

    QWidget *window = new QWidget();
    window->setLayout(layout);
    this->setCentralWidget(window);
}
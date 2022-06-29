#ifndef TESTWINDOW_H
#define TESTWINDOW_H

#include <QMainWindow>
#include "mainwindow.h"

class TestWindow : public QMainWindow
{
    Q_OBJECT

public:
    TestWindow(QWidget *parent = nullptr);
    ~TestWindow();

/* protected: */

/* private slots: */

private:
    void setupUI();
};
#endif // TESTWINDOW_H

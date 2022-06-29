#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QToolBar>
#include <QPushButton>
#include <QLabel>
#include <QLineEdit>
#include <QHBoxLayout>

#include <QWebEngineView>
#include <QWebEngineProfile>
#include <QtWebChannel/QtWebChannel>

#include "webview.hpp"

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

/* protected: */

private slots:
    void onUrlChanged(const QUrl &url);

private:
    QToolBar *toolbar;
    //
    QPushButton *backBtn;
    QPushButton *forwardBtn;
    QLabel *urlEdit;
    //
    QWebEngineProfile *profile;
    WebView *view;
};
#endif // MAINWINDOW_H

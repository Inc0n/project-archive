
#include "mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    view = new WebView(this);
    view->load(QUrl("https://web.whatsapp.com/"));
    view->show();
    connect(view, &QWebEngineView::urlChanged,
            this, &MainWindow::onUrlChanged);

    setCentralWidget(view);

    toolbar = new QToolBar(this);
    //
    backBtn = new QPushButton("<");
    forwardBtn = new QPushButton(">");
    //
    urlEdit = new QLabel("https://web.whatsapp.com/", this);
    urlEdit->setFrameStyle(QFrame::Panel);
    // QLineEdit *phoneEdit = new QLineEdit(this);
    // urlEdit->setBuddy(phoneEdit);
    //
    //
    toolbar->addAction("<", this, []() {
            qDebug() << "left and i got self as well";
        });
    toolbar->addAction(">", this, []() {
            qDebug() << "right and i got self as well";
        });
    // toolbar->addWidget(backBtn);
    // toolbar->addWidget(forwardBtn);
    toolbar->addWidget(urlEdit);
    //
    toolbar->setMovable(false);
    toolbar->setFloatable(false);
    this->addToolBar(Qt::TopToolBarArea, toolbar);

    profile = QWebEngineProfile::defaultProfile();
    profile->setHttpUserAgent("(X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.129 Safari/537.36");
    qDebug() << profile->httpUserAgent();
}

MainWindow::~MainWindow()
{
    delete view;
    delete toolbar;

}

void MainWindow::onUrlChanged(const QUrl &url) {
    qDebug() << "url changed" << url;
    urlEdit->setText(url.toString());
}
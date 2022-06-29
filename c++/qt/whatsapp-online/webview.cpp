
#include "webview.hpp"
#include "webpage.hpp"

#include <QString>
#include <QEvent>
#include <QMessageBox>
#include <QWebEnginePage>

WebView::WebView(QWidget *parent) :
    QWebEngineView(parent)
{
    this->setZoomFactor(1.1);
    connect(this->page(), &QWebEnginePage::registerProtocolHandlerRequested,
            this, &WebView::handleRegisterProtocolHandlerRequested);

    this->setPage(new WebPage(this));
}

// bool WebView::event(QEvent *ev) {
//     return QWebEngineView::event(ev);
// }

void WebView::contextMenuEvent(QContextMenuEvent *event) {
    qDebug() << "context" << event;
    return QWebEngineView::contextMenuEvent(event);
}

void WebView::handleRegisterProtocolHandlerRequested(QWebEngineRegisterProtocolHandlerRequest request)
{
    auto answer = QMessageBox::question(
        this,
        tr("Permission Request"),
        tr("Allow %1 to open all %2 links?")
        .arg(request.origin().host())
        .arg(request.scheme()));
    if (answer == QMessageBox::Yes)
        request.accept();
    else
        request.reject();
}
#ifndef _WEBVIEW_H
#define _WEBVIEW_H

#include <QWebEngineView>
#include <QWebEngineRegisterProtocolHandlerRequest>

class WebView : public QWebEngineView
{
    Q_OBJECT

public:
    WebView(QWidget* parent = nullptr);
    virtual ~WebView() {};
protected:
    // bool event(QEvent *ev) override;
    void contextMenuEvent(QContextMenuEvent *event) override;
private slots:
    void handleRegisterProtocolHandlerRequested(QWebEngineRegisterProtocolHandlerRequest request);
};

#endif // _WEBVIEW_H
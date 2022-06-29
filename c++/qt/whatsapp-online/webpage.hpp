#ifndef _WEBPAGE_H
#define _WEBPAGE_H

#include <QWebEnginePage>

class WebPage : public QWebEnginePage
{
    Q_OBJECT

public:
    WebPage(QWidget *parent = nullptr) : QWebEnginePage(parent) {};
    ~WebPage() {};
protected:
    bool acceptNavigationRequest(const QUrl &url, QWebEnginePage::NavigationType type, bool isMainFrame) override;
};


#endif // _WEBPAGE_H
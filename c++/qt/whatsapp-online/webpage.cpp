
#include "webpage.hpp"

bool WebPage::acceptNavigationRequest(const QUrl &url, QWebEnginePage::NavigationType type, bool isMainFrame) {
    qDebug() << type << url;
    if (type == QWebEnginePage::NavigationTypeLinkClicked) {
        return true;
    }
    return QWebEnginePage::acceptNavigationRequest(url, type, isMainFrame);
}
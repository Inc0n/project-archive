
#include "scribble-area.h"

ScribbleArea::ScribbleArea(QWidget *parent) :
    QWidget(parent),
    points(QVector<QPoint>()),
    items(QVector<QGraphicsItem*>()),
    scene(QGraphicsScene(0, 0, 400, 400)),
    graphView(QGraphicsView()),
    mode(raw_line)
{
    this->graphView.setScene(&this->scene);
    // this->graphView.installEventFilter(this);
    //
    QVBoxLayout *layout = new QVBoxLayout;
    layout->addWidget(graphView);
    setLayout(layout);
    //
    // setAttribute(Qt::WA_StaticContents);
}

bool ScribbleArea::openImage(const QString &fileName)
{
    QImage loadedImage;
    if (!loadedImage.load(fileName))
        return false;

    QSize newSize = loadedImage.size().expandedTo(size());
    resizeImage(&loadedImage, newSize);
    image = loadedImage;
    modified = false;
    update();
    return true;
}

bool ScribbleArea::saveImage(const QString &fileName, const char *fileFormat)
{
    QImage visibleImage = image;
    resizeImage(&visibleImage, size());

    if (visibleImage.save(fileName, fileFormat)) {
        modified = false;
        return true;
    }
    return false;
}

bool ScribbleArea::eventFilter(QObject *obj, QEvent *event)
{
    if (obj == this->graphView
        && event->type() == QEvent::MouseButtonPress)
    {
        QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
        switch (event->type()) {
        case QEvent::MouseButtonPress:
            this->onMousePress(mouseEvent);
            break;
        case QEvent::MouseMove:
            this->onMouseMove(mouseEvent);
            break;
        case QEvent::MouseButtonRelease:
            this->onMouseRelease(mouseEvent);
            break;
        default:
            break;
        }
        return true;
    } else {
        // standard event processing
        return QObject::eventFilter(obj, event);
    }
}

void ScribbleArea::setPenColor(const QColor &newColor)
{
    myPenColor = newColor;
}

void ScribbleArea::setPenWidth(int newWidth)
{
    myPenWidth = newWidth;
}

void ScribbleArea::clearImage()
{
    image.fill(qRgb(255, 255, 255));
    modified = true;
    update();
}

void ScribbleArea::mousePressEvent(QMouseEvent *event) {
    // onMousePress(event);
}

void ScribbleArea::onMousePress(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton) {
        lastPoint = this->graphView.mapToScene(event->pos());
        switch(mode) {
        case raw_line: {
            scribbling = true;
            return ;
        }
        case straight_line: {
            QGraphicsLineItem *item = new QGraphicsLineItem();
            item->setPen(QPen(myPenColor, myPenWidth, Qt::SolidLine, Qt::RoundCap,
                                  Qt::RoundJoin));
            lastItem = item;
            break;
        }
        case circle: {
            QGraphicsEllipseItem *item = new QGraphicsEllipseItem();
            item->setPen(QPen(myPenColor, myPenWidth, Qt::SolidLine, Qt::RoundCap,
                                  Qt::RoundJoin));
            lastItem = item;
            break;
        }
        case rectangle: {
            QGraphicsRectItem *item = new QGraphicsRectItem();
            item->setPen(QPen(myPenColor, myPenWidth, Qt::SolidLine, Qt::RoundCap,
                                  Qt::RoundJoin));
            lastItem = item;
            break;
        }
        default:
            // TODO - error
            break;
        }
        this->scene.addItem(lastItem);
        this->items.append(lastItem);
        scribbling = true;
    }
}

void ScribbleArea::mouseMoveEvent(QMouseEvent *event) {}

void ScribbleArea::onMouseMove(QMouseEvent *event)
{
    if ((event->buttons() & Qt::LeftButton)
        && scribbling)
    {
        QPointF endPoint = this->graphView.mapToScene(event->pos());
        //
        QPointF p1 = lastPoint;
        QPointF p2 = endPoint;
        QRectF rect = QRectF(p1, p2);
        //
        switch(mode) {
        case raw_line: {
            lastItem = this->scene.addLine(QLineF(p1, p2),
                                           QPen(myPenColor, myPenWidth, Qt::SolidLine, Qt::RoundCap,
                                                                Qt::RoundJoin));
            this->items.append(lastItem);
            //
            lastPoint = endPoint;
            break;
        }
        case straight_line: {
            QGraphicsLineItem *lineItem =
                qgraphicsitem_cast<QGraphicsLineItem*>(lastItem);
            lineItem->setLine(QLineF(p1, p2));
            //
            break;
        }
        case circle: {
            QGraphicsEllipseItem *ellipseItem =
                qgraphicsitem_cast<QGraphicsEllipseItem*>(lastItem);
            ellipseItem->setRect(rect);
            break;
        }
        case rectangle: {
            QGraphicsRectItem *rectItem =
                qgraphicsitem_cast<QGraphicsRectItem*>(lastItem);
            rectItem->setRect(rect);
            break;
        }
        default:
            // TODO - error
            break;
        }
        // QList<QRectF> rects = QList<QRectF>();
        // rects.prepend(rect);
        this->graphView.update();
    }
}

void ScribbleArea::mouseReleaseEvent(QMouseEvent *event) {}

void ScribbleArea::onMouseRelease(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton
        && scribbling
        && lastItem != nullptr)
    {
        this->mouseMoveEvent(event);
        lastItem = nullptr;
        scribbling = false;
    }
}

void ScribbleArea::paintEvent(QPaintEvent *event)
{
    // QPainter painter(this);
    // QRect dirtyRect = event->rect();
    // painter.drawImage(dirtyRect, image, dirtyRect);
}

void ScribbleArea::resizeEvent(QResizeEvent *event)
{
    if (width() > image.width() || height() > image.height()) {
        int newWidth = qMax(width() + 128, image.width());
        int newHeight = qMax(height() + 128, image.height());
        resizeImage(&image, QSize(newWidth, newHeight));
        update();
    }
    QWidget::resizeEvent(event);
}

void ScribbleArea::resizeImage(QImage *image, const QSize &newSize)
{
    if (image->size() == newSize)
        return;

    QImage newImage(newSize, QImage::Format_RGB32);
    newImage.fill(qRgb(255, 255, 255));
    QPainter painter(&newImage);
    painter.drawImage(QPoint(0, 0), *image);
    *image = newImage;
}

void ScribbleArea::print()
{
#if defined(QT_PRINTSUPPORT_LIB) && QT_CONFIG(printdialog)
    QPrinter printer(QPrinter::HighResolution);

    QPrintDialog printDialog(&printer, this);
    if (printDialog.exec() == QDialog::Accepted) {
        QPainter painter(&printer);
        QRect rect = painter.viewport();
        QSize size = image.size();
        size.scale(rect.size(), Qt::KeepAspectRatio);
        painter.setViewport(rect.x(), rect.y(), size.width(), size.height());
        painter.setWindow(image.rect());
        painter.drawImage(0, 0, image);
    }
#endif // QT_CONFIG(printdialog)
}
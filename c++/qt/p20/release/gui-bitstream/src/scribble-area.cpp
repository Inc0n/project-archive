#include "scribble-area.h"

#include <QVBoxLayout>
#include <QHBoxLayout>

#include <QDir>
#include <QDebug>
#include <QFileInfo>
#include <QFileInfoList>

ScribbleArea::ScribbleArea(QWidget *parent) :
    QWidget(parent),
    mode(raw_line),
    drawCmd(DrawCmd(raw_line)),
    items(QVector<QGraphicsItem*>())
{
    graphicsView = new QGraphicsView;
    graphicsView->setMouseTracking(true);
    graphicsView->viewport()->installEventFilter(this);
    //
    this->setupToolbar();
    //
    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMenuBar(toolbar);
    // layout->addWidget(toolsView, 1);
    layout->addWidget(graphicsView);
    setLayout(layout);

    scene = new QGraphicsScene(this);
    graphicsView->setScene(scene);
}

#define MACRO(img_path, name, block_body)                           \
    icon = QIcon(img_path);                                         \
    if (!icon.isNull()) {                                           \
        toolbar->addAction(icon, name, this, [this]() block_body);  \
    }

void ScribbleArea::setupToolbar()
{
    toolbar = new QToolBar();
    QIcon icon;
    MACRO("assets/imgs/pen.png", "draw", {
            printf("set mode to draw");
            this->mode = raw_line;
        });
    MACRO("assets/imgs/line.png", "line", {
            printf("set mode to line");
            this->mode = straight_line;
        });
    MACRO("assets/imgs/circle.png", "circle", {
            printf("set mode to circle");
            this->mode = circle;
        });
    MACRO("assets/imgs/square.png", "rectangle", {
            printf("set mode to rectangle");
            this->mode = rectangle;
        });
    MACRO("assets/imgs/mouse.png", "mouse", {
            this->mode = ScribbleMode::cursor;
        });
    MACRO("assets/imgs/undo.png", "undo", {
            if (!this->items.isEmpty()) {
                this->removeDrawItem(this->items.takeLast());
                DrawCmd undoCmd(ScribbleMode::undo);
                emit newDiagramDrawn(undoCmd);
            }
        });
    MACRO("assets/imgs/clear.png", "clear", {
            // TODO - remove all items
            for (auto item: this->items) {
                this->removeDrawItem(this->items.takeLast());
                DrawCmd undoCmd(ScribbleMode::undo);
                emit newDiagramDrawn(undoCmd);
            }
        });
}


ScribbleArea::~ScribbleArea()
{
    delete scene;
    delete toolbar;
    delete graphicsView;
}

// setter

void ScribbleArea::setPenColor(const QColor &newColor) {
    myPenColor = newColor;
}

void ScribbleArea::setPenWidth(int newWidth) {
    myPenWidth = newWidth;
}

//

bool ScribbleArea::eventFilter(QObject *obj, QEvent *event)
{
    if (obj == this->graphicsView->viewport()
        && this->mode != ScribbleMode::cursor)
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
            return false;
        }
        return true;
    } else {
        // standard event processing
        return QObject::eventFilter(obj, event);
    }
}

#define MACRO_get_new_pin() \
    QPen(myPenColor, myPenWidth, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin)

void ScribbleArea::onMousePress(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton) {
        // initialize new draw command
        drawCmd = DrawCmd(mode);
        drawCmd.p1 = graphicsView->mapToScene(event->pos());
        //
        switch(mode) {
        case raw_line: {
            scribbling = true;
            return ; // return early
        }
        case straight_line: {
            QGraphicsLineItem *item = new QGraphicsLineItem();
            item->setPen(MACRO_get_new_pin());
            lastItem = item;
            break;
        }
        case circle: {
            QGraphicsEllipseItem *item = new QGraphicsEllipseItem();
            item->setPen(MACRO_get_new_pin());
            lastItem = item;
            break;
        }
        case rectangle: {
            QGraphicsRectItem *item = new QGraphicsRectItem();
            item->setPen(MACRO_get_new_pin());
            lastItem = item;
            break;
        }
        default:
            // TODO - error
            printf("warning %s: unexpected mode %d\n", __FUNCTION__, mode);
            break;
        }
        this->scene->addItem(lastItem);
        this->items.append(lastItem);
        scribbling = true;
    }
}

void ScribbleArea::onMouseMove(QMouseEvent *event)
{
    if ((event->buttons() & Qt::LeftButton)
        && scribbling)
    {
        QPointF endPoint = graphicsView->mapToScene(event->pos());
        //
        QPointF p1 = this->drawCmd.startPoint();
        QPointF p2 = endPoint;
        //
        switch(mode) {
        case raw_line: {
            this->drawCmd.p2 = endPoint;
            QPen pen = MACRO_get_new_pin();
            QGraphicsItem *item = scene->addLine(QLineF(p1, p2), pen);
            this->items.append(item);
            //
            emit newDiagramDrawn(this->drawCmd);
            this->drawCmd.p1 = endPoint;
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
            ellipseItem->setRect(QRectF(p1, p2));
            break;
        }
        case rectangle: {
            QGraphicsRectItem *rectItem =
                qgraphicsitem_cast<QGraphicsRectItem*>(lastItem);
            rectItem->setRect(QRectF(p1, p2));
            break;
        }
        default:
            // TODO - error
            printf("error %s: unexpected mode %d\n", __FUNCTION__, mode);
            break;
        }
    }
}

void ScribbleArea::onMouseRelease(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton
        && scribbling
        && lastItem != nullptr)
    {
        this->onMouseMove(event);
        //
        QPointF endPoint = graphicsView->mapToScene(event->pos());
        // raw_line behaves differently than other drawing commands
        if (mode != raw_line) {
            this->drawCmd.p2 = endPoint;
            emit newDiagramDrawn(this->drawCmd);
        }
        // cleanup
        lastItem = nullptr;
        scribbling = false;
    }
}

//

QGraphicsItem *ScribbleArea::applyNewDrawCmd(DrawCmd cmd)
{
    QPointF p1 = cmd.startPoint();
    QPointF p2 = cmd.endPoint();
    QGraphicsItem *item = nullptr;
    QPen pen = MACRO_get_new_pin();
    // Draw correspondingly to the draw command
    switch (cmd.getMode()) {
    case raw_line:
    case straight_line: {
        item = scene->addLine(QLineF(p1, p2), pen);
        break;
    }
    case circle: {
        item = scene->addEllipse(QRectF(p1, p2), pen);
        break;
    }
    case rectangle: {
        item = scene->addRect(QRectF(p1, p2), pen);
        break;
    }
    default:
        printf("error %s: unexpected mode %d\n", __FUNCTION__, cmd.getMode());
        return nullptr;
    }
    return item;
}

void ScribbleArea::removeDrawItem(QGraphicsItem *item)
{
    this->scene->removeItem(item);
}

//
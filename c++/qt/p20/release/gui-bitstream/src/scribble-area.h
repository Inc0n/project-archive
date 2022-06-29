// SCRIBBLEAREA.h

#ifndef SCRIBBLE_H
#define SCRIBBLE_H

#include <QWidget>
#include <QMouseEvent>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QGraphicsItem>
#include <QToolBar>

#include "DrawCmd.h"

enum ScribbleMode
{
    cursor = 1, // disable custom draw event
    //
    undo,
    //
    raw_line, // draw line from each mouse move
    // unimplemented
    smooth_line, // draw raw_line, but replace it with smooth line
    straight_line, // draw a line from start to end
    circle,
    rectangle,
};

class ScribbleArea : public QWidget
{
    Q_OBJECT

public:
    explicit ScribbleArea(QWidget *parent = 0);
    ~ScribbleArea();

    // QGraphicsItem *applyNewDrawCmd(DrawCmd cmd)
    // Decsription:
    // methods for apply new draw 'cmd' programmatically
    // Returns:
    // the graphics item that represent the drawing
    QGraphicsItem *applyNewDrawCmd(DrawCmd cmd);

    // void removeDrawItem(QGraphicsItem *item);
    // Decsription:
    // method for removing an item drawn on the scene
    void removeDrawItem(QGraphicsItem *item);

    // accessors
    QColor penColor() const { return myPenColor; }
    int penWidth() const { return myPenWidth; }
    // setters
    void setPenColor(const QColor &newColor);
    void setPenWidth(int newWidth);

signals:
    void newDiagramDrawn(DrawCmd cmd);

protected:
    bool eventFilter(QObject *obj, QEvent *event) override;

private:
    void onMousePress(QMouseEvent *event);
    void onMouseMove(QMouseEvent *event);
    void onMouseRelease(QMouseEvent *event);

    void setupToolbar();

    QToolBar *toolbar;

    QGraphicsView *graphicsView;
    QGraphicsScene *scene;

    ScribbleMode mode;
    DrawCmd drawCmd;
    QGraphicsItem *lastItem;
    // used to keep track of currently drawn items
    QVector<QGraphicsItem *> items;
    //
    bool scribbling = false;
    // drawing configuration
    int myPenWidth = 1;
    QColor myPenColor = Qt::blue;
};
#endif // SCRIBBLEAREA_H

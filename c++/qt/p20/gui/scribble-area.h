// SCRIBBLEAREA.h

#ifndef SCRIBBLE_H
#define SCRIBBLE_H

#include <QWidget>
#include <QMouseEvent>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QGraphicsItem>
#include <QListView>

#include <QStandardItemModel>
#include <QToolBar>

#include "DrawCmd.h"

enum ScribbleMode
{
    cursor = 1, // disable custom draw event
    undo,
    //
    raw_line, // draw line from each mouse move
    smooth_line, // draw raw_line, but replace it with smooth line
    straight_line, // draw a line from start to end
    circle,
    rectangle,
    //
};

class ScribbleArea : public QWidget
{
    Q_OBJECT

public:
    explicit ScribbleArea(QWidget *parent = 0);
    ~ScribbleArea();

    bool openImage(const QString &fileName) { return 0; };
    bool saveImage(const QString &fileName, const char *fileFormat) {return 0;};
    QGraphicsItem *applyNewDrawCmd(DrawCmd cmd);
    //
    QColor penColor() const { return myPenColor; }
    int penWidth() const { return myPenWidth; }
    //
    void setPenColor(const QColor &newColor);
    void setPenWidth(int newWidth);
    bool isModified() const { return modified; }


public slots:
    void clearImage() {};
    void print() {};
    void toolsClicked(const QModelIndex &idx);

signals:
    // emit newDiagramDrawn(this->points);
    void newDiagramDrawn(DrawCmd cmd);

protected:
    // void mousePressEvent(QMouseEvent *event) override;
    // void mouseMoveEvent(QMouseEvent *event) override;
    // void mouseReleaseEvent(QMouseEvent *event) override;
    bool eventFilter(QObject *obj, QEvent *event) override;

private:
    void onMousePress(QMouseEvent *event);
    void onMouseMove(QMouseEvent *event);
    void onMouseRelease(QMouseEvent *event);

    void setupToolbar();

    QToolBar *toolbar;
    QGraphicsView *graphicsView;

    QGraphicsScene *scene;
    QGraphicsEllipseItem *ellipse;
    QGraphicsRectItem *_rectangle;
    QGraphicsTextItem *text;

    ScribbleMode mode;
    DrawCmd drawCmd;
    QGraphicsItem *lastItem;
    QVector<QGraphicsItem *> items;

    bool modified = false;
    bool scribbling = false;

    int myPenWidth = 1;
    QColor myPenColor = Qt::blue;
};
#endif // SCRIBBLEAREA_H

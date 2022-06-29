
#include <QString>
#include <QColor>
#include <QPoint>
#include <QSize>
#include <QImage>

#include <QPaintEvent>
#include <QResizeEvent>
#include <QMouseEvent>

#include <QPen>
#include <QPainter>
#include <QWidget>

#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsItem>

enum ScribbleMode
{
    raw_line, // draw line from each mouse move
    smooth_line, // draw raw_line, but replace it with smooth line
    straight_line, // draw a line from start to end
    circle,
    rectangle,
};

class ScribbleArea : public QWidget
{
    Q_OBJECT

public:
    ScribbleArea(QWidget *parent = nullptr);

    bool openImage(const QString &fileName);
    bool saveImage(const QString &fileName, const char *fileFormat);
    void setPenColor(const QColor &newColor);
    void setPenWidth(int newWidth);

    bool isModified() const { return modified; }
    QColor penColor() const { return myPenColor; }
    int penWidth() const { return myPenWidth; }

public slots:
    void clearImage();
    void print();

signals:
    // emit morePointsDrawn(this->points);
    void morePointsDrawn(QPoint from, QPoint to);

protected:
    void mousePressEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void paintEvent(QPaintEvent *event) override;
    void resizeEvent(QResizeEvent *event) override;
    bool eventFilter(QObject *obj, QEvent *event) override;

private:
    void resizeImage(QImage *image, const QSize &newSize);
    void onMousePress(QMouseEvent *event);
    void onMouseMove(QMouseEvent *event);
    void onMouseRelease(QMouseEvent *event);

    QGraphicsScene scene;
    QGraphicsView graphView;
    //
    ScribbleMode mode;
    QVector<QPoint> points;
    QVector<QGraphicsItem *> items;
    QGraphicsItem *lastItem;
    bool modified = false;
    bool scribbling = false;
    int myPenWidth = 1;
    QColor myPenColor = Qt::blue;
    QImage image;
    QImage image_copy;
    QPointF lastPoint; // sometimes used as starting point
};
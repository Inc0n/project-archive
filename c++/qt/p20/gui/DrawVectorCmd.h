

struct DrawCollectionCmd {
    friend class ScribbleArea;
private:
    byte_t mode;
    QVector<QPointF> points;
    //
public:
    byte_t getMode() { return mode; }
    //
    DrawCollectionCmd() :
        points(QVector<QPointF>()) {
        mode = 0;
    }
    DrawCollectionCmd(QVector<QPointF> points) {
        mode = 0;
        this->points = points;
    }
    DrawCollectionCmd(byte_t *data) { // deserialize
        DESERIALIZE(this->mode, data);
        //
        byte_t numof_points = 0;
        DESERIALIZE(numof_points, data);
        //
        this->points = QVector<QPointF>(numof_points);
        for (byte_t i = 0; i < numof_points; ++i) {
            qreal p_x, p_y;
            DESERIALIZE(p_x, data);
            DESERIALIZE(p_y, data);
            this->points[i] = QPointF(p_x, p_y);
        }
    }
    QPointF startPoint() {
        // TODO - dereference empty vector
        return points.first();
    }
    QPointF endPoint() {
        // TODO - dereference empty vector
        return points.last();
    }
    byte_t* serialize(uint *size) {
        *size = sizeof(byte_t) * 2 + sizeof(QPointF) * points.size();

        start_serialize(ret, *size);
        SERIALIZE(ret, mode);
        byte_t len = points.size();
        SERIALIZE(ret, len);
        for (QPointF p: points) {
            qreal p_x = p.x();
            qreal p_y = p.y();
            SERIALIZE(ret, p_x);
            SERIALIZE(ret, p_y);
        }
        end_serialize(ret, *size);

        return ret;
    }
    void print() {
        printf("DrawVectorCmd { %d %d", mode, points.size());
        for (QPointF p: points) {
            qreal p_x = p.x();
            qreal p_y = p.y();
            printf(" {%f %f}", p_x, p_y);
        }
        printf("}\n");
    }
};
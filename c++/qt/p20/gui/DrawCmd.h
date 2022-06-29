
/* #define _memcpy(buf_name, data, len) do {       \ */
/*         memcpy(buf_name, data, len);            \ */
/*         buf_name += len;                        \ */
/*     } while(0) */

#ifndef DRAWCMD_H
#define DRAWCMD_H

typedef char byte_t;

#define start_serialize(buf_name, size) \
    byte_t *buf_name = (byte_t*)malloc(size);

#define SERIALIZE(buf_name, data) {             \
        memcpy(buf_name, &data, sizeof(data));   \
        buf_name += sizeof(data);               \
}
#define SERIALIZE_byte(buf_name, data) {             \
        memset(buf_name, data, 1);   \
        buf_name += 1;               \
}

#define end_serialize(buf_name, size) \
    buf_name -= size;

#define DESERIALIZE(data, buf_name) {      \
    memcpy(&data, buf_name, sizeof(data)); \
    buf_name += sizeof(data);              \
}

struct DrawCmd {
    friend class ScribbleArea;
private:
    byte_t mode;
    QPointF p1;
    QPointF p2;
    //
public:
    byte_t getMode() { return mode; }
    QPointF startPoint() { return p1; }
    QPointF endPoint() { return p2; }
    //
    DrawCmd(byte_t mode) {
        this->mode = mode;
    }
    DrawCmd(QPointF p1, QPointF p2) {
        mode = 0;
        this->p1 = p1;
        this->p2 = p2;
    }
    DrawCmd(byte_t *data) { // deserialize
        qreal p1_x, p1_y, p2_x, p2_y;
        DESERIALIZE(this->mode, data);
        DESERIALIZE(p1_x, data);
        DESERIALIZE(p1_y, data);
        DESERIALIZE(p2_x, data);
        DESERIALIZE(p2_y, data);
        // there should be one more zero here for '\0'
        this->p1 = QPointF(p1_x, p1_y);
        this->p2 = QPointF(p2_x, p2_y);
    }
    //
    byte_t* serialize(uint *size) {
        *size = sizeof(byte_t) + sizeof(qreal) * 4 + sizeof(uint);

        qreal p1_x = p1.x();
        qreal p1_y = p1.y();
        qreal p2_x = p2.x();
        qreal p2_y = p2.y();
        uint zero = 0;

        start_serialize(ret, *size);
        SERIALIZE(ret, mode);
        SERIALIZE(ret, p1_x);
        SERIALIZE(ret, p1_y);
        SERIALIZE(ret, p2_x);
        SERIALIZE(ret, p2_y);
        SERIALIZE(ret, zero);
        end_serialize(ret, *size);

        return ret;
    }
    void print() {
        printf("DrawCmd { %d %f %f %f %f }\n",
               mode, p1.x(), p1.y(), p2.x(), p2.y());
    }
};

#endif // DRAWCMD_H
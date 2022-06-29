
#include <QAbstractListModel>
#include <QImage>

struct Model {
    friend class ListModel;
    QImage icon;
    uint8_t mode;
public:
    Model(QImage icon, uint8_t mode) :
        icon(icon), mode(mode) {}
};

class ListModel: public QAbstractListModel
{
public:
    ListModel(QObject *parent = nullptr);
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;
    //
    void addToolbarItem(QImage icon, uint8_t mode);
private:
    QVector<Model> models;
};
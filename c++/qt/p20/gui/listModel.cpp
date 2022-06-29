#include "listModel.h"

ListModel::ListModel(QObject *parent) :
    QAbstractListModel(parent) {
    models = QVector<Model>();
}

int ListModel::rowCount(const QModelIndex &parent) const {
    return this->models.size();
}

// void ListModel::addToolbarItem(QString iconPath, uint8_t mode) {
//     QPixmap pixmap(iconPath);
//     if ( !pixmap.isNull())
//     this->models.append(Model(icon, mode));
// }
void ListModel::addToolbarItem(QImage icon, uint8_t mode) {
    this->models.append(Model(icon, mode));
}

QVariant ListModel::data(const QModelIndex &index, int role) const {
    QImage img = this->models[index.row()].icon;
    QVariant var = img;
    return var;
}

QVariant ListModel::headerData(int section, Qt::Orientation orientation, int role) const {
    return QVariant("tools");
}
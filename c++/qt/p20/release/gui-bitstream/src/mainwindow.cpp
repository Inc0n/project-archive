
#include <QMessageBox>
#include <QColorDialog>
#include <QInputDialog>
#include <QApplication>
#include <QToolBar>

#include "mainwindow.h"

MainWindow::MainWindow(QWidget *parent, BitSocket *sock) :
    QMainWindow(parent),
    scribbleArea(new ScribbleArea(this)),
    items(QVector<QGraphicsItem *>()),
    bitsock(sock)
{
    //
    setCentralWidget(scribbleArea);
    connect(scribbleArea, &ScribbleArea::newDiagramDrawn,
            this, &MainWindow::onNewDiagramDrawn);

    qRegisterMetaType<QVector<char>>("Bytes_t");
    connect(bitsock, &BitSocket::dataReceived,
            this, &MainWindow::onDataReceived);
    // std::thread threadObj();

    createActions();
    createMenus();

    setWindowTitle(tr("Scribble"));
    resize(500, 500);
}

MainWindow::~MainWindow()
{
    delete bitsock;
    delete scribbleArea;
}

void MainWindow::onNewDiagramDrawn(DrawCmd cmd) {
    uint size = 0;
    byte_t* data = cmd.serialize(&size);
    bitsock->queueSendData(data, size);
    //
    free(data);
}

void MainWindow::onDataReceived(QVector<char> data)
{
    printf("Received: ");
    printf("{");
    for (char c: data) {
        printf("%#x ", c);
    }
    printf("}\n");
    //
    DrawCmd cmd(data.data());
    cmd.print();

    switch (cmd.getMode()) {
        // taking care of special case
        case undo:
            scribbleArea->removeDrawItem(this->items.takeLast());
            break;
        default:
            // keep track of sent items for undo
            QGraphicsItem *item = this->scribbleArea->applyNewDrawCmd(cmd);
            items.push_back(item);
            break;
    }
}

//
// void MainWindow::setPenColorUI()
// Description:
// setting color via color picker
//
void MainWindow::setPenColorUI()
{
    QColor newColor = QColorDialog::getColor(scribbleArea->penColor());
    if (newColor.isValid())
        scribbleArea->setPenColor(newColor);
}

//
// void MainWindow::setPenColorUI()
// Description:
// setting color via ui
//
void MainWindow::setPenWidthUI()
{
    bool ok;
    int newWidth = QInputDialog::getInt(this, tr("Scribble"),
                                        tr("Select pen width:"),
                                        scribbleArea->penWidth(),
                                        1, 50, 1, &ok);
    if (ok)
        scribbleArea->setPenWidth(newWidth);
}

void MainWindow::about()
{
    QMessageBox::about(this, tr("About Scribble"),
            tr("<p>The <b>Scribble</b> is a program written for the p20 coursework"
               "in this program we would explore how to use the qt framework"
               "to implement a whiteboard chat, with receive and send threadings"
               "and finally implement all that with our own bit-stream class</p><p> We reimplement the mouse event "));
}

void MainWindow::createActions()
{
    penColorAct = new QAction(tr("&Pen Color..."), this);
    connect(penColorAct, &QAction::triggered, this, &MainWindow::setPenColorUI);

    penWidthAct = new QAction(tr("Pen &Width..."), this);
    connect(penWidthAct, &QAction::triggered, this, &MainWindow::setPenWidthUI);

    aboutAct = new QAction(tr("&About"), this);
    connect(aboutAct, &QAction::triggered, this, &MainWindow::about);

    aboutQtAct = new QAction(tr("About &Qt"), this);
    connect(aboutQtAct, &QAction::triggered, qApp, &QApplication::aboutQt);
}

void MainWindow::createMenus()
{
    optionMenu = new QMenu(tr("&Options"), this);
    optionMenu->addAction(penColorAct);
    optionMenu->addAction(penWidthAct);

    helpMenu = new QMenu(tr("&Help"), this);
    helpMenu->addAction(aboutAct);
    helpMenu->addAction(aboutQtAct);

    menuBar()->addMenu(optionMenu);
    menuBar()->addMenu(helpMenu);
}

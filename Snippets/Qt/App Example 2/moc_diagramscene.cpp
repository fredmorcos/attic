/****************************************************************************
** Meta object code from reading C++ file 'diagramscene.h'
**
** Created: Sun Jul 26 20:08:31 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "diagramscene.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'diagramscene.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_DiagramScene[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      19,   14,   13,   13, 0x05,
      46,   14,   13,   13, 0x05,
      79,   14,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
     113,  108,   13,   13, 0x0a,
     132,  127,   13,   13, 0x0a,
     170,   14,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_DiagramScene[] = {
    "DiagramScene\0\0item\0itemInserted(DiagramItem*)\0"
    "textInserted(QGraphicsTextItem*)\0"
    "itemSelected(QGraphicsItem*)\0mode\0"
    "setMode(Mode)\0type\0"
    "setItemType(DiagramItem::DiagramType)\0"
    "editorLostFocus(DiagramTextItem*)\0"
};

const QMetaObject DiagramScene::staticMetaObject = {
    { &QGraphicsScene::staticMetaObject, qt_meta_stringdata_DiagramScene,
      qt_meta_data_DiagramScene, 0 }
};

const QMetaObject *DiagramScene::metaObject() const
{
    return &staticMetaObject;
}

void *DiagramScene::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_DiagramScene))
        return static_cast<void*>(const_cast< DiagramScene*>(this));
    return QGraphicsScene::qt_metacast(_clname);
}

int DiagramScene::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QGraphicsScene::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: itemInserted((*reinterpret_cast< DiagramItem*(*)>(_a[1]))); break;
        case 1: textInserted((*reinterpret_cast< QGraphicsTextItem*(*)>(_a[1]))); break;
        case 2: itemSelected((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        case 3: setMode((*reinterpret_cast< Mode(*)>(_a[1]))); break;
        case 4: setItemType((*reinterpret_cast< DiagramItem::DiagramType(*)>(_a[1]))); break;
        case 5: editorLostFocus((*reinterpret_cast< DiagramTextItem*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void DiagramScene::itemInserted(DiagramItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void DiagramScene::textInserted(QGraphicsTextItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void DiagramScene::itemSelected(QGraphicsItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_END_MOC_NAMESPACE

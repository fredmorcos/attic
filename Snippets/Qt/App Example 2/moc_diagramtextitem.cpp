/****************************************************************************
** Meta object code from reading C++ file 'diagramtextitem.h'
**
** Created: Sun Jul 26 20:08:31 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "diagramtextitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'diagramtextitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_DiagramTextItem[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      22,   17,   16,   16, 0x05,
      50,   17,   16,   16, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_DiagramTextItem[] = {
    "DiagramTextItem\0\0item\0lostFocus(DiagramTextItem*)\0"
    "selectedChange(QGraphicsItem*)\0"
};

const QMetaObject DiagramTextItem::staticMetaObject = {
    { &QGraphicsTextItem::staticMetaObject, qt_meta_stringdata_DiagramTextItem,
      qt_meta_data_DiagramTextItem, 0 }
};

const QMetaObject *DiagramTextItem::metaObject() const
{
    return &staticMetaObject;
}

void *DiagramTextItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_DiagramTextItem))
        return static_cast<void*>(const_cast< DiagramTextItem*>(this));
    return QGraphicsTextItem::qt_metacast(_clname);
}

int DiagramTextItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QGraphicsTextItem::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: lostFocus((*reinterpret_cast< DiagramTextItem*(*)>(_a[1]))); break;
        case 1: selectedChange((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void DiagramTextItem::lostFocus(DiagramTextItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void DiagramTextItem::selectedChange(QGraphicsItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE

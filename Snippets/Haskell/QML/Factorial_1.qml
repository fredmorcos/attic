import QtQuick 2.0

Column {
    height: 300;
    TextEdit {
        id: input; width: 300; height: 30; font.pixelSize: 30; focus: true;
    }
    Rectangle {
        color: "red"; width: childrenRect.width; height: childrenRect.height;
        Text {
            width: 300; height: 30; font.pixelSize: 30;
            text: "Calculate Factorial"; color: "white";
        }
        MouseArea {
            anchors.fill: parent;
            onClicked: output.text = factorial(input.text);
        }
    }
    Text {
        width: 300; wrapMode: Text.WrapAnywhere; font.pixelSize: 30;
        id: output;
    }
}

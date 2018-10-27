var socketio = require("socket.io");
var io;
var guestNum = 1;
var nickNames = {};
var namesUsed = [];
var currentRoom = {};

exports.listen = function(server) {
    io = socketio.listen(server, { "log level": 1 });
    io.sockets.on("connection", function(socket) {
        assignGuestName(socket, nickNames, namesUsed);
        joinRoom(socket, "Lobby");
        handleMessageBroadcasting(socket, nickNames);
        handleNameChangeAttempts(socket, nickNames, namesUsed);
        handleRoomJoining(socket);

        socket.on("rooms", function() {
            socket.emit("rooms", io.sockets.manager.rooms);
        });

        handleClientDisconnection(socket, nickNames, namesUsed);
    });
};

function handleClientDisconnection(socket, nickNames, namesUsed) {
    socket.on("disconnect", function() {
        var nameIndex = namesUsed.indexOf(nickNames[socket.id]);
        delete namesUsed[nameIndex];
        delete nickNames[socket.id];
    });
}

function handleRoomJoining(socket) {
    socket.on("join", function(room) {
        socket.leave(currentRoom[socket.id]);
        joinRoom(socket, room.newRoom);
    });
}

function handleMessageBroadcasting(socket, nickNames) {
    socket.on("message", function(message) {
        socket.broadcast.to(message.room).emit("message", {
            text: nickNames[socket.id] + ": " + message.text
        });
    });
}

function handleNameChangeAttempts(socket, nickNames, namesUsed) {
    socket.on("nameAttempt", function(name) {
        if (name.startsWith("Guest")) {
            socket.emit("nameResult", {
                success: false,
                message: "Name cannot begin with `Guest`"
            });
        } else {
            if (namesUsed.indexOf(name) == -1) {
                var previousName = nickNames[socket.id];
                var previousNameIndex = namesUsed.indexOf(previousName);

                namesUsed.push(name);
                nickNames[socket.id] = name;

                delete namesUsed[previousNameIndex];

                socket.emit("nameResult", {
                    success: true,
                    name: name
                });

                socket.broadcast.to(currentRoom[socket.id]).emit("message", {
                    text: previousName + " is now known as " + name
                });
            } else {
                socket.emit("nameResult", {
                    success: false,
                    message: "Name is already in use"
                });
            }
        }
    });
}

function newGuestName() {
    var res = "Guest" + guestNum;
    guestNum += 1;
    return res;
}

function assignGuestName(socket, nickNames, namesUsed) {
    var name = newGuestName();
    nickNames[socket.id] = name;
    socket.emit("nameResult", {
        success: true,
        name: name
    });
    namesUsed.push(name);
}

function joinRoom(socket, room) {
    socket.join(room);
    currentRoom[socket.id] = room;
    socket.emit("joinResult", {
        room: room
    });
    socket.broadcast.to(room).emit("message", {
        text: nickNames[socket.id] + " has joined " + room
    });

    var usersInRoom = io.sockets.clients(room);
    if (usersInRoom.length > 1) {
        var usersInRoomSummary = "Users currently in " + room + ": ";
        for (var index in usersInRoom) {
            var userSocketId = usersInRoom[index].id;
            if (userSocketId != socket.id) {
                if (index > 0) {
                    usersInRoomSummary += ", ";
                }
                usersInRoomSummary += nickNames[userSocketId];
            }
        }
        socket.emit("message", {
            text: usersInRoomSummary
        });
    }
}

require("net").createServer(function(socket) {
    console.log("somebody connected!");

    socket.on("data", function(data) {
        socket.write(data);
    });

    socket.on("end", function() {
        console.log("somebody disconnected!");
    });
}).listen(8888);

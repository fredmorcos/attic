var http  = require("http");
var fs    = require("fs");
var path  = require("path");
var mime  = require("mime");
var cache = {};

function send404(res) {
    res.writeHead(404, {
        "Content-Type": "text/plain"
    });
    res.write("Error 404: resource not found.");
    res.end();
}

function sendFile(res, filename, contents) {
    res.writeHead(200, {
        "Content-Type": mime.lookup(path.basename(filename))
    });
    res.end(contents);
}

function serveStatic(res, cache, abspath) {
    if(cache[abspath]) {
        sendFile(res, abspath, cache[abspath]);
    } else {
        fs.exists(abspath, function(exists) {
            if (exists) {
                fs.readFile(abspath, function(err, data) {
                    if (err) {
                        send404(res);
                    } else {
                        cache[abspath] = data;
                        serveStatic(res, cache, abspath);
                    }
                });
            } else {
                send404(res);
            }
        });
    }
}

var server = http.createServer(function(req, res) {
    var filename = false;

    if (req.url == '/') {
        filename = 'public/index.html';
    } else {
        filename = 'public' + req.url;
    }

    var abspath = './' + filepath;
    serveStatic(res, cache, abspath);
});

server.listen(3000, function() {
    console.log("Listening on 3000");
});

var chatServer = require("./lib/chat_server");
chatServer.listen(server);

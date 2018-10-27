/*jslint nomen: true */

"use strict";

var fs = require('fs');
var express = require('express');
var app = require('app').app;

// Route imports
var indexRoute = require('./routes/index');
var loginRoute = require('./routes/login');
var logoutRoute = require('./routes/logout');
var registerRoute = require('./routes/register');

// Models
var UserDB = require('./models/user').UserDB;

console.log('Starting in ' + __dirname);
console.log('Loading...');

console.log('  -> Parsing JSON settings');
var settings = JSON.parse(fs.readFileSync('./settings.json'));

console.log('  -> Parsing package settings');
var pkg = JSON.parse(fs.readFileSync('./package.json'));

app.info = settings;
app.info.name = pkg.name;
app.info.version = pkg.version;

console.log('Setting up DB');
app.userDB = new UserDB();

// TODO remove that!
var User = require('./models/user').User;
app.userDB.addUser(new User('test', 'test', 'test@test.com'));

app.configure(function () {
    app.set('views', __dirname + '/views');
    app.set('view engine', 'jade');
    app.use(express.favicon());	// TODO install favicon
    app.use(express.logger('dev'));

    app.use(express.cookieParser('keyboard cat')); // TODO we need a secret here
    app.use(express.session());

    app.use(express.bodyParser());
    app.use(express.methodOverride());
    app.use(app.router);
    app.use(express['static'](__dirname + '/public'));
});

app.configure('development', function () {
    app.use(express.errorHandler());
});

// Routes
app.get('/', indexRoute.get);
app.post('/login', loginRoute.post);
app.get('/logout', logoutRoute.get);
app.get('/register', registerRoute.get);
app.post('/register', registerRoute.post);

if (!app.info.port) {
    console.warn('app.info.port not defined, using 3000 instead');
    app.info.port = 3000;
}

if (!app.info.host) {
    console.warn('app.info.host not defined, using localhost instead');
    app.info.host = 'localhost';
}

if (module.parent) {
    console.warn('module.parent is defined, not going to start a server');
} else {
    app.listen(app.info.port, function () {
        console.log('Running at http://' + app.info.host + ':' + app.info.port);
    });
}

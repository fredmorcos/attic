var fs = require('fs');
var express = require('express');

// Route imports
var indexRoute = require('./routes/index');
var loginRoute = require('./routes/login');

var app = express();

console.log('Starting in ' + __dirname);

if (app.info) {
    console.error('app.info is already defined');
    process.exit(1);
} else {
    if (settings) {
	console.error('settings is already defined');
	process.exit(1);
    }

    if (pkg) {
	console.error('pkg is already defined');
	process.exit(1);
    }

    var settings = JSON.parse(fs.readFileSync('./settings.json'));
    var pkg = JSON.parse(fs.readFileSync('./package.json'));

    app.info = settings;
    app.info.name = pkg.name;
    app.info.version = pkg.version;
}

app.configure(function () {
    app.set('views', __dirname + '/views');
    app.set('view engine', 'jade');
    app.use(express.favicon());	// TODO install favicon
    app.use(express.logger('dev'));
    app.use(express.bodyParser());
    app.use(express.methodOverride());
    app.use(app.router);
    app.use(express.static(__dirname + '/public'));
});

app.configure('development', function(){
    app.use(express.errorHandler());
});

// Routes
app.get('/', indexRoute.getIndex);
app.post('/login', loginRoute.postLogin);

if (!app.info.port) {
    console.warn('app.info.port not defined, using 3000 instead');
    app.info.port = 3000;
}

if (!app.info.host) {
    console.warn('app.info.host not defined, using localhost instead');
    app.info.host = 'localhost';
}

app.listen(app.info.port, function () {
    console.log('Running at http://' + app.info.host + ':' + app.info.port);
});

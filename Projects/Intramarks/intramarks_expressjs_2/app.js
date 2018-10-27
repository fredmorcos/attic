// Express defaults
var express = require('express');
var path    = require('path');
var app     = module.exports = express();
var pkg     = require('./package');

// Data validation module
// https://github.com/ctavan/express-validator
var validator = require('express-validator');

// Mongo and DB module
var db         = require('./app/core/db');
var mongostore = require('connect-mongo')(express);

// Core modules
var routes = require('./app/core/routes');

// A reference of the main app's assets paths
var paths = {
    views     : path.join(__dirname, 'app/views'),
    resources : path.join(__dirname, 'app/resources')
};

// A reference to the current page id
var pageID;

// Common configuration
app.info         = require('./settings');
app.info.name    = pkg.name;
app.info.version = pkg.version;

// Session configuration
var sessionConf = {
    db: {
        db: app.info.dbname,
        host: app.info.dbhost,
        // port: xxxx
        // username: admin/root
        // password: xxxx
        collection: 'sessions'
    },
    // head -c 200K < /dev/urandom | sha256sum
    secret: '0bf9d0cfaab80af03f93957d8b2c9922c4bbb4990cd7c594bab189f35b134d9f'
};

var expressSessionConf = {
    secret: sessionConf.secret,
    maxAge: new Date(Date.now() + 3600000),
    store:  new mongostore(sessionConf.db)
};

db.init(app);

db.onError(function() {
    app.use(function(req, res) {
        pageID = 'error';
        res.status(500).render(pageID, {
            id: pageID,
            title: 'Error',
            error_id: '500',
            error_str: 'Lorem ipsum in mollit velit adipisicing qui sit minim in dolor ullamco laborum.'
        });
    });
});

db.onConnect(function() {
    routes.bind(app);
});

db.connect(app);

// Configures global defaults
app.set('views', paths.views);
app.set('view engine', 'jade');
app.use(express.favicon());
app.use(express.logger('dev'));
app.use(express.bodyParser());
app.use(validator);
app.use(express.methodOverride());
app.use(express.cookieParser(sessionConf.secret));
app.use(express.cookieSession());
app.use(express.session(expressSessionConf));
app.use(express.csrf());
app.use(express.errorHandler({dumpExceptions:false, showStack:false}));
app.use(app.router);
app.use('/resources', express['static'](paths.resources));
app.locals.pretty = false;
app.locals.app_name = app.info.name;
app.locals.app_version = app.info.version;

// Configures development defaults
if (app.get('env') === 'development') {
    app.use(express.errorHandler({dumpExceptions:true, showStack:true}));
    app.locals.pretty = true;
}

// Server initialization
app.listen(app.info.port, function() {
    console.log("App running at http://" + app.info.host + ":" + app.info.port);
});

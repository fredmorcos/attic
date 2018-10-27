/*global require process __dirname console*/

// Default modules
var express = require('express');
var routes = require('./routes');
var app = express();

// Default configuration
app.set('port', process.env.PORT || 3000);
app.set('views', __dirname + '/views');
app.set('view engine', 'jade');
app.use(express.favicon());
app.use(express.logger('dev'));
app.use(express.bodyParser());
app.use(express.methodOverride());
app.use(express.cookieParser('app-name'));
app.use(app.router);
app.use(express['static'](__dirname + '/../public'));
app.locals.pretty = false;

// Development configuration
if ('development' === app.get('env')) {
  app.use(express.errorHandler({
      dumpExceptions: true,
      showStack: true
  }));
  app.locals.pretty = true;
}

// Routes
app.get('/', routes.index);

// Initialization
app.listen(app.get('port'), function () {
  console.log("App running on port " + app.get('port'));
});

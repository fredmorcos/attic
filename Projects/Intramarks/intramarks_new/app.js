var loadStamp = new Date();
var line = new Array(75).join('-');

log([line,
  '- ' + loadStamp.toUTCString(),
  '- Loading app at ' + __dirname,
line].join('\n'));

log('Setting up paths..');
var appPath = __dirname + '/app/';
var routesPath = appPath + 'routes/';

log('Loading package file..');
var pkg = require('./package');

log('Initializing express..');
var express = require('express');
var app = express();

log('Requiring routes..');
var home          = require( routesPath + 'index'         );
var auth          = require( routesPath + 'auth'          );
var account       = require( routesPath + 'account'       );
var bookmarks     = require( routesPath + 'bookmarks'     );
var organizations = require( routesPath + 'organizations' );
var plans         = require( routesPath + 'plans'         );
var about         = require( routesPath + 'about'         );
var contact       = require( routesPath + 'contact'       );
var faq           = require( routesPath + 'faq'           );
var privacy       = require( routesPath + 'privacy'       );
var terms         = require( routesPath + 'terms'         );
var errors        = require( routesPath + 'errors'        );

if(typeof app.info !== 'undefined'){
  log('Error: app.info is already defined', 1, 1);
}

log('Setting up app..');
app.info = {
  name: pkg.name,
  title: pkg.title,
  version: pkg.version,
  env: pkg.environment
};

app[ app.info.env === 'development' ? 'enable' : 'disable' ]('dev');

app.info.host = pkg.config[app.info.env].host;
app.info.port = pkg.config[app.info.env].port;

app.set('views', appPath + 'views');
app.set('view engine', 'jade');
app.locals.app_title = app.info.title;
app.locals.pretty = app.enabled('dev') ? true : false;

app.use(express.favicon());

if(app.enabled('dev')) {
  log('Using express\'s dev logger..');
  app.use(express.logger('dev'));
}

app.use(express.bodyParser());
app.use(express.methodOverride());
app.use(app.router);
app.use('/assets', express['static']( appPath + 'assets' ));
app.use(express.errorHandler());

if(app.enabled('dev')){
  log('Using express\'s verbose error handler..');
  app.use(express.errorHandler({dumpExceptions:true,showStack:true}));
}

app.get('/', home.index);

app.get('/login', auth.login);
app.post('/login', auth.login);

app.post('/logout', auth.logout);

app.get('/sign-up', auth.signup);
app.post('/sign-up', auth.signup);

app.get('/account', account.index);
app.get('/account/profile', account.index);
app.get('/account/admin', account.index);
app.get('/account/overview', account.overview);

app.get('/account/profile/display-name', account.displayName);
app.post('/account/profile/display-name', account.displayName);

app.get('/account/profile/gravatar-email', account.gravatarEmail);
app.post('/account/profile/gravatar-email', account.gravatarEmail);

app.get('/account/admin/username', account.username);
app.post('/account/admin/username', account.username);

app.get('/account/admin/email', account.email);
app.post('/account/admin/email', account.email);

app.get('/account/admin/password', account.password);
app.post('/account/admin/password', account.password);

app.get('/account/terminate', account.terminate);
app.post('/account/terminate', account.terminate);

app.get('/account/billing', account.billing);
app.post('/account/billing', account.billing);

app.get('/account/payments', account.payments);
app.post('/account/payments', account.payments);

app.get('/account/organizations', account.organizations);
app.post('/account/organizations', account.organizations);

app.get('/account/export', account.dataExport);
app.post('/account/export', account.dataExport);

app.get('/bookmarks', bookmarks.index);

app.get('/organizations', organizations.index);
app.get('/organizations/:name', organizations.orgIndex);

app.get('/plans', plans.index);
app.get('/about', about.index);
app.get('/contact', contact.index);
app.get('/faq', faq.index);
app.get('/privacy', privacy.index);
app.get('/terms', terms.index);

app.get('/maintenance', errors.maintenance);

log('Starting up..');
app.listen(app.info.port, function() {
  log([line,
    '- ' +  app.info.name + ' v' + app.info.version,
    '- running at http://' + app.info.host + (app.info.port==80?'':':'+app.info.port) + '/',
    '- in ' + app.info.env + ' environment.',
  line].join('\n'));
});

/**
* Helpers
**/

function log(msg, die, err){
  console.log(msg);
  if(die){
    console.log('Exiting..');
    process.exit( err ? 1 : 0 );
  }
}

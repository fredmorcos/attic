/*
 * Routes
 */

var _          = require('underscore');
var l10n       = require('./l10n');
var navigation = require('./navigation');
var utils      = require('./utils');
var codes      = require('./codes');

var pageID;

exports.bind = function (app) {

    /**
     * Homepage.
     */
    app.get('/', function(req, res) {
        pageID = 'index';
        res.render(pageID, { id: pageID, title: 'Homepage', nav: navigation.select(pageID) });
    });

    /**
     * Login.
     */
    app.get('/login', function(req, res) {
        pageID = 'login';
        res.render(pageID, { id: pageID, title: 'Login', nav: navigation.select(pageID) });
    });

    app.post('/login', function(req, res) {
        
        function callback(msgs_arr) {
            pageID = 'login';
            res.render(pageID, {
                id: pageID,
                title: 'Login',
                nav: navigation.select(pageID),
                messages: msgs_arr
            });
        }

        req.assert('password', l10n.say('invalid_password_len')).len(5,100);
        req.assert('email', l10n.say('invalid_email_len')).len(6,64);
        req.assert('email', l10n.say('invalid_email')).isEmail();

        var errors = req.validationErrors();
        
        if (errors) {
            callback(utils.validatorErrors(errors));
            return;
        }

        var email = req.body.email;
        var password = utils.hash(req.body.password);

        app.UserModel.find_match(email, password, function(err, results){
            if (err || !results.length) {
                console.log(err);
                callback([utils.respMsg(codes.ERROR, l10n.say('login_failed'))]);
            } else {
                console.log(results);
                callback([utils.respMsg(codes.SUCCESS, l10n.say('login_success'))]);
            }
        });
    });

    /**
     * Logout.
     */
    app.get('/logout', function(req, res) {
        pageID = 'logout';
        res.render(pageID, { id: pageID, title: 'Logout', nav: navigation.select() });
    });

    /**
     * Sign-up.
     */
    app.post('/sign-up', function(req, res) {

        function callback(msgs_arr) {
            pageID = 'sign-up';
            res.render(pageID, {
                id       : pageID,
                title    : 'Sign-up',
                nav      : navigation.select(pageID),
                messages : msgs_arr
            });
        }

        var data = {
            username : req.body.username,
            email    : req.body.email,
            password : utils.hash(req.body.password)
        };

        req.assert('username', l10n.say('invalid_username_len')).len(4, 15);
        req.assert('password', l10n.say('invalid_password_len')).len(5,100);
        req.assert('email', l10n.say('invalid_email_len')).len(6,64);
        req.assert('email', l10n.say('invalid_email')).isEmail();

        var errors = req.validationErrors();
        
        if (errors) {
            callback( utils.validatorErrors(errors) );
            return;
        }

        app.UserModel.email_exists(data.email, function(err, em){
            if(err){
                callback( [utils.respMsg(codes.ERROR, err)] );
                return;
            }
            if(em.length){
                callback( [utils.respMsg(codes.ERROR, l10n.say('email_exists'))] );
                return;
            }
            app.UserModel.username_exists(data.username, function(err, un){
                if(err){
                    callback( [utils.respMsg(codes.ERROR, err)] );
                    return;
                }
                if(un.length){
                    callback( [utils.respMsg(codes.ERROR, l10n.say('username_exists'))] );
                    return;
                }
                var newUser = new app.UserModel(data);
                newUser.save(function (resp) {
                    if ( resp && 'err' in resp ){
                        callback( [utils.respMsg(codes.ERROR, resp.err)] );
                        return;
                    }
                    callback( [utils.respMsg(codes.SUCCESS, l10n.say('register_success'))] );
                });
            });
        });
    });

    app.get('/sign-up', function(req, res) {
        pageID = 'sign-up';
        res.render(pageID, { id: pageID, title: 'Sign-up', nav: navigation.select(pageID) });
    });

    /**
     * Account.
     */
    function accountIndex(req, res) {
        res.redirect('/account/overview', 301);
    }

    function extAccTempOpts(opts){
        return _.extend({
            id: 'account',
            title: 'Account',
            nav: navigation.select('account')
        }, opts);
    }

    var renderData;

    app.get('/account', accountIndex);
    app.get('/account/profile', accountIndex);
    app.get('/account/admin', accountIndex);

    app.get('/account/overview', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'overview': 'active' }
        });
        res.render('account/overview', renderData);
    });

    app.get('/account/profile/display-name', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'profile_display_name': 'active' },
            form_url: '/account/profile/display-name'
        });
        res.render('account/profile-display-name', renderData);
    });

    app.get('/account/profile/gravatar-email', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'profile_gravatar_email': 'active' },
            form_url: '/account/profile/gravatar-email'
        });
        res.render('account/profile-gravatar-email', renderData);
    });

    app.get('/account/admin/username', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'admin_username': 'active' },
            form_url: '/account/admin/username'
        });
        res.render('account/admin-username', renderData);
    });

    app.get('/account/admin/email', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'admin_email': 'active' },
            form_url: '/account/admin/email'
        });
        res.render('account/admin-email', renderData);
    });

    app.get('/account/admin/password', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'admin_password': 'active' },
            form_url: '/account/admin/password'
        });
        res.render('account/admin-password', renderData);
    });

    app.get('/account/terminate', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'terminate': 'active' },
            form_url: '/account/terminate'
        });
        res.render('account/terminate', renderData);
    });

    app.get('/account/billing', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'billing': 'active' },
            form_url: '/account/billing'
        });
        res.render('account/billing', renderData);
    });

    app.get('/account/payments', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'payments': 'active' },
            form_url: '/account/payments'
        });
        res.render('account/payments', renderData);
    });

    app.get('/account/organizations', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'organizations': 'active' },
            form_url: '/account/organizations'
        });
        res.render('account/organizations', renderData);
    });

    app.get('/account/export', function(req, res) {
        renderData = extAccTempOpts({
            acc_nav: { 'export': 'active' },
            form_url: '/account/export'
        });
        res.render('account/export', renderData);
    });

    /**
     * Bookmarks.
     */
    app.get('/bookmarks', function(req, res) {
        pageID = 'bookmarks';
        res.render(pageID, { id: pageID, title: 'Bookmarks', nav: navigation.select(pageID) });
    });

    /**
     * Organizations.
     */
    app.get('/organizations', function(req, res) {
        pageID = 'organizations';
        res.render(pageID, { id: pageID, title: 'Organizations', nav: navigation.select(pageID) });
    });

    /**
     * Organization page.
     */
    app.get('/organizations/:name', function(req, res) {
        pageID = 'organization-index';
        res.render(pageID, {
            id: pageID,
            title: req.params.name + ' oragnization',
            org_title: req.params.name,
            nav: navigation.select()
        });
    });

    /**
     * Plans.
     */
    app.get('/plans', function(req, res) {
        pageID = 'plans';
        res.render(pageID, { id: pageID, title: 'Plans', nav: navigation.select(pageID) });
    });

    /**
     * About.
     */
    app.get('/about', function(req, res) {
        pageID = 'about';
        res.render(pageID, { id: pageID, title: 'About', nav: navigation.select(pageID) });
    });

    /**
     * Contact.
     */
    app.get('/contact', function(req, res) {
        pageID = 'contact';
        res.render(pageID, { id: pageID, title: 'Contact', nav: navigation.select(pageID) });
    });

    /**
     * F.A.Q
     */
    app.get('/faq', function(req, res) {
        pageID = 'faq';
        res.render(pageID, { id: pageID, title: 'Frequently asked questions', nav: navigation.select(pageID) });
    });

    /**
     * Privacy.
     */
    app.get('/privacy', function(req, res) {
        pageID = 'privacy';
        res.render(pageID, { id: pageID, title: 'Privacy', nav: navigation.select() });
    });

    /**
     * Terms.
     */
    app.get('/terms', function(req, res) {
        pageID = 'terms';
        res.render(pageID, { id: pageID, title: 'Terms & Conditions', nav: navigation.select() });
    });

    /**
     * Development pages.
     */
    if(app.get('env') === 'development') {
        /**
         * Maintenance.
         */
        app.get('/maintenance', function(req, res) {
            pageID = 'maintenance';
            res.render(pageID, { id: pageID, title: 'Maintenance', nav: navigation.select() });
        });
    }

};

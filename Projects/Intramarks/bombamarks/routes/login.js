"use strict";

var app = require('app').app;
var user = require('../models/user');

exports.post = function (req, res) {
    var errors = false,
        invalidUsername = false,
        invalidPassword = false;

    try {
        app.userDB.checkCredentials(req.body.username,
                                    req.body.password);
        req.session.user = {
            username: req.body.username
        };
    } catch (err) {
        errors = true;

        if (err === user.UserDBInvalidUsername) {
            invalidUsername = true;
        } else if (err === user.UserDBInvalidPassword) {
            invalidPassword = true;
        } else {
            console.error(err);
        }
    } finally {
        if (errors) {
            res.render('index', {
                app: app,
                user: req.session.user,
                errors: {
                    invalidUsername: invalidUsername,
                    invalidPassword: invalidPassword
                }
            });
        } else {
            res.redirect('/');
        }
    }
};

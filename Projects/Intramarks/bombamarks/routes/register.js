"use strict";

var app = require('app').app;
var UserModule = require('../models/user');
var User = UserModule.User;
var UserDB = UserModule.UserDB;

exports.get = function (req, res) {
    try {
        res.render('register', {
            app: app,
            user: req.session.user
        });
    } catch (err) {
        console.error(err);
    }
};

exports.post = function (req, res) {
    var errors = false,
        usernameExists = false,
        emailExists = false;

    try {
        app.userDB.addUser(new User(req.body.username,
                                    req.body.password,
                                    req.body.email));
    } catch (err) {
        errors = true;

        if (err === UserDB.exceptions.UsernameExists) {
            usernameExists = true;
        } else if (err === UserDB.exceptions.EmailExists) {
            emailExists = true;
        } else {
            console.error(err);
        }
    } finally {
        if (errors) {
            res.render('register', {
                app: app,
                errors: {
                    usernameExists: usernameExists,
                    emailExists: emailExists
                }
            });
        } else {
            res.redirect('/');
        }
    }
};

"use strict";

var app = require('app').app;

exports.get = function (req, res) {
    try {
        res.render('index', {
            app: app,
            user: req.session.user
        });
    } catch (err) {
        console.error(err);
    }
};

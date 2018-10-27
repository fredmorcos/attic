"use strict";

var app = require('app').app;

exports.get = function (req, res) {
    try {
        req.session.destroy(function (err) {
            if (err) {
                console.error(err);
                throw err;
            }
        });
    } catch (err) {
    } finally {
        res.redirect('/');
    }
};

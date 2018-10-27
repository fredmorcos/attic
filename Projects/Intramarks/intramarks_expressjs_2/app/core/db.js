// Module for single-point of access to DB

var mongoose = require('mongoose');

var UserSchema = mongoose.Schema({
    username: { type: String, unique: true },
    fname: String,
    lname: String,
    password: String,
    bookmarks: Array,
    email: { type: String, unique: true },
    verified: {type: Boolean, 'default': false }
});

var BookmarkSchema = mongoose.Schema ({
    url: String,
    shortcut: String,
    tags: Array
});

var UserModel = mongoose.model('UserModel', UserSchema);
var BookmarkModel = mongoose.model('BookmarkModel', BookmarkSchema);

exports.init = function (app) {
};

exports.onConnect = function (fn) {
    mongoose.connection.once('open', fn);
};

exports.onError = function (fn) {
    mongoose.connection.on('error', fn);
};

exports.connect = function (app) {
    mongoose.connect(app.info.dbhost, app.info.dbname);
};

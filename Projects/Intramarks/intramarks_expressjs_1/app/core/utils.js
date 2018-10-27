/*
 * Random helper functions
 */

// Cryptography module
var crypto = require('crypto');

// hash function used for passwords
exports.hash = function (str) {
    return crypto.createHash('md5').update(str).digest("hex");
};

// response message function used to output a message on a page
exports.respMsg = function (t, m) {
    return { type: t, content: m };
};

// converts validation errors to reponse messages
exports.validatorErrors = function (errors) {
    var that = this,
        errors_arr = [];
    errors.forEach(function(err, i){
        errors_arr.push(that.respMsg("error", err.msg));
    });
    return errors_arr;
};

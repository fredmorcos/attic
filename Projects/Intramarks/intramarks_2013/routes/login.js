exports.postLogin = function (req, res) {
    console.info('User login ' + req.body.username + ' ' + req.body.password);
    res.redirect('/');
};

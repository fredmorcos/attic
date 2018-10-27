action('signup', function () {
    console.log('INFO: user signup');
    username = req.body.username;
    pass = req.body.pass;
    redirect('/');
});

action('signup_page', function () {
    console.log('INFO: user signup page');
    // TODO: remove this when we implement the layout
    layout(false);
    render();
});

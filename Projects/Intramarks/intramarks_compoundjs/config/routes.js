exports.routes = function (map) {
    // Generic routes. Add all your routes below this line
    // feel free to remove generic routes
    // map.all(':controller/:action');
    // map.all(':controller/:action/:id');
    map.root('home#index');
    map.get('signup', 'users#signup_page');
    map.post('signup', 'users#signup');
};

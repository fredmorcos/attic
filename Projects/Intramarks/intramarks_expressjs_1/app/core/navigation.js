/*
 * Main navigation.
 */

exports.select = function(current, model) {
    /*
        The following `var` statement should be probably stored somewhere else,
        i.e `var nav = require('../models/navigation')`
    */

    var nav = [
        { id: 'sign-up', name: 'Sign-up',    url: '/sign-up' },
        { id: 'login',   name: 'Login',      url: '/login'   },
        { id: 'plans',   name: 'Plans',      url: '/plans'   },
        { id: 'about',   name: 'About us',   url: '/about'   },
        { id: 'contact', name: 'Contact us', url: '/contact' },
        { id: 'faq',     name: 'FAQ',        url: '/faq'     }
    ];

    if (typeof current === 'undefined') {
        return nav;
    }

    nav.forEach(function (val, i) {
        if (current === nav[i].id) {
            // TODO change that to a boolean
            nav[i].active = 'active';
        }
    });

    return nav;
};

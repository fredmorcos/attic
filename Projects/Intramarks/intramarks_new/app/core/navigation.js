/*
 * Main navigation.
 */

exports.navData = function(){
  return [{
    id: 'sign-up',
    name: 'Sign-up',
    url: '/sign-up'
  }, {
    id: 'login',
    name: 'Login',
    url: '/login'
  }, {
    id: 'plans',
    name: 'Plans',
    url: '/plans'
  }, {
    id: 'about',
    name: 'About us',
    url: '/about'
  }, {
    id: 'contact',
    name: 'Contact us',
    url: '/contact'
  }, {
    id: 'faq',
    name: 'FAQ',
    url: '/faq'
  }];
};

exports.select = function (current) {
  var nav = this.navData();

  if(typeof current === 'undefined') {
    return nav;
  }

  nav.forEach(function (val, i) {
    if(current === nav[i].id) {
      nav[i].active = true;
    }
  });

  return nav;
};

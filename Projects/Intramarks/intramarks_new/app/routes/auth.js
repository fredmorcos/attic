var navigation = require('../core/navigation');

exports.login = function (req, res) {
  var pageID = 'login';
  switch(req.method){
    case 'POST':
      res.render(pageID, {
        id: pageID,
        title: 'Login',
        nav: navigation.select(pageID)
      });
      break;
    case 'GET':
      res.render(pageID, {
        id: pageID,
        title: 'Login',
        nav: navigation.select(pageID)
      });
      break;
    default:
      console.log('-- Unknown request [' + req.method + '] on url [' + req.url + ']');
      break;
  }
};

exports.logout = function (req, res) {
  var pageID = 'logout';
  switch(req.method){
    case 'POST':
      res.render(pageID, {
        id: pageID,
        title: 'Logout',
        nav: navigation.select(pageID)
      });
      break;
    case 'GET':
      res.render(pageID, {
        id: pageID,
        title: 'Logout',
        nav: navigation.select(pageID)
      });
      break;
    default:
      console.log('-- Unknown request [' + req.method + '] on url [' + req.url + ']');
      break;
  }
};

exports.signup = function (req, res) {
  var pageID = 'sign-up';
  switch(req.method){
    case 'POST':
      res.render(pageID, {
        id: pageID,
        title: 'Sign-up',
        nav: navigation.select(pageID)
      });
      break;
    case 'GET':
      res.render(pageID, {
        id: pageID,
        title: 'Sign-up',
        nav: navigation.select(pageID)
      });
      break;
    default:
      console.log('-- Unknown request [' + req.method + '] on url [' + req.url + ']');
      break;
  }
};

var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'about';
  res.render(pageID, {
    id: pageID,
    title: 'About',
    nav: navigation.select(pageID)
  });
};

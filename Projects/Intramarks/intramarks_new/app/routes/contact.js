var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'contact';
  res.render(pageID, {
    id: pageID,
    title: 'Contact',
    nav: navigation.select(pageID)
  });
};

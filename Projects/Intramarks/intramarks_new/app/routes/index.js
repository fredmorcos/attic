var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'index';
  res.render(pageID, {
    id: pageID,
    title: 'Homepage',
    nav: navigation.select(pageID)
  });
};

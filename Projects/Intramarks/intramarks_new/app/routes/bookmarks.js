var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'bookmarks';
  res.render(pageID, {
    id: pageID,
    title: 'Bookmarks',
    nav: navigation.select(pageID)
  });
};

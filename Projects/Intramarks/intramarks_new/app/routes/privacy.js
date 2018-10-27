var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'privacy';
  res.render(pageID, {
    id: pageID,
    title: 'Privacy',
    nav: navigation.select()
  });
};

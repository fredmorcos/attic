var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'faq';
  res.render(pageID, {
    id: pageID,
    title: 'Frequently asked questions',
    nav: navigation.select(pageID)
  });
};

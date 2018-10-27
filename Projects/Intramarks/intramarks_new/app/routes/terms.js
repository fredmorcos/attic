var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'terms';
  res.render(pageID, {
    id: pageID,
    title: 'Terms & Conditions',
    nav: navigation.select()
  });
};

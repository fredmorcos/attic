var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'plans';
  res.render(pageID, {
    id: pageID,
    title: 'Plans',
    nav: navigation.select(pageID)
  });
};

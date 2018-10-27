var navigation = require('../core/navigation');

exports.maintenance = function (req, res) {
  var pageID = 'maintenance';
  res.render(pageID, {
    id: pageID,
    title: 'Maintenance',
    nav: navigation.select()
  });
};

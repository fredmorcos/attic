var navigation = require('../core/navigation');

exports.index = function (req, res) {
  var pageID = 'organizations';
  res.render(pageID, {
    id: pageID,
    title: 'Organizations',
    nav: navigation.select(pageID)
  });
};

exports.orgIndex = function (req, res) {
  var pageID = 'organization-index';
  res.render(pageID, {
    id: pageID,
    title: req.params.name + ' oragnization',
    org_title: req.params.name,
    nav: navigation.select()
  });
};

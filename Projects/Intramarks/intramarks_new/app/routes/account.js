var navigation = require('../core/navigation');
var _ = require('underscore');

function extendDefaults(opts) {
  return _.extend({
    id: 'account',
    title: 'Account',
    nav: navigation.select('account')
  }, opts);
}

exports.index = function (req, res) {
  res.redirect('/account/overview', 301);
};

exports.overview = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'overview': true }
  });
  res.render('account/overview', renderData);
};

exports.displayName = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'displayName': true },
    form_url: '/account/profile/display-name'
  });
  res.render('account/profile-display-name', renderData);
};

exports.gravatarEmail = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'gravatarEmail': true },
    form_url: '/account/profile/gravatar-email'
  });
  res.render('account/profile-gravatar-email', renderData);
};

exports.username = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'username': true },
    form_url: '/account/admin/username'
  });
  res.render('account/admin-username', renderData);
};

exports.email = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'email': true },
    form_url: '/account/admin/email'
  });
  res.render('account/admin-email', renderData);
};

exports.password = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'password': true },
    form_url: '/account/admin/password'
  });
  res.render('account/admin-password', renderData);
};

exports.terminate = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'terminate': true },
    form_url: '/account/terminate'
  });
  res.render('account/terminate', renderData);
};

exports.billing = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'billing': true },
    form_url: '/account/billing'
  });
  res.render('account/billing', renderData);
};

exports.payments = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'payments': true },
    form_url: '/account/payments'
  });
  res.render('account/payments', renderData);
};

exports.organizations = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'organizations': true },
    form_url: '/account/organizations'
  });
  res.render('account/organizations', renderData);
};

exports.dataExport = function (req, res) {
  var renderData = extendDefaults({
    acc_nav: { 'export': true },
    form_url: '/account/export'
  });
  res.render('account/export', renderData);
};

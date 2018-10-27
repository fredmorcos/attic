/* ============================================================================
 * app-touch.js
 * ------------
 * @dependencies: jQuery, App
 * ============================================================================ */

(function($, App) {

  "use strict";

  $(function() {
    $.support.touch = 'ontouchstart' in document;
  });

}(window.jQuery, window.App));

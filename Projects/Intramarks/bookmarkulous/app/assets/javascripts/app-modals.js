/* =========================================================
 * bootstrap-modal.js v2.3.1
 * http://twitter.github.com/bootstrap/javascript.html#modals
 * ========================================================= */

(function ($, App, Avgrund) {

  "use strict";
  /* MODAL DATA-API
   * ============== */
  $(document).on('click.modal.data-api', '[data-toggle="modal"]', function (e) {
    e.preventDefault();
    Avgrund.show( "#default-popup" );
  });

}(window.jQuery, window.App, window.Avgrund));

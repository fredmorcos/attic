/* ============================================================================
 * app-kontext.js
 * -----------------
 * @dependencies: jQuery, App
 * ============================================================================ */

(function($, App, kontext) {

  "use strict";

  $(function() {

    // Create a new instance of kontext
    var k = kontext(document.querySelector('.kontext'));

    // API METHODS:
    // k.prev(); // Show prev layer
    // k.next(); // Show next layer
    // k.show( 3 ); // Show specific layer
    // k.getIndex(); // Index of current layer
    // k.getTotal(); // Total number of layers

    $('[data-kontext-to]').on('click', function(e){
      e.preventDefault();
      k.show( +$(this).attr('data-kontext-to') );
    });

    document.addEventListener('keyup', function(event) {
      if (event.keyCode === 37){ k.prev();}
      if (event.keyCode === 39){ k.next();}
    }, false);

    var touchX = 0;
    var touchConsumed = false;
    var lastX;

    document.addEventListener('touchstart', function(event) {
      touchConsumed = false;
      lastX = event.touches[0].clientX;
    }, false);

    document.addEventListener('touchmove', function(event) {
      event.preventDefault();
      if (!touchConsumed) {
        if (event.touches[0].clientX > lastX + 10) {
          k.prev();
          touchConsumed = true;
        } else if (event.touches[0].clientX < lastX - 10) {
          k.next();
          touchConsumed = true;
        }
      }
    }, false);

  });

}(window.jQuery, window.App, window.kontext));

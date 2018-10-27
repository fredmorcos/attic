/* ============================================================================
 * app.js
 * ------
 * @dependencies: none
 * ============================================================================ */

(function(window) {

  /* APP DEFINITION
   * ============== */

  var App = {
    settings: {
      debug: true
    }
  };

  /* EXPOSE APP
   * ========== */

  window.App = App;

}(window));

/* ============================================================================
 * app-transition.js
 * -----------------
 * @dependencies: jQuery, App
 * ============================================================================ */

(function($, App) {

  "use strict";

  $.support.transition = (function() {
    var transitionEnd = (function() {
      var el = document.createElement('bootstrap'),
        transEndEventNames = {
          'WebkitTransition': 'webkitTransitionEnd',
          'MozTransition': 'transitionend',
          'OTransition': 'oTransitionEnd otransitionend',
          'transition': 'transitionend'
        },
        name;
      for (name in transEndEventNames) {
        if (el.style[name] !== undefined) {
          return transEndEventNames[name];
        }
      }
    }());
    return transitionEnd && {
      end: transitionEnd
    };
  })();

}(window.jQuery, window.App));

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

/* ============================================================================
 * app-push-loader.js
 * ------------------
 * @dependencies: jQuery, App, App-transition
 * ============================================================================ */

(function($, App) {

  "use strict";

  var trigger = '.push[data-lazyload]';

  /* PUSH-LOADER DATA-API
  * ===================== */

  if(!$.support.transition){
    return false;
  }

  $(trigger).each(function(){
    // Don't add the loader if the image is already loaded
    if( $(this)[0].complete ){
      return false;
    }
    $('<span class="loader" />').append('<i class="icon-spinner icon-spin" />').css({
      width: $(this).find('figure').width(),
      height: $(this).find('figure').height()
    }).appendTo($(this));
  });

  $('figure img', trigger).on('load error', function(){
    $(this).parents('.push').find('.loader').one($.support.transition.end, function(e) {
      $(this).remove();
    }).addClass('shoo');
  });

}(window.jQuery, window.App));

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

/* ============================================================================
 * app-carousel.js
 * ---------------
 * @dependencies: jQuery, App, jQuery-Flexslider
 * ============================================================================ */

(function($, App) {

  "use strict";

  var trigger = '[data-carousel]';
  var slideTrigger = '[data-carousel-control]';

  var defaults = {
    animation: "slide",
    slideshow: false,
    prevText: "<span class='icon-angle-left'></span>",
    nextText: "<span class='icon-angle-right'></span>"
  };

  /* CAROUSEL DATA-API
  * =================== */

  $(function(){
    $(trigger).each(function(){
      var attr = $(this).attr('data-carousel-opts');
      var opts = typeof attr !== 'undefined' ? attr : defaults;
      if(typeof opts === 'string'){
        opts = $.extend({}, defaults, $.parseJSON(opts));
      }
      $(this).flexslider( opts );
    });

    $(slideTrigger).on('click', function(e){
      e.preventDefault();
      var targ = $(this).attr('data-carousel-control').split(':');
      //$('html, body').animate({scrollTop:$(targ[0]).offset().top+"px"}, 500);
      $(targ[0]).flexslider(+targ[1]);
    });
  });

}(window.jQuery, window.App));

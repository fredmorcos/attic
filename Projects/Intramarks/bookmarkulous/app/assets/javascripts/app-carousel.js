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

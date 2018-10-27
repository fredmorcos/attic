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

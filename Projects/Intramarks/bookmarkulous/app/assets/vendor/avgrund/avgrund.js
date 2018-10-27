/*!
 * avgrund 0.1
 * http://lab.hakim.se/avgrund
 * MIT licensed
 *
 * Copyright (C) 2012 Hakim El Hattab, http://hakim.se
 */
var Avgrund = (function() {

  var container = $('html')[0],
    popup = $('.avgrund-popup-animate')[0],
    cover = $('.avgrund-cover')[0],
    currentState = null;

  $(container).addClass('avgrund-ready');

  // Deactivate on ESC

  function onDocumentKeyUp(event) {
    if (event.keyCode === 27) {
      deactivate();
    }
  }

  // Deactivate on click outside

  function onDocumentClick(event) {
    if (event.target === cover) {
      deactivate();
    }
  }

  function activate(state) {
    document.addEventListener('keyup', onDocumentKeyUp, false);
    document.addEventListener('click', onDocumentClick, false);
    document.addEventListener('touchstart', onDocumentClick, false);

    $(popup).removeClass(currentState).addClass('no-transition').removeClass(state);

    setTimeout(function() {
      $(popup).removeClass('no-transition');
      $(container).addClass('avgrund-active');
    }, 0);

    currentState = state;
  }

  function deactivate() {
    document.removeEventListener('keyup', onDocumentKeyUp, false);
    document.removeEventListener('click', onDocumentClick, false);
    document.removeEventListener('touchstart', onDocumentClick, false);

    $(container).removeClass('avgrund-active');
    $(popup).removeClass('avgrund-popup-animate')
  }

  function disableBlur() {
    $('html').addClass('no-blur');
  }

  function show(selector) {
    popup = document.querySelector(selector);
    $(popup).addClass('avgrund-popup-animate');
    activate();
    return this;
  }

  function hide() {
    deactivate();
  }

  return {
    activate: activate,
    deactivate: deactivate,
    disableBlur: disableBlur,
    show: show,
    hide: hide
  }

})();

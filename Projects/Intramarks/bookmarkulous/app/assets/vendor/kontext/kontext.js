/*!
 * kontext
 * http://lab.hakim.se/kontext
 * MIT licensed
 *
 * Copyright (C) 2013 Hakim El Hattab, http://hakim.se
 */

window.kontext = function(container) {
  // All layers in this instance of kontext
  var layers = Array.prototype.slice.call(container.querySelectorAll('.layer'));
  // Flag if the browser is capable of handling our fancy transition
  var capable = 'WebkitPerspective' in document.body.style ||
    'MozPerspective' in document.body.style ||
    'msPerspective' in document.body.style ||
    'OPerspective' in document.body.style ||
    'perspective' in document.body.style;
  if (capable) {
    container.classList.add('capable');
  }
  // Create dimmer elements to fade out preceding slides
  layers.forEach(function(el, i) {
    if (!el.querySelector('.dimmer')) el.innerHTML += '<div class="dimmer"></div>';
  });

  /**
   * Transitions to and shows the target layer.
   *
   * @param target index of layer or layer DOM element
   */

  function show(target, direction) {
    // Make sure our listing of available layers is up to date
    layers = Array.prototype.slice.call(container.querySelectorAll('.layer'));
    // Flag to CSS that we're ready to animate transitions
    container.classList.add('animate');
    // Flag which direction
    direction = direction || (target > getIndex() ? 'right' : 'left');
    // Accept multiple types of targets
    if (typeof target === 'string') target = parseInt(target);
    if (typeof target !== 'number') target = getIndex(target);
    // Enforce index bounds
    target = Math.max(Math.min(target, layers.length), 0);
    // Only navigate if were able to locate the target
    if (layers[target] && !layers[target].classList.contains('on')) {
      layers.forEach(function(el, i) {
        el.classList.remove('left', 'right');
        el.classList.add(direction);
        if (el.classList.contains('on')) {
          el.classList.remove('on');
          el.classList.add('off');
        } else {
          el.classList.remove('off');
        }
      });
      layers[target].classList.add('on');
    }
  }

  /**
   * Shows the previous layer.
   */

  function prev() {
    var index = getIndex() - 1;
    show(index >= 0 ? index : layers.length + index, 'left');
  }

  /**
   * Shows the next layer.
   */

  function next() {
    show((getIndex() + 1) % layers.length, 'right');
  }

  /**
   * Retrieves the index of the current slide.
   *
   * @param of [optional] layer DOM element which index is
   * to be returned
   */

  function getIndex(of) {
    var index = 0;
    layers.forEach(function(layer, i) {
      if ((of && of == layer) || (!of && layer.classList.contains('on'))) {
        index = i;
        return;
      }
    });
    return index;
  }

  /**
   * Retrieves the total number of layers.
   */

  function getTotal() {
    return layers.length;
  }
  // API
  return {
    show: show,
    prev: prev,
    next: next,
    getIndex: getIndex,
    getTotal: getTotal
  };
};

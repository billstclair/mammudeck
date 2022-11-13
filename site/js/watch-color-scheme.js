//////////////////////////////////////////////////////////////////////
//
// watch-color-schame.js
// Custom HTML element to notify Elm when prefers-color-scheme changes
// Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

(function() {

  function dispatchEvent(that) {
    that.dispatchEvent(new CustomEvent('change'));
  }

  customElements.define('watch-color-scheme', class extends HTMLElement {
    constructor() {
      super();
        this._mediaQuery = window.matchMedia("(prefers-color-scheme:dark)");
    }

    // https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements#using_the_lifecycle_callbacks
    connectedCallback() {
      if (!this._listener) {
        var that = this;
        this._listener = function(e) {
          that.dispatchEvent(new CustomEvent('change'));
        }
        this._mediaQuery.addEventListener('change', this._listener);
        window.setTimeout(this._listener, 1);
      }
    }

    disconnectedCallback() {
      var listener = this._listener;
      if (listener) {
        this._listener = null;
        this._mediaQuery.removeEventListener('change', listener);
      }
    }

    // read-only 'prefersDarkColorScheme` property
    get prefersDarkColorScheme() {
      return this._mediaQuery.matches;
    }

  });

})();

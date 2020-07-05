//////////////////////////////////////////////////////////////////////
//
// render-notify.js
// Custom HTML element to notify Elm when rendering happens.
// Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

(function() {

customElements.define('render-notify', class extends HTMLElement {
  constructor() {
    super();
    this._triggerValue = null;
  }

  connectedCallback() {
    // Move along. Nothing to see here.
  }

  get triggerValue() {
    return this._triggerValue;
  }

  set triggerValue(value) {
    this._triggerValue = value;
    if (value) {
      var that = this;
      function dispatch() {
        that.dispatchEvent(new CustomEvent('render-notify'));
      }
      // Need to delay or Elm doesn't call view.
      window.setTimeout(dispatch, 1);
    }
  }

});

})();

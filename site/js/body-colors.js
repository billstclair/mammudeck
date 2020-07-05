//////////////////////////////////////////////////////////////////////
//
// body-colors.js
// Custom HTML element to set background-color and color in the <head> element.
// Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

(function() {

customElements.define('body-colors', class extends HTMLElement {
  constructor() {
    super();
    // { id: "node id", text: "foo", count: 2 }
    this._color = null;
    this._backgroundColor = null;
  }

  connectedCallback() {
    // Move along. Nothing to see here.
  }

  get color() {
    return this._color;
  }

  get backgroundColor () {
    return this._backgroundColor;
  }

  set color(value) {
    this._color = value;
    document.body.style.color = value;
  }

  set backgroundColor(value) {
    this._backgroundColor = value;
    document.body.style.backgroundColor = value;
  }

});

})();

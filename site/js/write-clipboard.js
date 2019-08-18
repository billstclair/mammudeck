//////////////////////////////////////////////////////////////////////
//
// write-clipboard.js
// Custom HTML element to write a string to the clipboard.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

(function() {

// From https://stackoverflow.com/a/49186677/1386989
const getScrollParent = (node) => {
  const regex = /(auto|scroll)/;
  const parents = (_node, ps) => {
    if (_node.parentNode === null) { return ps; }
    return parents(_node.parentNode, ps.concat([_node]));
  };

  const style = (_node, prop) => getComputedStyle(_node, null).getPropertyValue(prop);
  const overflow = _node => style(_node, 'overflow') + style(_node, 'overflow-y') + style(_node, 'overflow-x');
  const scroll = _node => regex.test(overflow(_node));

  /* eslint-disable consistent-return */
  const scrollParent = (_node) => {
    if (!(_node instanceof HTMLElement || _node instanceof SVGElement)) {
      return;
    }

    const ps = parents(_node.parentNode, []);

    for (let i = 0; i < ps.length; i += 1) {
      if (scroll(ps[i])) {
        return ps[i];
      }
    }

    return document.scrollingElement || document.documentElement;
  };

  return scrollParent(node);
  /* eslint-enable consistent-return */
}

customElements.define('write-clipboard', class extends HTMLElement {
  constructor() {
    super();
    // { id: "node id", text: "foo", count: 2 }
    this._write = {};
  }

  connectedCallback() {
    // Move along. Nothing to see here.
  }

  get write() {
    return this._write;
  }

  set write(value) {
    var write = this._write;
    if ('object' != typeof(write)) {
      write = {};
    }
    this._write = value;
    if ('object' == typeof(value) &&
        'function' == typeof(document.execCommand)) {
      if (value.count > 0 &&
          write.count != value.count) {
        var id = value.id
        var text = value.text;
        if ('string' == typeof(id) && 'string' == typeof(text)) {
          var node = null;
          if (id != "") {
            node = document.getElementById(id);
            if (!node) {
              return;
            }
          }
          var parent = this.parentNode;
          var scrollElement = null;
          var scrollLeft = null;
          var scrollTop = null;
          if (!node) {
            if (parent) {
              var p = getScrollParent(this);
              if ('object' == typeof(p)) {
                if ('number' == typeof(p.scrollLeft)) {
                  scrollElement = p;
                  scrollLeft = p.scrollLeft;
                  scrollTop = p.scrollTop;
                }
              }
              node = document.createElement('textarea')
              parent.appendChild(node);
            }
          }
          if (node) {
            if ('function' == typeof(node.focus) &&
                'function' == typeof(node.select)) {
              node.value = text;
              node.focus();
              node.select();
              try {
                document.execCommand('copy');
              } catch(e) {
                console.log(e);
              }
              if (id == "") {
                parent.removeChild(node);
              }
              if (scrollElement) {
                scrollElement.scrollLeft = scrollLeft;
                scrollElement.scrollTop = scrollTop;
              }
            }
          }
        }
      }
    }
  }
});

})();

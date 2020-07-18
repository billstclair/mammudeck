//////////////////////////////////////////////////////////////////////
//
// text-area-tracker.js
// Define the `text-area-tracker` custom element.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

(function() {
  
  customElements.define('text-area-tracker', class extends HTMLElement {
    constructor() {
      super();
      this._textAreaId = null;
      this._triggerCoordinates = null;
      this._triggerSelection = null;
      this._setSelection = null;
    }

    connectedCallback() {
      // Move along. Nothing to see here.
    }

    get textAreaId() {
      return this._textAreaId;
    }

    set textAreaId(value) {
      this._textAreaId = value;
    }

    get setSelection() {
      return this._setSelection;
    }

    set setSelection(value) {
      var oldValue = this._setSelection
      this._setSelection = value;
      if (value) {
        if (!oldValue ||
            oldValue.count != value.count ||
            oldValue.start != value.start ||
            oldValue.end != value.end ||
            oldValue.direction != value.direction
           ) {
          var element = this.textArea;
          if (element) {
            var that = this;
            function doit() {
              var start = value.start;
              var end = value.end;
              if (start == null) {
                start = end;
              } else if (end == null) {
                end = start;
              }
              element.setSelectionRange(start, end, value.direction);
            }
            // The user will often set the text at the same time as
            // the selection. Elm will sync in an unknown order,
            // so delay changing the selection.
            window.setTimeout(doit, 1);
          }
        }
      }
    }

    get triggerSelection() {
      return this._triggerSelection;
    }

    set triggerSelection(value) {
      // Don't trigger on first set.
      var doit = this._triggerSelection !== null;
      this._triggerSelection = value;
      if (doit) {
        var that = this;
        function dispatch() {
          that.dispatchEvent(new CustomEvent('selection'));
        }
        // Need to delay or Elm doesn't call view.
        window.setTimeout(dispatch, 1);
      }
    }

    get triggerCoordinates() {
      return this._triggerCoordinates;
    }

    set triggerCoordinates(value) {
      // Don't trigger on first set.
      var doit = this._triggerCoordinates !== null;
      this._triggerCoordinates = value;
      if (doit) {
        var that = this;
        function dispatch() {
          that.dispatchEvent(new CustomEvent('caretCoordinates'));
        }
        // Need to delay or Elm doesn't call view.
        window.setTimeout(dispatch, 1);        
      }
    }

    get textArea() {
      if (this._textAreaId) {
        var element = document.getElementById(this._textAreaId);
        if (element && (element.type == "textarea" || element.type == "text")) {
          return element;
        }
        console.log("getTextArea failed, _textAreaId:", this._textAreaId);
        return null;
      }
    }

    get selectionStart() {
      var element = this.textArea;
      if (element) {
        return element.selectionStart;
      }
      return null;
    }

    get selectionEnd() {
      var element = this.textArea;
      if (element) {
        return element.selectionEnd;
      }
      return null;
    }

    get selection() {
      return { id: this.textAreaId,
               selectionStart: this.selectionStart,
               selectionEnd: this.selectionEnd
             }
    }

    get elmProperties() {
      return { id: this.textAreaId,
               selectionStart: this.selectionStart,
               selectionEnd: this.selectionEnd,
               caretCoordinates: this.caretCoordinates
             }
    }
    
    // Adapted from https://github.com/component/textarea-caret-position
    // Copyright (c) 2015 Jonathan Ong me@jongleberry.com
    // Distributed under the MIT license.
    // See CREDITS

    get caretCoordinates() {
      if (!isBrowser) {
        console.log("Not a browser!");
        return null;
      }

      var element = this.textArea;
      if (!element) {
        return null;
      }
      var position = element.selectionEnd;
      
      var debug = false;

      // The mirror div will replicate the textarea's style
      var div = document.createElement('div');
      div.id = 'input-textarea-caret-position-mirror-div';
      document.body.appendChild(div);

      var style = div.style;
      var computed = window.getComputedStyle ? window.getComputedStyle(element) : element.currentStyle;  // currentStyle for IE < 9
      var isInput = element.nodeName === 'INPUT';

      // Default textarea styles
      style.whiteSpace = 'pre-wrap';
      if (!isInput)
        style.wordWrap = 'break-word';  // only for textarea-s

      // Position off-screen
      style.position = 'absolute';  // required to return coordinates properly
      if (!debug)
        style.visibility = 'hidden';  // not 'display: none' because we want rendering

      // Transfer the element's properties to the div
      properties.forEach(function (prop) {
        if (isInput && prop === 'lineHeight') {
          // Special case for <input>s because text is rendered centered and line height may be != height
          if (computed.boxSizing === "border-box") {
            var height = parseInt(computed.height);
            var outerHeight =
                parseInt(computed.paddingTop) +
                parseInt(computed.paddingBottom) +
                parseInt(computed.borderTopWidth) +
                parseInt(computed.borderBottomWidth);
            var targetHeight = outerHeight + parseInt(computed.lineHeight);
            if (height > targetHeight) {
              style.lineHeight = height - outerHeight + "px";
            } else if (height === targetHeight) {
              style.lineHeight = computed.lineHeight;
            } else {
              style.lineHeight = 0;
            }
          } else {
            style.lineHeight = computed.height;
          }
        } else {
          style[prop] = computed[prop];
        }
      });

      if (isFirefox) {
        // Firefox lies about the overflow property for textareas: https://bugzilla.mozilla.org/show_bug.cgi?id=984275
        if (element.scrollHeight > parseInt(computed.height))
          style.overflowY = 'scroll';
      } else {
        style.overflow = 'hidden';  // for Chrome to not render a scrollbar; IE keeps overflowY = 'scroll'
      }

      div.textContent = element.value.substring(0, position);
      // The second special handling for input type="text" vs textarea:
      // spaces need to be replaced with non-breaking spaces - http://stackoverflow.com/a/13402035/1269037
      if (isInput)
        div.textContent = div.textContent.replace(/\s/g, '\u00a0');

      var span = document.createElement('span');
      // Wrapping must be replicated *exactly*, including when a long word gets
      // onto the next line, with whitespace at the end of the line before (#7).
      // The  *only* reliable way to do that is to copy the *entire* rest of the
      // textarea's content into the <span> created at the caret position.
      // For inputs, just '.' would be enough, but no need to bother.
      span.textContent = element.value.substring(position) || '.';  // || because a completely empty faux span doesn't render at all
      div.appendChild(span);

      var coordinates = {
        top: span.offsetTop + parseInt(computed['borderTopWidth']),
        left: span.offsetLeft + parseInt(computed['borderLeftWidth']),
        height: parseInt(computed['lineHeight'])
      };

      if (debug) {
        span.style.backgroundColor = '#aaa';
      } else {
        document.body.removeChild(div);
      }

      return coordinates;
    }

  })

  var isBrowser = (typeof window !== 'undefined');
  var isFirefox = (isBrowser && window.mozInnerScreenX != null);

  // We'll copy the properties below into the mirror div.
  // Note that some browsers, such as Firefox, do not concatenate properties
  // into their shorthand (e.g. padding-top, padding-bottom etc. -> padding),
  // so we have to list every single property explicitly.
  var properties = [
    'direction',  // RTL support
    'boxSizing',
    'width',  // on Chrome and IE, exclude the scrollbar, so the mirror div wraps exactly as the textarea does
    'height',
    'overflowX',
    'overflowY',  // copy the scrollbar for IE

    'borderTopWidth',
    'borderRightWidth',
    'borderBottomWidth',
    'borderLeftWidth',
    'borderStyle',

    'paddingTop',
    'paddingRight',
    'paddingBottom',
    'paddingLeft',

    // https://developer.mozilla.org/en-US/docs/Web/CSS/font
    'fontStyle',
    'fontVariant',
    'fontWeight',
    'fontStretch',
    'fontSize',
    'fontSizeAdjust',
    'lineHeight',
    'fontFamily',

    'textAlign',
    'textTransform',
    'textIndent',
    'textDecoration',  // might not make a difference, but better be safe

    'letterSpacing',
    'wordSpacing',

    'tabSize',
    'MozTabSize'
  ];

})();

//////////////////////////////////////////////////////////////////////
//
// idleNotify.js
// Send idle time to an incoming port (Sub).
// Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

/*
  
  To use, create a `Sub` port in your Elm code:

      port idleNotify : (Int -> msg) -> Sub msg

  And load this file in your `index.html`'s `<head>` section, and call
  the single function it defines:

      <html>
        <head>
          ...
          <script type='text/javascript' src='js/idleNotify.js'></script>
          ...
          idleNotify(app.ports.idleNotify);
          ...
        </head>
      </html>

  For instructions on subscribing to the port, and processing its messages, see:

      https://guide.elm-lang.org/interop/ports.html

  Your port will be sent the idle time, in milliseconds, once a second,
  or immediately, if an event happens, and it's been idle for more than
  10 seconds. You can change the events that are considered non-idle
  by editing the value of the `events` array below.

*/
function idleNotify(port) {
    if (port && port.send) {
        var timeout;
        var lastEventTime = Date.now();
        function resetTimer() {
            var now = Date.now();
            var diff = now - lastEventTime;
            lastEventTime = now;
            if (diff > 10000) {
                notify();
            }
        }
        function notify() {
            clearTimeout(timeout);
            var diff = Date.now() - lastEventTime;
            port.send(diff);
            timeout = window.setTimeout(notify, 1000);
        }
        notify();
        var events = ['mousedown','mousemove','keypress','scroll','touchstart'];
        events.forEach(function(name) {
            document.addEventListener(name, resetTimer, true);
        });
    }
}

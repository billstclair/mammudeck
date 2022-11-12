//////////////////////////////////////////////////////////////////////
//
// worker.js
//
// A simple worker. Load this file in your top-level HTML.
// It will load itself as a worker, then, once a second, send
// a message from top-level to worker, which is sent back.
// It exists only to convince Chromium-based browsers to all saving
// the webapp as a separate application.
//
// Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

if (typeof window === 'undefined') {
    console.log('worker.js running as a worker.');
    self.addEventListener('fetch', (event) => {
    });
    onmessage = (e) => {
        self.postMessage(e.data);
    }
} else {
    var count = 0;
    if (window.Worker) {
        const worker = new Worker(document.currentScript.src);
        worker.onmessage = (e) => {
            //console.log("message: ", e.data);
            setTimeout(tick, 1000);
        };
        function tick() {
            count++;
            worker.postMessage(count);
        };
        setTimeout(tick, 1000);
    }
}

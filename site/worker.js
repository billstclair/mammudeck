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
    self.addEventListener('fetch', (event) => {});
} else {
    if ('serviceWorker' in navigator) {
        const url = document.currentScript.src;
        var registration = null;
        navigator.serviceWorker.register(url).then((reg) => {
            registration = reg;
        });
    } else {
        console.log('serviceWorker does not exist. Not started.');
    }
}

//////////////////////////////////////////////////////////////////////
//
// worker.js
//
// A simple service worker.
// Load this file in your top-level HTML.
// It must be in the same directory as index.html, or it
// won't get the right default scope.
// It exists only to convince Chromium-based browsers to allow
// saving the webapp as a separate application.
//
// Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

if (typeof window === 'undefined') {
    // We're running as a service worker.
    console.log('worker.js running as a service worker.');
    self.addEventListener('fetch', (event) => {});
} else {
    // We're running in normal browser mode.
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

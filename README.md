[Mammudeck](https://mammudeck.com/) is a Tweetdeck-like interface to the [Mastodon](https://mastodon.social/) social media platform. It is written in [Elm](https://elm-lang.org/), and uses the [billstclair/elm-mastodon](https://package.elm-lang.org/packages/billstclair/elm-mastodon/latest) package to handle the wire protocol.

This is a work in progress.

For development:

    cd ~/.../elm-mammudeck
    elm reactor
    
Then aim your browser at http://localhost:8000/site/index.html

After code changes, use `bin/build` to create `site/elm.js`, then reload the browser window.

To test `Mammudeck.EncodeDecode`:

    cd ~/.../elm-mammudeck
    elm-test

Bill St. Clair, 4 July 2019

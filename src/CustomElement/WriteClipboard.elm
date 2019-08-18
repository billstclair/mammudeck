----------------------------------------------------------------------
--
-- WriteClipboard.elm
-- Elm interface to <write-clipboard> custom element.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.WriteClipboard exposing
    ( writeClipboard
    , write
    )

{-| The Elm interface to the `write-clipboard` custom element.

This code won't do anything unless `site/js/write-clipboard.js` is loaded.


# Types

@docs Write


# Html Elements

@docs writeClipboard


# Attributes

@docs write

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Encode as JE exposing (Value)


{-| Create a `write-clipboard` HTML element
-}
writeClipboard : List (Attribute msg) -> List (Html msg) -> Html msg
writeClipboard =
    Html.node "write-clipboard"


{-| Text to write and a node to put it in for the copy command.

`id` is the id of a `textarea` or text `input` element, or another element
that supports `focus()` and `select()`.

`text` is the text to write into that element.

`count` is a counter that you should increment every time you want a write
to the clipboard to happen.

-}
type alias Write =
    { id : String
    , text : String
    , count : Int
    }


encodeWrite : Write -> Value
encodeWrite { id, text, count } =
    JE.object
        [ ( "id", JE.string id )
        , ( "text", JE.string text )
        , ( "count", JE.int count )
        ]


{-| This is how you set the id of the tracked image element.
-}
write : Write -> Attribute msg
write properties =
    property "write" <|
        encodeWrite properties

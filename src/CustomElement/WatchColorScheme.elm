----------------------------------------------------------------------
--
-- WatchColorScheme.elm
-- Elm interface to <watch-color-scheme> custom element.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.WatchColorScheme exposing
    ( watchColorScheme
    , ColorScheme(..)
    , onChange
    )

{-| The Elm interface to the `watch-color-scheme` custom element.

This code won't do anything unless `site/js/watch-color-scheme.js` is loaded.


# Html Elements

@docs watchColorScheme


# Events

@docs ColorScheme, onRender

-}

import Html exposing (Attribute, Html)
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Create a render-notify HTML element.
-}
watchColorScheme : List (Attribute msg) -> List (Html msg) -> Html msg
watchColorScheme =
    Html.node "watch-color-scheme"


{-| The value sent back via `onChange`
-}
type ColorScheme
    = LightColorScheme
    | DarkColorScheme


{-| Notification will happen after your `view` function sends a value.
-}
onChange : (ColorScheme -> msg) -> Attribute msg
onChange tagger =
    on "change" <|
        JD.map tagger
            (JD.at [ "target", "prefersDarkColorScheme" ] JD.bool
                |> JD.andThen
                    (\bool ->
                        JD.succeed <|
                            if bool then
                                DarkColorScheme

                            else
                                LightColorScheme
                    )
            )

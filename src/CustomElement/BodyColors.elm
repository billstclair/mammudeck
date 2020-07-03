----------------------------------------------------------------------
--
-- BodyColors.elm
-- Elm interface to <body-colors> custom element.
-- Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.BodyColors exposing
    ( bodyColors
    , color
    , backgroundColor
    )

{-| The Elm interface to the `body-colors` custom element.

This code won't do anything unless `site/js/body-colors.js` is loaded.


# Html Elements

@docs bodyColors


# Attributes

@docs color
@docs backgroundColor

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Json.Encode as JE exposing (Value)


{-| Create a `body-colors` HTML element
-}
bodyColors : List (Attribute msg) -> List (Html msg) -> Html msg
bodyColors =
    Html.node "body-colors"


{-| This is how you set the backgroundColor of your page's <body>.
-}
backgroundColor : String -> Attribute msg
backgroundColor theColor =
    property "backgroundColor" <|
        JE.string theColor


{-| This is how you set the color of your page's <body>.
-}
color : String -> Attribute msg
color theColor =
    property "color" <|
        JE.string theColor

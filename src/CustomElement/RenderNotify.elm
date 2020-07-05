----------------------------------------------------------------------
--
-- RenderNotify.elm
-- Elm interface to <text-area-tracker> custom element.
-- Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.RenderNotify exposing
    ( renderNotify
    , notifyValue
    , onRender
    )

{-| The Elm interface to the `render-notify` custom element.

This code won't do anything unless `site/js/render-notify.js` is loaded.


# Html Elements

@docs renderNotify


# Attributes

@docs notifyValue


# Events

@docs onRender

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Create a render-notify HTML element.
-}
renderNotify : List (Attribute msg) -> List (Html msg) -> Html msg
renderNotify =
    Html.node "render-notify"


type alias WireValue =
    { count : Int
    , value : Value
    }


encodeWireValue : Value -> Value
encodeWireValue value =
    -- Wrapped in an object so it's unique each time
    JE.object [ ( "value", value ) ]


valueDecoder : Decoder Value
valueDecoder =
    JD.field "value" JD.value


{-| This value will be sent back through onNotify.
-}
notifyValue : Value -> Attribute msg
notifyValue value =
    property "triggerValue" <| encodeWireValue value


{-| Notification will happen after your `view` function sends a value.
-}
onRender : (Value -> msg) -> Attribute msg
onRender tagger =
    on "render-notify" <|
        JD.map tagger <|
            JD.at [ "target", "triggerValue" ] valueDecoder

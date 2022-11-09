----------------------------------------------------------------------
--
-- WatchColorSchemeExample.elm
-- Example of using the CustomElement.WatchColorScheme module.
-- Your index.html must load site/js/watch-color-scheme.js,
-- or this will do nothing but display "unknown".
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WatchColorSchemeExample exposing (main)

import Browser
import CustomElement.WatchColorScheme as WatchColorScheme exposing (ColorScheme(..))
import Html
    exposing
        ( Attribute
        , Html
        , b
        , div
        , h2
        , p
        , text
        )
import Html.Attributes exposing (style)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { colorScheme : Maybe ColorScheme
    }


type Msg
    = SetColorScheme ColorScheme


init : () -> ( Model, Cmd Msg )
init _ =
    ( { colorScheme = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetColorScheme colorScheme ->
            ( { model
                | colorScheme = Just colorScheme
              }
            , Cmd.none
            )


colorSchemeStyles : Maybe ColorScheme -> List (Attribute msg)
colorSchemeStyles colorScheme =
    case colorScheme of
        Nothing ->
            [ style "background" "red"
            , style "color" "blue"
            ]

        Just cs ->
            case cs of
                LightColorScheme ->
                    [ style "background" "white"
                    , style "color" "black"
                    ]

                DarkColorScheme ->
                    [ style "background" "black"
                    , style "color" "white"
                    ]


view : Model -> Html Msg
view model =
    div
        (List.concat
            [ [ style "width" "100%"
              , style "height" "100%"
              , style "padding" "8"
              ]
            , colorSchemeStyles model.colorScheme
            ]
        )
        [ h2 []
            [ text "Watch Color Scheme Example" ]
        , WatchColorScheme.watchColorScheme [ WatchColorScheme.onChange SetColorScheme ]
            []
        , p []
            [ b [] [ text "Color Scheme: " ]
            , text <|
                case model.colorScheme of
                    Nothing ->
                        "unknown"

                    Just colorScheme ->
                        case colorScheme of
                            DarkColorScheme ->
                                "Dark"

                            LightColorScheme ->
                                "Light"
            ]
        ]

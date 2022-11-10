----------------------------------------------------------------------
--
-- AppStateExample.elm
-- Example of using `DynamoDB.AppState`.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module AppStateExample exposing (main)

import Browser
import Dict exposing (Dict)
import DynamoDB
import DynamoDB.AppState as AppState exposing (AppState, Error, InitialLoad)
import DynamoDB.Types as Types
    exposing
        ( Account
        , AttributeValue(..)
        , Error(..)
        , Item
        , Key(..)
        , TableName
        )
import Html
    exposing
        ( Attribute
        , Html
        , b
        , div
        , h2
        , option
        , p
        , select
        , text
        )
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Task exposing (Task)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { account : Account
    , appState : AppState
    , display : String
    , accounts : List Account
    }


type Msg
    = ReceiveAccounts (Result Error (List Account))
    | SetAccount String


emptyAccount : Account
emptyAccount =
    let
        account =
            AppState.emptyAccount
    in
    { account
        | name = "mammudeck"
        , tableName = "mammudeck"
    }


emptyAppState : AppState
emptyAppState =
    AppState.makeAppState emptyAccount


init : () -> ( Model, Cmd Msg )
init _ =
    ( { account = emptyAccount
      , appState = emptyAppState
      , display = "Fetching accounts..."
      , accounts = []
      }
    , Task.attempt ReceiveAccounts
        (DynamoDB.readAccounts <| Just "accounts.json")
    )


findAccount : Model -> String -> Account
findAccount model name =
    case LE.find (\a -> a.name == name) model.accounts of
        Nothing ->
            emptyAccount

        Just a ->
            a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAccount name ->
            let
                account =
                    findAccount model name

                appState =
                    model.appState
            in
            ( { model
                | account = account
                , appState = { appState | account = account }
              }
            , Cmd.none
            )

        ReceiveAccounts result ->
            case result of
                Err error ->
                    ( { model | display = Debug.toString error }
                    , Cmd.none
                    )

                Ok accounts ->
                    let
                        account =
                            case accounts of
                                a :: _ ->
                                    a

                                _ ->
                                    emptyAccount

                        appState =
                            model.appState
                    in
                    ( { model
                        | accounts = accounts
                        , account = account
                        , appState =
                            { appState | account = account }
                        , display = "Accounts received."
                      }
                    , Cmd.none
                    )


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    div []
        [ h2 []
            [ text "AppState Example" ]
        , p [ style "color" "red" ]
            [ text model.display ]
        , p []
            [ b "Account: "
            , accountSelector model
            , br
            , b "TableName: "
            , text model.account.tableName
            ]
        ]


accountSelector : Model -> Html Msg
accountSelector model =
    select [ on "change" (JD.map SetAccount targetValue) ]
        (List.map (accountOption model) model.accounts)


accountOption : Model -> Account -> Html Msg
accountOption model account =
    option
        [ value account.name
        , selected (model.account.name == account.name)
        ]
        [ text account.name ]

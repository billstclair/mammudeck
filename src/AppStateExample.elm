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
import DynamoDB.AppState as AppState exposing (AppState, Updates)
import DynamoDB.Html exposing (renderTable)
import DynamoDB.Types as Types
    exposing
        ( Account
        , AttributeValue(..)
        , Item
        , Key(..)
        , TableName
        )
import Html
    exposing
        ( Attribute
        , Html
        , b
        , button
        , div
        , h2
        , input
        , option
        , p
        , select
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, selected, size, style, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Task exposing (Task)
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Time.every 1000 Tick
        , view = view
        }


type alias Model =
    { time : Int
    , display : String
    , accounts : List Account
    , account : Account
    , appState : AppState
    , columns : List String
    , row : Row
    , selection : Row
    , rows : List Row
    , columnName : String
    }


type Msg
    = ReceiveAccounts (Result Types.Error (List Account))
    | ReceiveAppStateUpdates (Result AppState.Error (Maybe Updates))
    | ReceiveAppStateStore (Result AppState.Error Int)
    | SaveRow (Maybe String) (Maybe Row)
    | Tick Posix
    | SetAccount String
    | SetSelection Row
    | SetColumnName String
    | SetRowColumn String String
    | AddRow
    | RemoveRow
    | UpdateRow


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
    let
        appState =
            AppState.makeAppState emptyAccount
    in
    { appState
        | keyPrefix = Just "_AppStateExample"
        , idlePeriod = 2000
        , updatePeriod = 2000
    }


type alias Row =
    Dict String String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , display = "Fetching accounts..."
      , accounts = []
      , account = emptyAccount
      , appState = emptyAppState
      , columns = [ "key", "value" ]
      , row = Dict.empty
      , selection = Dict.empty
      , rows = []
      , columnName = "key"
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


sortRows : List Row -> List Row
sortRows rows =
    List.sortBy (\x -> Maybe.withDefault "" <| Dict.get "key" x) rows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            let
                time =
                    Time.posixToMillis posix

                ( mdl, cmd ) =
                    if AppState.accountIncomplete model.appState then
                        ( model, Cmd.none )

                    else
                        case AppState.idle time model.appState of
                            Nothing ->
                                case AppState.update time model.appState of
                                    Nothing ->
                                        ( model, Cmd.none )

                                    Just ( appState, task ) ->
                                        ( { model | appState = appState }
                                        , Task.attempt ReceiveAppStateUpdates task
                                        )

                            Just ( appState, task ) ->
                                ( { model | appState = appState }
                                , Task.attempt ReceiveAppStateStore task
                                )
            in
            ( { mdl | time = time }
            , cmd
            )

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
                        ( account, cmdp ) =
                            case accounts of
                                a :: _ ->
                                    ( a, True )

                                _ ->
                                    ( emptyAccount, False )

                        astate =
                            model.appState

                        appState =
                            { astate | account = account }
                    in
                    ( { model
                        | accounts = accounts
                        , account = account
                        , appState = appState
                        , display = "Accounts received."
                      }
                    , if not cmdp then
                        Cmd.none

                      else if AppState.accountIncomplete appState then
                        Cmd.none

                      else
                        AppState.initialLoad appState appState.saveCount appState.keyCounts
                            |> Task.attempt ReceiveAppStateUpdates
                    )

        ReceiveAppStateUpdates result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok Nothing ->
                    ( model, Cmd.none )

                Ok (Just updates) ->
                    let
                        appState =
                            model.appState

                        folder k mv rowsTail =
                            case mv of
                                Nothing ->
                                    rowsTail

                                Just v ->
                                    Dict.fromList [ ( "key", k ), ( "value", v ) ]
                                        :: rowsTail

                        oldRows =
                            List.filter
                                (\row ->
                                    case Dict.get "key" row of
                                        Nothing ->
                                            True

                                        Just k ->
                                            Nothing
                                                == Dict.get k updates.updates
                                )
                                model.rows

                        rows =
                            Dict.foldr folder oldRows updates.updates
                    in
                    ( { model
                        | display =
                            if model.appState.saveCount == 0 then
                                "InitialLoad received."

                            else
                                "Updates received."
                        , appState =
                            { appState
                                | saveCount = updates.saveCount
                                , keyCounts = updates.keyCounts
                            }
                        , rows = sortRows rows
                      }
                    , Cmd.none
                    )

        ReceiveAppStateStore result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok count ->
                    if count == 0 then
                        -- Nothing actually done
                        ( model, Cmd.none )

                    else
                        ( { model
                            | display =
                                "Saved " ++ String.fromInt count ++ " items."
                          }
                        , Cmd.none
                        )

        SaveRow key row ->
            case key of
                Nothing ->
                    ( model, Cmd.none )

                Just k ->
                    let
                        val =
                            case row of
                                Nothing ->
                                    Nothing

                                Just mv ->
                                    case Dict.get "value" mv of
                                        Nothing ->
                                            Nothing

                                        Just v ->
                                            Just <| JE.string v
                    in
                    if AppState.accountIncomplete model.appState then
                        ( { model
                            | display =
                                "Can't save to DynamoDB: account incomplete."
                          }
                        , Cmd.none
                        )

                    else
                        case AppState.save model.time k val model.appState of
                            Nothing ->
                                ( model, Cmd.none )

                            Just ( appState, task ) ->
                                ( { model | appState = appState }
                                , Task.attempt ReceiveAppStateStore task
                                )

        SetSelection row ->
            ( { model
                | selection = row
                , row = row
              }
            , Cmd.none
            )

        SetColumnName name ->
            ( { model | columnName = name }
            , Cmd.none
            )

        SetRowColumn columnName v ->
            let
                row =
                    if v == "" then
                        Dict.remove columnName model.row

                    else
                        Dict.insert columnName v model.row
            in
            ( { model | row = row }
            , Cmd.none
            )

        AddRow ->
            let
                keyValue =
                    Dict.get "key" model.row
            in
            case LE.find (\row -> Dict.get "key" row == keyValue) model.rows of
                Just _ ->
                    update UpdateRow model

                Nothing ->
                    ( { model
                        | selection = model.row
                        , rows =
                            model.rows ++ [ model.row ] |> sortRows
                      }
                    , saveRow keyValue <| Just model.row
                    )

        RemoveRow ->
            let
                keyValue =
                    Dict.get "key" model.row
            in
            ( { model
                | selection =
                    case keyValue of
                        Nothing ->
                            Dict.empty

                        _ ->
                            model.selection
                , rows = LE.filterNot ((==) keyValue << Dict.get "key") model.rows
              }
            , saveRow keyValue Nothing
            )

        UpdateRow ->
            case Dict.get "key" model.row of
                Nothing ->
                    ( model, Cmd.none )

                keyValue ->
                    ( { model
                        | selection = model.row
                        , rows =
                            List.map
                                (\row ->
                                    if Dict.get "key" row == keyValue then
                                        model.row

                                    else
                                        row
                                )
                                model.rows
                                |> sortRows
                      }
                    , saveRow keyValue <| Just model.row
                    )


saveRow : Maybe String -> Maybe Row -> Cmd Msg
saveRow key row =
    Task.perform (SaveRow key) <| Task.succeed row


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


rowTable : Row -> Model -> Html Msg
rowTable row model =
    let
        columnTh columnName =
            th [] [ text columnName ]

        columnTd columnName =
            let
                v =
                    case Dict.get columnName model.row of
                        Nothing ->
                            ""

                        Just s ->
                            s
            in
            td []
                [ input
                    [ value v
                    , size 20
                    , onInput <| SetRowColumn columnName
                    ]
                    []
                ]
    in
    table [ class "prettytable" ] <|
        tr [] (List.map columnTh model.columns)
            :: List.map columnTd model.columns


view : Model -> Html Msg
view model =
    let
        addSelectionMarker row =
            if row == model.selection then
                Dict.insert "*" "*" row

            else
                row
    in
    div [ style "margin" "8px" ]
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
        , p []
            [ rowTable model.row model
            , button [ onClick AddRow ]
                [ text "Add" ]
            , text " "
            , button [ onClick RemoveRow ]
                [ text "Remove" ]
            , text " "
            , button [ onClick UpdateRow ]
                [ text "Update" ]
            ]
        , p []
            [ renderTable SetSelection
                { tableConfig | columnDescriptors = "*" :: model.columns }
                (List.map addSelectionMarker model.rows)
            ]
        ]


tableConfig =
    { columnDescriptors = []
    , columnDescriptorToString = identity
    , elementToString = getRowValue
    }


getRowValue : String -> Row -> Maybe String
getRowValue column row =
    Dict.get column row


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

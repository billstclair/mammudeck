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
import DynamoDB.Html exposing (renderTable)
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


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { display : String
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
    = ReceiveAccounts (Result Error (List Account))
    | SetAccount String
    | SetSelection Row
    | SetColumnName String
    | AddColumn
    | RemoveColumn
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
    AppState.makeAppState emptyAccount


type alias Row =
    Dict String String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = emptyAccount
      , appState = emptyAppState
      , columns = [ "key", "value" ]
      , row = Dict.empty
      , selection = Dict.empty
      , rows =
            [ Dict.fromList [ ( "key", "key1" ), ( "value", "value1" ) ]
            , Dict.fromList [ ( "key", "key2" ), ( "value", "value2" ) ]
            ]
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

        AddColumn ->
            if
                (model.columnName == "*")
                    || List.member model.columnName model.columns
            then
                ( model, Cmd.none )

            else
                ( { model
                    | columns =
                        List.append model.columns [ model.columnName ]
                  }
                , Cmd.none
                )

        RemoveColumn ->
            if model.columnName == "key" then
                ( model, Cmd.none )

            else
                ( { model
                    | columns =
                        LE.filterNot ((==) model.columnName) model.columns
                  }
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
            ( { model
                | selection = model.row
                , rows =
                    model.rows ++ [ model.row ]
              }
            , Cmd.none
            )

        RemoveRow ->
            ( { model
                | selection =
                    if model.row == model.selection then
                        Dict.empty

                    else
                        model.selection
                , rows = LE.filterNot ((==) model.row) model.rows
              }
            , Cmd.none
            )

        UpdateRow ->
            ( { model
                | selection = model.row
                , rows =
                    List.map
                        (\row ->
                            if row == model.selection then
                                model.row

                            else
                                row
                        )
                        model.rows
              }
            , Cmd.none
            )


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
            [ b "Column name: "
            , input
                [ size 20
                , onInput SetColumnName
                , value model.columnName
                ]
                []
            , br
            , button [ onClick AddColumn ]
                [ text "Add column" ]
            , text " "
            , button [ onClick RemoveColumn ]
                [ text "Remove column" ]
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

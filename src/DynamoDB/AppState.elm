----------------------------------------------------------------------
--
-- DynamoDB/AppState.elm
-- Help for storing application state in DynanoDB.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module DynamoDB.AppState exposing
    ( AppState, makeAppState
    , save, idle
    , renderAccount
    , store
    , mergeAccount
    )

{-| Support for storing application state in DynamoDB.

You specify key/value pairs to save by calling `save`.
They are stored until the `savePeriod` has passed, then pushed to DynamoDB.
Each time the clock ticks, call `idle` to make sure unstored changes get pushed to DynamoDB.

Call `update` to pull changes from S3, at `updatePeriod` intervals.
It returns a list of key/value pairs that have been changed by another machine pushing to DynamoDB.


# State

@docs AppState, makeAppState


# Updating state

@docs save, idle


# User Interface

@docs renderAccount


# Internals

@docs store

-}

import Dict exposing (Dict)
import DynamoDB
import DynamoDB.Types
    exposing
        ( Account
        , AttributeValue(..)
        , Error(..)
        , Item
        , Key(..)
        , TableName
        )
import Html exposing (Html, input, label, p, span, text)
import Html.Attributes exposing (checked, size, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Task exposing (Task)


{-| Track updates for DynamoDB persistence.
-}
type alias AppState =
    { account : Account
    , savePeriod : Int
    , idlePeriod : Int
    , updatePeriod : Int
    , saveCountKey : String
    , keyCountsKey : String

    -- Above here is usually constant. Below changes.
    , lastSaveTime : Int
    , lastIdleTime : Int
    , lastUpdateTime : Int
    , saveCount : Int
    , updates : Dict String (Maybe Value)
    , keyCounts : Dict String Int
    }


{-| Make an AppState with good defaults.
-}
makeAppState : Account -> AppState
makeAppState account =
    { account = account
    , savePeriod = 5000
    , idlePeriod = 2000
    , updatePeriod = 10000
    , saveCountKey = "DynamoDB.saveCount"
    , keyCountsKey = "DynamoDB.keyCounts"

    -- Times are in milliseconds, as returned by `Time.posixToMillis` applied
    -- to the result of `Time.now`.
    , lastSaveTime = 0
    , lastIdleTime = 0
    , lastUpdateTime = 0
    , saveCount = 0
    , updates = Dict.empty
    , keyCounts = Dict.empty
    }


{-| Add a new key/value pair to the DynamoDB store.

Don't actually push it to DynamoDB unless it's time.

Probably needs to change to return the AppState after save right away,
so that errors don't keep retyring forever. It's hard to do the right
thing for partial saves.

Need two-phase commit in DynamoDB to do this right. Don't bother.

-}
save : Int -> String -> Maybe Value -> AppState -> Maybe ( AppState, Task Error String )
save time key value appState =
    let
        state =
            { appState
                | updates = Dict.insert key value appState.updates
                , lastIdleTime = time
            }
    in
    if time <= state.lastSaveTime + state.savePeriod then
        Just
            ( appState
            , Task.succeed ""
            )

    else
        store time state


{-| Push the saved changes to DynamoDB if the idle time has elapsed.
-}
idle : Int -> AppState -> Maybe ( AppState, Task Error String )
idle time appState =
    if time <= appState.lastIdleTime + appState.idlePeriod then
        Nothing

    else
        store time appState


putDynamoDBValue : AppState -> String -> Maybe Value -> Task Error String
putDynamoDBValue appState key value =
    let
        valueString =
            case value of
                Nothing ->
                    "Nothing"

                Just v ->
                    JE.encode 0 v

        s =
            Debug.log ("*** putDynamoDBValue " ++ key) valueString
    in
    Task.succeed s


keyFieldName : String
keyFieldName =
    "key"


valueFieldName : String
valueFieldName =
    "value"


putDynamoDBValueInternal : AppState -> String -> Maybe Value -> Task Error ()
putDynamoDBValueInternal appState keyString value =
    let
        key =
            SimpleKey ( keyFieldName, StringValue keyString )
    in
    (case value of
        Nothing ->
            DynamoDB.deleteItem appState.account.tableName key

        Just v ->
            let
                attributeValue =
                    StringValue <| JE.encode 0 v

                item =
                    DynamoDB.makeItem [ ( valueFieldName, attributeValue ) ]
            in
            DynamoDB.putItem appState.account.tableName key item
    )
        |> DynamoDB.send appState.account


{-| Push all saved updates to DynamoDB.
-}
store : Int -> AppState -> Maybe ( AppState, Task Error String )
store time appState =
    if appState.updates == Dict.empty then
        Nothing

    else
        let
            saveCount =
                appState.saveCount + 1

            keyCounts =
                Dict.foldl
                    (\k v counts ->
                        Dict.insert k saveCount counts
                    )
                    appState.keyCounts
                    appState.updates

            saveTasks =
                Dict.foldl
                    (\k v tasks ->
                        putDynamoDBValue appState k v :: tasks
                    )
                    []
                    appState.updates

            keyCountsTask =
                putDynamoDBValue appState
                    appState.keyCountsKey
                    (Just <| encodeKeyCounts keyCounts)

            saveCountTask =
                putDynamoDBValue appState
                    appState.saveCountKey
                    (Just <| JE.int saveCount)

            state =
                { appState
                    | lastSaveTime = time
                    , lastIdleTime = time
                    , lastUpdateTime = time
                    , saveCount = saveCount
                    , updates = Dict.empty
                    , keyCounts = keyCounts
                }
        in
        Just
            ( appState
            , Task.sequence saveTasks
                |> Task.andThen (\_ -> keyCountsTask)
                |> Task.andThen (\_ -> saveCountTask)
            )


encodeKeyCounts : Dict String Int -> Value
encodeKeyCounts keyCounts =
    JE.dict identity JE.int keyCounts


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


{-| A UI for entering account information.

You can certainly do this yourself, if you want it to look different,
but this gives you a good start.

The callback is called when a field changes.

You need to do OK and Cancel buttons yourself.

-}
renderAccount : (Account -> msg) -> Account -> Html msg
renderAccount callback account =
    span []
        [ b "tableName: "
        , input
            [ size 20
            , value account.tableName
            , onInput <|
                \a ->
                    callback
                        { account | tableName = a }
            ]
            []
        , br
        , b "accessKey: "
        , input
            [ size 20
            , value account.accessKey
            , onInput <|
                \a -> callback { account | accessKey = a }
            ]
            []
        , br
        , b "secretKey: "
        , input
            [ size 50
            , type_ "password"
            , value account.secretKey
            , onInput <|
                \a -> callback { account | secretKey = a }
            ]
            []
        , br
        , b "region: "
        , let
            v =
                case account.region of
                    Nothing ->
                        ""

                    Just reg ->
                        reg
          in
          input
            [ size 20
            , value v
            , onInput <|
                \a ->
                    let
                        reg =
                            if a == "" then
                                Nothing

                            else
                                Just a
                    in
                    callback { account | region = reg }
            ]
            []
        ]


{-| Merge information from `Account` into AppState.
-}
mergeAccount : Account -> AppState -> AppState
mergeAccount account appState =
    { appState | account = account }

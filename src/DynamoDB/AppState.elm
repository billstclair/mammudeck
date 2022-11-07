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
    , Error, save, idle
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

@docs Error, save, idle


# User Interface

@docs renderAccount


# Internals

@docs store

-}

import Dict exposing (Dict)
import DynamoDB exposing (TransactWrite(..))
import DynamoDB.Types
    exposing
        ( Account
        , AttributeValue(..)
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
    , keyName : String
    , saveCountKey : String
    , keyCountsKey : String
    , valueAttributeName : String
    , saveCountAttributeName : String

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
    , keyName = "key"
    , saveCountKey = "DynamoDB.saveCount"
    , keyCountsKey = "DynamoDB.keyCounts"
    , valueAttributeName = "value"
    , saveCountAttributeName = "saveCount"

    -- Times are in milliseconds, as returned by `Time.posixToMillis` applied
    -- to the result of `Time.now`.
    , lastSaveTime = 0
    , lastIdleTime = 0
    , lastUpdateTime = 0
    , saveCount = 0
    , updates = Dict.empty
    , keyCounts = Dict.empty
    }


{-| DynamoDB allows a maximum of 100 `TransactItems`.

We use two for the update count and the key -> count map.

-}
maxSaves : Int
maxSaves =
    100 - 2


{-| The `Error` in the result from executing the task returned by `save` and `idle`.
-}
type alias Error =
    { appState : AppState
    , error : DynamoDB.Types.Error
    }


{-| Add a new key/value pair to the DynamoDB store.

Don't actually push it to DynamoDB unless it's time.

Probably needs to change to return the AppState after save right away,
so that errors don't keep retyring forever. It's hard to do the right
thing for partial saves.

Need two-phase commit in DynamoDB to do this right. Don't bother.

-}
save : Int -> String -> Maybe Value -> AppState -> Maybe ( AppState, Task Error Int )
save time key value appState =
    let
        state =
            { appState
                | updates = Dict.insert key value appState.updates
                , lastIdleTime = time
                , lastSaveTime =
                    if Dict.size appState.updates == 0 then
                        time

                    else
                        appState.lastSaveTime
            }
    in
    if
        (time <= state.lastSaveTime + state.savePeriod)
            && (Dict.size state.updates < maxSaves)
    then
        Just
            ( state
            , Task.succeed 0
            )

    else
        store time state


{-| Push the saved changes to DynamoDB if the idle time has elapsed.
-}
idle : Int -> AppState -> Maybe ( AppState, Task Error Int )
idle time appState =
    if time <= appState.lastIdleTime + appState.idlePeriod then
        Nothing

    else
        store time appState


makeKey : AppState -> String -> Key
makeKey appState key =
    SimpleKey ( appState.keyName, StringValue key )


makeValueItem : AppState -> Value -> Item
makeValueItem appState value =
    DynamoDB.makeItem
        [ ( appState.valueAttributeName
          , StringValue <| JE.encode 0 value
          )
        , ( appState.saveCountAttributeName
          , StringValue <| String.fromInt appState.saveCount
          )
        ]


{-| Push all saved updates to DynamoDB.
-}
store : Int -> AppState -> Maybe ( AppState, Task Error Int )
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

            tableName =
                appState.account.tableName

            saveWrites =
                List.map
                    (\( k, v ) ->
                        let
                            key =
                                SimpleKey
                                    ( appState.keyName, StringValue k )
                        in
                        case v of
                            Nothing ->
                                TransactWriteDelete
                                    { tableName = tableName
                                    , key = key
                                    }

                            Just val ->
                                TransactWritePut
                                    { tableName = tableName
                                    , key = Just key
                                    , item =
                                        makeValueItem appState val
                                    }
                    )
                <|
                    Dict.toList appState.updates

            keyCountsWrite =
                TransactWritePut
                    { tableName = tableName
                    , key = Just <| makeKey appState appState.keyCountsKey
                    , item = makeValueItem appState <| encodeKeyCounts keyCounts
                    }

            saveCountWrite =
                TransactWritePut
                    { tableName = tableName
                    , key = Just <| makeKey appState appState.saveCountKey
                    , item = makeValueItem appState <| JE.int saveCount
                    }

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
            ( state
            , saveCountWrite
                :: (keyCountsWrite :: saveWrites)
                |> DynamoDB.transactWriteItems
                |> DynamoDB.send appState.account
                |> Task.andThen
                    (\() -> Task.succeed <| Dict.size appState.updates)
                |> Task.mapError
                    (\err ->
                        { appState = appState
                        , error = err
                        }
                    )
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

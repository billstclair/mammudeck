----------------------------------------------------------------------
--
-- S3/AppState.elm
-- Help for storing application state in S3.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module S3.AppState exposing
    ( AppState, makeAppState
    , save, idle
    , store
    )

{-| Support for storing application state in S3.

You specify key/value pairs to save by calling `save`.
They are stored until the `savePeriod` has passed, then pushed to S3.
Each time the clock ticks, call `idle` to make sure unstored changes get pushed to S3.

Call `update` to pull changes from S3, at `updatePeriod` intervals.
It returns a list of key/value pairs that have been changed by another machine pushing to S3.


# State

@docs AppState, makeAppState


# Updating state

@docs save, idle


# Internals

@docs store

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import S3
import S3.Types
    exposing
        ( Account
        , Bucket
        , Error(..)
        , Key
        )
import Task exposing (Task)


{-| Track updates for S3 persistence.
-}
type alias AppState =
    { account : Account
    , bucket : Bucket
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
    , updates : Dict String Value
    , keyCounts : Dict String Int
    }


{-| Make an AppState with good defaults.
-}
makeAppState : Account -> Bucket -> AppState
makeAppState account bucket =
    { account = account
    , bucket = bucket
    , savePeriod = 5000
    , idlePeriod = 2000
    , updatePeriod = 10000
    , saveCountKey = "S3.saveCount"
    , keyCountsKey = "S3.keyCounts"

    -- Times are in milliseconds, as returned by `Time.posixToMillis` applied
    -- to the result of `Time.now`.
    , lastSaveTime = 0
    , lastIdleTime = 0
    , lastUpdateTime = 0
    , saveCount = 0
    , updates = Dict.empty
    , keyCounts = Dict.empty
    }


{-| Add a new key/value pair to the S3 store.

Don't actually push it to S3 unless it's time.

-}
save : Int -> String -> Value -> AppState -> Maybe (Task Error AppState)
save time key value appState =
    let
        state =
            { appState
                | updates = Dict.insert key value appState.updates
                , lastIdleTime = time
            }
    in
    if time <= state.lastSaveTime + state.savePeriod then
        Just <| Task.succeed state

    else
        Just <| store time state


{-| Push the saved changes to S3 if the idle time has elapsed.
-}
idle : Int -> AppState -> Maybe (Task Error AppState)
idle time appState =
    if time <= appState.lastIdleTime + appState.idlePeriod then
        Nothing

    else
        Just <| store time appState


putS3Value : AppState -> Key -> Value -> Task Error String
putS3Value appState key value =
    S3.putObject appState.bucket key (S3.jsonBody value)
        |> S3.send appState.account


{-| Push all saved updates to S3.
-}
store : Int -> AppState -> Task Error AppState
store time appState =
    if appState.updates == Dict.empty then
        Task.succeed appState

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
                        putS3Value appState k v :: tasks
                    )
                    []
                    appState.updates

            keyCountsTask =
                putS3Value appState appState.keyCountsKey <|
                    encodeKeyCounts keyCounts

            saveCountTask =
                putS3Value appState appState.saveCountKey <|
                    JE.int saveCount

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
        Task.sequence saveTasks
            |> Task.andThen (\_ -> keyCountsTask)
            |> Task.andThen (\_ -> saveCountTask)
            |> Task.andThen (\_ -> Task.succeed state)


encodeKeyCounts : Dict String Int -> Value
encodeKeyCounts keyCounts =
    JE.dict identity JE.int keyCounts

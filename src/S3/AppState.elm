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


module S3.AppState exposing (AppState, maybeSave)

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


type alias AppState =
    { account : Account
    , bucket : Bucket
    , savePeriod : Int
    , updatePeriod : Int
    , saveCountKey : String
    , keyCountsKey : String

    -- Above here is usually constant. Below changes.
    -- Maybe package above here as a single record.
    , idlePeriod : Int
    , lastSaveTime : Int
    , saveCount : Int
    , updates : Dict String Value
    , keyCounts : Dict String Int
    , lastUpdateTime : Int
    }


maybeSave : Int -> String -> Value -> AppState -> Maybe (Task Error AppState)
maybeSave time key value appState =
    let
        state =
            { appState
                | updates = Dict.insert key value appState.updates
            }
    in
    if time <= state.lastSaveTime + state.savePeriod then
        Just <| Task.succeed state

    else
        Just <| doSave time state


idleSave : Int -> AppState -> Maybe (Task Error AppState)
idleSave time appState =
    if time <= appState.lastSaveTime + appState.idlePeriod then
        Nothing

    else
        Just <| doSave time appState


putS3Value : AppState -> Key -> Value -> Task Error String
putS3Value appState key value =
    S3.putObject appState.bucket key (S3.jsonBody value)
        |> S3.send appState.account


doSave : Int -> AppState -> Task Error AppState
doSave time appState =
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
                    , saveCount = saveCount
                    , updates = Dict.empty
                    , keyCounts = keyCounts
                    , lastUpdateTime = time
                }
        in
        Task.sequence saveTasks
            |> Task.andThen (\_ -> keyCountsTask)
            |> Task.andThen (\_ -> saveCountTask)
            |> Task.andThen (\_ -> Task.succeed state)


encodeKeyCounts : Dict String Int -> Value
encodeKeyCounts keyCounts =
    JE.dict identity JE.int keyCounts

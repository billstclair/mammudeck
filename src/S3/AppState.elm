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
    , renderAccount
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


# User Interface

@docs renderAccount


# Internals

@docs store

-}

import Dict exposing (Dict)
import Html exposing (Html, input, p, span, text)
import Html.Attributes exposing (checked, size, type_, value)
import Html.Events exposing (onClick, onInput)
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


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


{-| A UI for entering account and bucket information.

You can certainly do this yourself, if you want it to look different,
but this gives you a good start.

The callback is called when a field changes.
You need to do OK and Cancel buttons yourself.

-}
renderAccount : Account -> (Account -> msg) -> Html msg
renderAccount account callback =
    span []
        [ b "bucket: "
        , input
            [ size 20
            , value
                (List.head account.buckets |> Maybe.withDefault "")
            , onInput <|
                \a ->
                    callback
                        { account
                            | buckets =
                                if a == "" then
                                    []

                                else
                                    [ a ]
                        }
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
        , br
        , b "isDigitalOcean: "
        , input
            [ type_ "checkbox"
            , checked account.isDigitalOcean
            , onClick <|
                callback
                    { account
                        | isDigitalOcean = not account.isDigitalOcean
                    }
            ]
            []
        ]

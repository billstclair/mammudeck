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
import Html exposing (Html, input, label, p, span, text)
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
    , updates : Dict String (Maybe Value)
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

Probably needs to change to return the AppState after save right away,
so that errors don't keep retyring forever. It's hard to do the right
thing for partial saves.

Need two-phase commit in S3 to do this right. Don't bother.

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


{-| Push the saved changes to S3 if the idle time has elapsed.
-}
idle : Int -> AppState -> Maybe ( AppState, Task Error String )
idle time appState =
    if time <= appState.lastIdleTime + appState.idlePeriod then
        Nothing

    else
        store time appState


putS3Value : AppState -> Key -> Maybe Value -> Task Error String
putS3Value appState key value =
    let
        valueString =
            case value of
                Nothing ->
                    "Nothing"

                Just v ->
                    JE.encode 0 v

        s =
            Debug.log ("*** putS3Value " ++ key) valueString
    in
    Task.succeed s


putS3ValueInternal : AppState -> Key -> Maybe Value -> Task Error String
putS3ValueInternal appState key value =
    (case value of
        Nothing ->
            S3.deleteObject appState.bucket key

        Just v ->
            S3.putObject appState.bucket key (S3.jsonBody v)
    )
        |> S3.send appState.account


{-| Push all saved updates to S3.
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
                        putS3Value appState k v :: tasks
                    )
                    []
                    appState.updates

            keyCountsTask =
                putS3Value appState
                    appState.keyCountsKey
                    (Just <| encodeKeyCounts keyCounts)

            saveCountTask =
                putS3Value appState
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


{-| A UI for entering account and bucket information.

You can certainly do this yourself, if you want it to look different,
but this gives you a good start.

The callback is called when a field changes.

You need to do OK and Cancel buttons yourself.

-}
renderAccount : Account -> (Account -> msg) -> Html msg
renderAccount account callback =
    let
        ( disabled, bucket ) =
            case account.buckets of
                [] ->
                    ( False, "" )

                [ buck ] ->
                    ( False, buck )

                buck :: tail ->
                    ( True, buck )
    in
    span []
        [ b "bucket: "
        , input
            [ size 20
            , value bucket
            , onInput <|
                \a ->
                    callback
                        { account
                            | buckets =
                                if disabled then
                                    [ a, "<disabled>" ]

                                else if a == "" then
                                    []

                                else
                                    [ a ]
                        }
            ]
            []
        , label []
            [ text " "
            , input
                [ type_ "checkbox"
                , checked disabled
                , onClick <|
                    callback
                        { account
                            | buckets =
                                if not disabled then
                                    [ bucket, "<disabled>" ]

                                else if bucket == "" then
                                    []

                                else
                                    [ bucket ]
                        }
                ]
                []
            , text " disabled"
            ]
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
        , label []
            [ b "isDigitalOcean: "
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
        ]


{-| Merge information from `Account` into AppState.

If `account.buckets` has 0 or more than 1 element, `appState.disabled`
will be set true and `appState.bucket` will be set to `""`.

-}
mergeAccount : Account -> AppState -> AppState
mergeAccount account appState =
    let
        ( disabled, bucket ) =
            case account.buckets of
                "" :: _ ->
                    ( True, "" )

                [ buck ] ->
                    ( False, buck )

                _ ->
                    ( True, "" )
    in
    { appState
        | account = account
        , bucket = bucket
    }

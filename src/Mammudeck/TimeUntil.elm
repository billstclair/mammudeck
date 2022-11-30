----------------------------------------------------------------
--
-- TimeUntil.elm
-- Format time differences as a string.
-- Copyright (c) 2019-2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mammudeck.TimeUntil exposing (timeUntil)

import List
import Time exposing (Posix, Zone)
import Time.Extra as TE exposing (Interval(..))


timeUntil : Zone -> Int -> Posix -> Posix -> String
timeUntil zone count from to =
    let
        mfrom =
            Time.posixToMillis from

        mto =
            Time.posixToMillis to
    in
    if mfrom == mto then
        "now"

    else if mfrom < mto then
        "in " ++ timeUntilInternal zone count from to

    else
        timeUntilInternal zone count to from ++ " ago"


intervals : List ( Interval, String )
intervals =
    [ ( Year, "year" )
    , ( Month, "month" )
    , ( Day, "day" )
    , ( Hour, "hour" )
    , ( Minute, "minute" )
    , ( Second, "second" )
    , ( Millisecond, "millisecond" )
    ]


timeUntilInternal : Zone -> Int -> Posix -> Posix -> String
timeUntilInternal zone count from to =
    let
        pluralize : Int -> String -> String
        pluralize diff label =
            if diff == 1 then
                String.fromInt diff ++ " " ++ label

            else
                String.fromInt diff ++ " " ++ label ++ "s"

        loop : Int -> Posix -> List ( Interval, String ) -> List String -> List String
        loop cnt start intervalsTail res =
            if cnt <= 0 then
                List.reverse res

            else
                case intervalsTail of
                    [] ->
                        List.reverse res

                    ( interval, label ) :: rest ->
                        let
                            diff =
                                TE.diff interval zone start to
                        in
                        if diff == 0 && res == [] then
                            loop cnt start rest res

                        else
                            let
                                next =
                                    TE.add interval diff zone start
                            in
                            loop (cnt - 1) next rest <|
                                pluralize diff label
                                    :: res
    in
    case loop count from intervals [] of
        [] ->
            "0 seconds"

        list ->
            String.join ", " list

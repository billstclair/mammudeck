----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON Encoders and Decoders
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mammudeck.EncodeDecode exposing
    ( encodeFeedSetDefinition
    , feedSetDefinitionDecoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Mammudeck.Types as Types
    exposing
        ( FeedSetDefinition
        , FeedType(..)
        , UserFeedFlags
        )
import Mastodon.EncodeDecode as ED


encodeUserFeedFlags : UserFeedFlags -> Value
encodeUserFeedFlags { only_media, pinned, replies, reblogs } =
    JE.object
        [ ( "only_media", JE.bool only_media )
        , ( "pinned", JE.bool pinned )
        , ( "replies", JE.bool replies )
        , ( "reblogs", JE.bool reblogs )
        ]


userFeedFlagsDecoder : Decoder UserFeedFlags
userFeedFlagsDecoder =
    JD.succeed UserFeedFlags
        |> required "only_media" JD.bool
        |> required "pinned" JD.bool
        |> required "replies" JD.bool
        |> required "reblogs" JD.bool


encodeFeedType : FeedType -> Value
encodeFeedType feedType =
    case feedType of
        HomeFeed ->
            JE.string "HomeFeed"

        UserFeed { username, id, flags } ->
            JE.object
                [ ( "feedType", JE.string "UserFeed" )
                , ( "username", JE.string username )
                , ( "id", JE.string id )
                , ( "flags", ED.encodeMaybe encodeUserFeedFlags flags )
                ]

        _ ->
            -- TODO
            JE.null


encodeFeedSetDefinition : FeedSetDefinition -> Value
encodeFeedSetDefinition { name, feedTypes } =
    JE.object
        [ ( "name", JE.string name )
        , ( "feedTypes", JE.list encodeFeedType feedTypes )
        ]


feedSetDefinitionDecoder : Decoder FeedSetDefinition
feedSetDefinitionDecoder =
    JD.succeed Types.defaultFeedSetDefinition

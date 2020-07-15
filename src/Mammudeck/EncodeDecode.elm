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
    ( accountIdsDecoder
    , encodeAccountIds
    , encodeFeedSetDefinition
    , feedSetDefinitionDecoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Mammudeck.Types as Types
    exposing
        ( AccountId
        , FeedSetDefinition
        , FeedType(..)
        , ProFeedFlags
        , PublicFeedFlags
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


encodeProFeedFlags : ProFeedFlags -> Value
encodeProFeedFlags { only_media } =
    JE.object
        [ ( "only_media", JE.bool only_media )
        ]


proFeedFlagsDecoder : Decoder ProFeedFlags
proFeedFlagsDecoder =
    JD.succeed ProFeedFlags
        |> required "only_media" JD.bool


encodePublicFeedFlags : PublicFeedFlags -> Value
encodePublicFeedFlags { local, only_media } =
    JE.object
        [ ( "local", JE.bool local )
        , ( "only_media", JE.bool only_media )
        ]


publicFeedFlagsDecoder : Decoder PublicFeedFlags
publicFeedFlagsDecoder =
    JD.succeed PublicFeedFlags
        |> required "local" JD.bool
        |> required "only_media" JD.bool


encodeFeedType : FeedType -> Value
encodeFeedType feedType =
    case feedType of
        HomeFeed ->
            JE.string "HomeFeed"

        UserFeed { username, server, flags } ->
            JE.object
                [ ( "feedType", JE.string "UserFeed" )
                , ( "username", JE.string username )
                , ( "server", JE.string server )
                , ( "flags", ED.encodeMaybe encodeUserFeedFlags flags )
                ]

        ProFeed { flags } ->
            JE.object
                [ ( "feedType", JE.string "ProFeed" )
                , ( "flags", ED.encodeMaybe encodeProFeedFlags flags )
                ]

        PublicFeed { flags } ->
            JE.object
                [ ( "feedType", JE.string "PublicFeed" )
                , ( "flags", ED.encodeMaybe encodePublicFeedFlags flags )
                ]

        HashtagFeed hashtag ->
            JE.object
                [ ( "feedType", JE.string "HashtagFeed" )
                , ( "hashtag", JE.string hashtag )
                ]

        ListFeed listname ->
            JE.object
                [ ( "feedType", JE.string "ListFeed" )
                , ( "listname", JE.string listname )
                ]

        GroupFeed groupname ->
            JE.object
                [ ( "feedType", JE.string "GroupFeed" )
                , ( "groupname", JE.string groupname )
                ]

        NotificationFeed { accountId, exclusions } ->
            JE.object
                [ ( "feedType", JE.string "NotificationFeed" )
                , ( "accountId", ED.encodeMaybe JE.string accountId )
                , ( "exclusions", JE.list ED.encodeNotificationType exclusions )
                ]

        ConversationsFeed ->
            JE.string "ConversationsFeed"

        SearchFeed { q, resolve, following } ->
            JE.object
                [ ( "feedType", JE.string "SearchFeed" )
                , ( "q", JE.string q )
                , ( "resolve", JE.bool resolve )
                , ( "following", JE.bool following )
                ]


feedTypeDecoder : Decoder FeedType
feedTypeDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    case s of
                        "HomeFeed" ->
                            JD.succeed HomeFeed

                        "ConversationsFeed" ->
                            JD.succeed ConversationsFeed

                        _ ->
                            JD.fail <| "Unknown FeedType: " ++ s
                )
        , JD.field "feedType" JD.string
            |> JD.andThen
                (\feedType ->
                    case feedType of
                        "UserFeed" ->
                            JD.succeed
                                (\username server flags ->
                                    UserFeed
                                        { username = username
                                        , server = server
                                        , flags = flags
                                        }
                                )
                                |> required "username" JD.string
                                |> required "server" JD.string
                                |> optional "flags" (JD.nullable userFeedFlagsDecoder) Nothing

                        "ProFeed" ->
                            JD.succeed
                                (\flags ->
                                    ProFeed { flags = flags }
                                )
                                |> optional "flags" (JD.nullable proFeedFlagsDecoder) Nothing

                        "PublicFeed" ->
                            JD.succeed
                                (\flags ->
                                    PublicFeed { flags = flags }
                                )
                                |> optional "flags" (JD.nullable publicFeedFlagsDecoder) Nothing

                        "HashtagFeed" ->
                            JD.succeed HashtagFeed
                                |> required "hashtag" JD.string

                        "ListFeed" ->
                            JD.succeed ListFeed
                                |> required "listname" JD.string

                        "GroupFeed" ->
                            JD.succeed GroupFeed
                                |> required "groupname" JD.string

                        "NotificationFeed" ->
                            JD.succeed
                                (\accountId exclusions ->
                                    NotificationFeed
                                        { accountId = accountId
                                        , exclusions = exclusions
                                        }
                                )
                                |> optional "accountId" (JD.nullable JD.string) Nothing
                                |> required "exclusions" (JD.list ED.notificationTypeDecoder)

                        "SearchFeed" ->
                            JD.succeed
                                (\q resolve following ->
                                    SearchFeed
                                        { q = q
                                        , resolve = resolve
                                        , following = following
                                        }
                                )
                                |> required "q" JD.string
                                |> required "resolve" JD.bool
                                |> required "following" JD.bool

                        _ ->
                            JD.fail <| "Unknown feedType: " ++ feedType
                )
        ]


encodeFeedSetDefinition : FeedSetDefinition -> Value
encodeFeedSetDefinition { name, feedTypes } =
    JE.object
        [ ( "name", JE.string name )
        , ( "feedTypes", JE.list encodeFeedType feedTypes )
        ]


feedSetDefinitionDecoder : Decoder FeedSetDefinition
feedSetDefinitionDecoder =
    JD.succeed FeedSetDefinition
        |> required "name" JD.string
        |> required "feedTypes" (JD.list feedTypeDecoder)


encodeAccountId : AccountId -> Value
encodeAccountId { id, username, display_name, avatar, url } =
    JE.object
        [ ( "id", JE.string id )
        , ( "username", JE.string username )
        , ( "display_name", JE.string display_name )
        , ( "avatar", JE.string avatar )
        , ( "url", JE.string url )
        ]


accountIdDecoder : Decoder AccountId
accountIdDecoder =
    JD.succeed AccountId
        |> required "id" JD.string
        |> required "username" JD.string
        |> required "display_name" JD.string
        |> required "avatar" JD.string
        |> required "url" JD.string


encodeAccountIds : List AccountId -> Value
encodeAccountIds accountIds =
    JE.list encodeAccountId accountIds


accountIdsDecoder : Decoder (List AccountId)
accountIdsDecoder =
    JD.list accountIdDecoder

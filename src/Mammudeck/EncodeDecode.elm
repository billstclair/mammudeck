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
    , encodeFeedType
    , encodePollDefinition
    , encodePropertyAsList
    , encodeUserFeedFlags
    , feedSetDefinitionDecoder
    , feedTypeDecoder
    , pollDefinitionDecoder
    , userFeedFlagsDecoder
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
        , defaultUserFeedType
        )
import Mastodon.EncodeDecode as ED
import Mastodon.Request exposing (PollDefinition)


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


encodePropertyAsList : String -> property -> (property -> Value) -> property -> List ( String, Value )
encodePropertyAsList name property encoder default =
    if default == property then
        []

    else
        [ ( name, encoder property ) ]


encodeFeedType : FeedType -> Value
encodeFeedType feedType =
    case feedType of
        HomeFeed ->
            JE.string "HomeFeed"

        UserFeed { username, server, flags } ->
            JE.object <|
                List.concat
                    [ [ ( "feedType", JE.string "UserFeed" )
                      , ( "username", JE.string username )
                      ]
                    , encodePropertyAsList "server"
                        server
                        JE.string
                        ""
                    , encodePropertyAsList "flags"
                        flags
                        (ED.encodeMaybe encodeUserFeedFlags)
                        Nothing
                    ]

        ProFeed { flags } ->
            JE.object <|
                List.concat
                    [ [ ( "feedType", JE.string "ProFeed" ) ]
                    , encodePropertyAsList "flags"
                        flags
                        (ED.encodeMaybe encodeProFeedFlags)
                        Nothing
                    ]

        PublicFeed { flags } ->
            JE.object <|
                List.concat
                    [ [ ( "feedType", JE.string "PublicFeed" ) ]
                    , encodePropertyAsList "flags"
                        flags
                        (ED.encodeMaybe encodePublicFeedFlags)
                        Nothing
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
            JE.object <|
                List.concat
                    [ [ ( "feedType", JE.string "NotificationFeed" ) ]
                    , encodePropertyAsList "accountId"
                        accountId
                        (ED.encodeMaybe JE.string)
                        Nothing
                    , encodePropertyAsList "exclusions"
                        exclusions
                        (JE.list ED.encodeNotificationType)
                        Types.defaultNotificationExclusions
                    ]

        ConversationsFeed ->
            JE.string "ConversationsFeed"

        SearchFeed { q, resolve, following } ->
            JE.object <|
                List.concat
                    [ [ ( "feedType", JE.string "SearchFeed" )
                      , ( "q", JE.string q )
                      ]
                    , encodePropertyAsList "resolve"
                        resolve
                        JE.bool
                        False
                    , encodePropertyAsList "following"
                        following
                        JE.bool
                        False
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
                                |> optional "server" JD.string ""
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
                                |> optional "exclusions"
                                    (JD.list ED.notificationTypeDecoder)
                                    Types.defaultNotificationExclusions

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
                                |> optional "resolve" JD.bool False
                                |> optional "following" JD.bool False

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


encodePollDefinition : PollDefinition -> Value
encodePollDefinition { options, expires_in, multiple, hide_totals } =
    JE.object <|
        List.concat
            [ [ ( "options", JE.list JE.string options )
              , ( "expires_in", JE.int expires_in )
              ]
            , encodePropertyAsList "multiple"
                multiple
                JE.bool
                False
            , encodePropertyAsList "hide_totals"
                hide_totals
                JE.bool
                False
            ]


pollDefinitionDecoder : Decoder PollDefinition
pollDefinitionDecoder =
    JD.succeed PollDefinition
        |> required "options" (JD.list JD.string)
        |> required "expires_in" JD.int
        |> optional "multiple" JD.bool False
        |> optional "hide_totals" JD.bool False

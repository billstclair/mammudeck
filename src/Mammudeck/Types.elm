----------------------------------------------------------------------
--
-- Types.elm
-- Shared types for Mammudeck.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mammudeck.Types exposing
    ( Feed
    , FeedElements(..)
    , FeedSet
    , FeedSetDefinition
    , FeedType(..)
    , FetchType(..)
    , Fetcher
    , PublicFeedFlags
    , Renderer
    , UserFeedFlags
    , allButMentionNotificationExclusions
    , defaultFeedSet
    , defaultFeedSetDefinition
    , defaultNotificationExclusions
    , defaultPublicFeedFlags
    , defaultUserFeedFlags
    , emptyFeedSet
    , emptyFeedSetDefinition
    , feedSetDefinitionToFeedSet
    , feedSetToDefinition
    , feedTypeToElements
    )

import Html exposing (Html)
import Mastodon.Entity
    exposing
        ( Conversation
        , Notification
        , NotificationType(..)
        , Results
        , Status
        )
import Mastodon.Request exposing (Error, Response)
import Task
import Time exposing (Month, Posix, Zone)


type alias UserFeedFlags =
    { only_media : Bool
    , pinned : Bool
    , replies : Bool
    , reblogs : Bool
    }


defaultUserFeedFlags : UserFeedFlags
defaultUserFeedFlags =
    { only_media = False
    , pinned = False
    , replies = True
    , reblogs = True
    }


type alias PublicFeedFlags =
    { local : Bool
    , only_media : Bool
    }


defaultPublicFeedFlags : PublicFeedFlags
defaultPublicFeedFlags =
    { local = True
    , only_media = False
    }


defaultNotificationExclusions : List NotificationType
defaultNotificationExclusions =
    []


allButMentionNotificationExclusions : List NotificationType
allButMentionNotificationExclusions =
    [ FollowNotification
    , ReblogNotification
    , FavouriteNotification
    , PollNotification
    ]


type FeedType
    = HomeFeed
    | UserFeed { username : String, id : String, flags : Maybe UserFeedFlags }
    | PublicFeed { flags : Maybe PublicFeedFlags }
    | HashtagFeed String
    | ListFeed String
    | GroupFeed String
    | NotificationFeed
        { accountId : Maybe String
        , exclusions : List NotificationType
        }
    | ConversationsFeed
    | SearchFeed { q : String, resolve : Bool, following : Bool }


type FeedElements
    = StatusElements (List Status)
    | NotificationElements (List Notification)
    | ConversationsElements (List Conversation)
    | ResultsElements (List Results)


feedTypeToElements : FeedType -> FeedElements
feedTypeToElements feedType =
    case feedType of
        NotificationFeed _ ->
            NotificationElements []

        ConversationsFeed ->
            ConversationsElements []

        SearchFeed _ ->
            ResultsElements []

        _ ->
            StatusElements []


type alias Feed =
    { feedType : FeedType
    , elements : FeedElements
    }


type FetchType
    = FetchNew
    | FetchNextPage
    | FetchPreviousPage


type alias Renderer msg =
    FeedElements -> Html msg


type alias Fetcher msg =
    (Result Error Response -> msg) -> FetchType -> FeedElements -> Cmd msg


type alias FeedSet =
    { name : String
    , feeds : List Feed
    }


type alias FeedSetDefinition =
    { name : String
    , feedTypes : List FeedType
    }


feedSetToDefinition : FeedSet -> FeedSetDefinition
feedSetToDefinition { name, feeds } =
    { name = name
    , feedTypes = List.map .feedType feeds
    }


feedSetDefinitionToFeedSet : FeedSetDefinition -> FeedSet
feedSetDefinitionToFeedSet { name, feedTypes } =
    { name = name
    , feeds =
        List.map
            (\feedType ->
                { feedType = feedType
                , elements = feedTypeToElements feedType
                }
            )
            feedTypes
    }


emptyFeedSetDefinition : FeedSetDefinition
emptyFeedSetDefinition =
    { name = ""
    , feedTypes = []
    }


defaultFeedSetDefinition : FeedSetDefinition
defaultFeedSetDefinition =
    { name = "default"
    , feedTypes =
        [ HomeFeed
        , PublicFeed { flags = Nothing }
        ]
    }


emptyFeedSet : FeedSet
emptyFeedSet =
    feedSetDefinitionToFeedSet emptyFeedSetDefinition


defaultFeedSet : FeedSet
defaultFeedSet =
    feedSetDefinitionToFeedSet defaultFeedSetDefinition

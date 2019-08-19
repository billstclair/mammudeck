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
    , FeedType(..)
    , PublicFeedFlags
    , UserFeedFlags
    , allButMentionNotificationExclusions
    , defaultNotificationExclusions
    , defaultPublicFeedFlags
    , defaultUserFeedFlags
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


type alias UserFeedFlags =
    { media_only : Bool
    , pinned : Bool
    , replies : Bool
    , reblogs : Bool
    }


defaultUserFeedFlags : UserFeedFlags
defaultUserFeedFlags =
    { media_only = False
    , pinned = False
    , replies = True
    , reblogs = True
    }


type alias PublicFeedFlags =
    { local : Bool
    , media_only : Bool
    }


defaultPublicFeedFlags : PublicFeedFlags
defaultPublicFeedFlags =
    { local = True
    , media_only = False
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
    = UserFeed { username : String, id : String, flags : Maybe UserFeedFlags }
    | PublicFeed { flags : Maybe PublicFeedFlags }
    | HashtagFeed String
    | ListFeed String
    | GroupFeed String
    | NotificationFeed
        { accountId : Maybe String
        , exclusions : List NotificationType
        }
    | SearchFeed { q : String, resolve : Bool, following : Bool }


type FeedElements
    = StatusElement (List Status)
    | NotificationElement (List Notification)
    | ConversationElement (List Conversation)
    | ResultsElement (List Results)


type alias Feed msg =
    { feedType : FeedType
    , renderer : FeedElements -> Html msg
    , elements : FeedElements
    }

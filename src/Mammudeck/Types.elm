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
    ( AccountId
    , Feed
    , FeedElements(..)
    , FeedSet
    , FeedSetDefinition
    , FeedType(..)
    , FetchType(..)
    , Fetcher
    , GangedNotification
    , NotificationFeedParams
    , PublicFeedFlags
    , PublicFeedParams
    , Renderer
    , ScrollNotification
    , ScrollState(..)
    , UserFeedFlags
    , UserFeedParams
    , accountToAccountId
    , allButMentionNotificationExclusions
    , defaultFeedSet
    , defaultFeedSetDefinition
    , defaultNotificationExclusions
    , defaultPublicFeedFlags
    , defaultUserFeedFlags
    , defaultUserFeedType
    , emptyAccountId
    , emptyFeedSet
    , emptyFeedSetDefinition
    , feedID
    , feedIdToType
    , feedSetDefinitionToFeedSet
    , feedSetToDefinition
    , feedTypeToElements
    )

import Html exposing (Html)
import Mastodon.Entity
    exposing
        ( Account
        , Conversation
        , Notification
        , NotificationType(..)
        , Results
        , Status
        , UrlString
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


type alias UserFeedParams =
    { username : String
    , server : String
    , flags : Maybe UserFeedFlags
    }


defaultUserFeedType : FeedType
defaultUserFeedType =
    UserFeed
        { username = ""
        , server = ""
        , flags = Nothing
        }


type alias PublicFeedParams =
    { flags : Maybe PublicFeedFlags }


type alias NotificationFeedParams =
    { accountId : Maybe String
    , exclusions : List NotificationType
    }


type FeedType
    = HomeFeed
    | UserFeed UserFeedParams
    | PublicFeed PublicFeedParams
    | HashtagFeed String
    | ListFeed String
    | GroupFeed String
    | NotificationFeed NotificationFeedParams
    | ConversationsFeed
    | SearchFeed
        { q : String
        , resolve : Bool
        , following : Bool
        }


{-| Need to represent xxxParams, and parse them in feedIdToType
-}
feedID : FeedType -> String
feedID feedType =
    case feedType of
        HomeFeed ->
            "home"

        UserFeed { username, server } ->
            let
                nameSlashServer =
                    if server == "" then
                        username

                    else
                        username ++ "/" ++ server
            in
            "user: " ++ nameSlashServer

        PublicFeed _ ->
            "public"

        HashtagFeed hash ->
            "hashtag: " ++ hash

        ListFeed list ->
            "list: " ++ list

        GroupFeed group ->
            "group: " ++ group

        NotificationFeed _ ->
            "notifications"

        ConversationsFeed ->
            "conversations"

        SearchFeed { q } ->
            "search: " ++ q


feedIdToType : String -> Maybe FeedType
feedIdToType id =
    if "home" == id then
        Just HomeFeed

    else if "user: " == String.left 6 id then
        case String.split "/" <| String.dropLeft 6 id of
            [ username ] ->
                Just <| UserFeed { username = username, server = "", flags = Nothing }

            [ username, server ] ->
                Just <| UserFeed { username = username, server = server, flags = Nothing }

            _ ->
                Nothing

    else if "public" == id then
        Just <| PublicFeed { flags = Nothing }

    else if "hashtag: " == String.left 9 id then
        Just <| HashtagFeed (String.right 9 id)

    else if "list: " == String.left 6 id then
        Just <| ListFeed (String.dropLeft 6 id)

    else if "notifications" == id then
        Just <| NotificationFeed { accountId = Nothing, exclusions = [] }

    else if "conversations" == id then
        Just ConversationsFeed

    else if "search: " == String.left 8 id then
        Just <|
            SearchFeed
                { q = String.dropLeft 8 id
                , following = False
                , resolve = False
                }

    else
        Nothing


type FeedElements
    = StatusElements (List Status)
    | NotificationElements (List Notification)
    | AccountElements (List Account)
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
        , NotificationFeed
            { accountId = Nothing
            , exclusions = defaultNotificationExclusions
            }
        , PublicFeed { flags = Nothing }
        ]
    }


emptyFeedSet : FeedSet
emptyFeedSet =
    feedSetDefinitionToFeedSet emptyFeedSetDefinition


defaultFeedSet : FeedSet
defaultFeedSet =
    feedSetDefinitionToFeedSet defaultFeedSetDefinition


type alias GangedNotification =
    { id : String
    , notification : Notification
    , accounts : List Account
    }


type alias ScrollNotification =
    { id : String
    , scrollLeft : Int
    , scrollTop : Int
    , scrollWidth : Int
    , scrollHeight : Int
    , clientWidth : Int
    , clientHeight : Int
    }


type ScrollState
    = NoScroll
    | AwaitingGetViewportScroll
    | NotifyReceivedScroll


{-| A subset of Mastodon.Entity.Account
-}
type alias AccountId =
    { id : String
    , username : String
    , display_name : String
    , avatar : UrlString
    , url : UrlString
    }


emptyAccountId : AccountId
emptyAccountId =
    { id = ""
    , username = ""
    , display_name = ""
    , avatar = ""
    , url = ""
    }


accountToAccountId : Account -> AccountId
accountToAccountId account =
    { id = account.id
    , username = account.username
    , display_name = account.display_name
    , avatar = account.avatar
    , url = account.url
    }

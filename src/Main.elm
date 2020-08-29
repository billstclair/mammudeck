-----------------------------------------------------------------
--
-- Main.elm
-- Mammudeck, a TweetDeck-like columnar interface to Mastodon/Pleroma.
-- Copyright (c) 2019-2020 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------
--
-- Startup URLs
-- See `parseInitialPage`.
--
-- https://mammudeck.com/...
--
--   ?page=[splash|columns|api]
--   &api=[key in selectedRequestFromUrlDict]
--
----------------------------------------------------------------------
{--Immediate TODOs

See ../TODO.md for the full list.

* Refresh:
    Handle a gap between new posts and already displayed posts.
    First, detect that it happened.
    Next, truncate to just the new posts, so that scrolling will
        fill the now non-existent gap.

* ThreadExplorer:
    ** Be less eager to pull from the server.
    ** When click on existing status, scroll it to the top.
        We want to see the replies, since we already saw the
        reply-to in the previous screen.
        First status wants go at the bottom, as it does now.

* @foo in Post dialog should use (AccountsRequest << GetSearchAccounts), not
  (SearchRequest << GetSearch). The latter can be slow on servers that support
  full text search in statuses.

* Unalloyed @billstclair on impeccable.social gets the wrong ID when
  added as a column, even when clicking on popup, which should know
  the ID.

* Add reply-to as in Pleroma before message.
  ** This requires keeping a table mapping server & id to acct (username@server) & url.
     Accumulate this from Status.mentions, Notification.account, Status.account.
     Look up separately when necessary.
     All we get is Status.in_reply_to_id and Status.in_reply_to_account_id.
     Need to be able to turn that into username@server and url
     But to which server does in_reply_to_account_id refer.
     Probably loginServer, but need to verify.
  ** *DONE* modulo scanning status received after making a post.
  ** Add "pinned" indication with rendered Status

* If the first pull of notifications doesn't fill the column, there's
  no way to get it to fetch more. Need a button at the bottom to do that,
  not just scrolling.

* Configure location of the scroll pill (and call it that).
    Top, bottom or center. Left, middle, or right.
    Assuming the CSS works for center and middle.

* More feed types. Lists, groups, hashtags, search
    Parameters for user, public, and notification
    Increment search for users.
    *DONE* except lists and search

* Multiple named feedsets per server.
    Multiple host instances per feedset.

* Ellipsis dialog: block, mute, (un)follow, delete, edit, (un)mute status

* Update feed button at top of feed (first step in auto-update).
  Buttons are there, and they reload the feed.  Need to make them do
  incremental update, just load messages since the top one, and put a
  red line between new and old.

--}


port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import CustomElement.BodyColors as BodyColors
import CustomElement.RenderNotify as RenderNotify
import CustomElement.TextAreaTracker as TextAreaTracker exposing (Coordinates)
import CustomElement.WriteClipboard as WriteClipboard
import Delay
import Dialog
import Dict exposing (Dict)
import DropZone
import File exposing (File)
import File.Select
import Html
    exposing
        ( Attribute
        , Html
        , a
        , col
        , div
        , h2
        , h3
        , img
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( alt
        , autocomplete
        , autofocus
        , checked
        , class
        , cols
        , colspan
        , disabled
        , draggable
        , height
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseDown)
import Html.Lazy as Lazy
import Html.Parser exposing (Node)
import Html.Parser.Util as Util
import Http
import Iso8601
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import JsonTree exposing (TaggedValue(..))
import List.Extra as LE
import Mammudeck.EmojiChar as EmojiChar exposing (EmojiChar)
import Mammudeck.EncodeDecode as MED exposing (encodePropertyAsList)
import Mammudeck.Types as Types
    exposing
        ( AccountId
        , Feed
        , FeedElements(..)
        , FeedSet
        , FeedSetDefinition
        , FeedType(..)
        , FetchType(..)
        , Fetcher
        , GangedNotification
        , PublicFeedFlags
          --, Renderer
        , ScrollNotification
        , UserFeedFlags
        , UserFeedParams
        )
import Markdown
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity
    exposing
        ( Account
        , App
        , Attachment
        , AttachmentType(..)
        , Authorization
        , Context
        , Datetime
        , Emoji
        , Entity(..)
        , Field
        , FilterContext(..)
        , Focus
        , Group
        , ListEntity
        , Mention
        , Notification
        , NotificationType(..)
        , Privacy(..)
        , Status
        , UrlString
        , Visibility(..)
        , WrappedStatus(..)
        )
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , FieldUpdate
        , Paging
        , PollDefinition
        , RawRequest
        , Request(..)
        , Response
        , WhichGroups
        , emptyPaging
        )
import PopupPicker exposing (PopupPicker)
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Regex
import Set exposing (Set)
import String.Extra as SE
import Svg exposing (Svg, svg)
import Svg.Attributes as Svga
import Svg.Button as Button exposing (Button, TriangularButtonDirection(..))
import Task
import Time exposing (Month, Posix, Zone)
import Time.Format as Format
import Time.Format.Config.Configs as Configs
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


{-| This is used by links created by Util.toVirtualDom calls below.

It forces them to open in a new tab/window.

-}
port openWindow : Value -> Cmd msg


{-| Scroll monitor requests.

JSON: {id: <string>, enable: <bool>}

-}
port scrollRequest : Value -> Cmd msg


{-| Scroll monitor notifications.

JSON: {id: <string>, scrollX: <int>, scrollY: <int>}

-}
port scrollNotify : (Value -> msg) -> Sub msg


{-| When the window is focused, this gets True, when blurred, False.
-}
port focusNotify : (Bool -> msg) -> Sub msg


{-| Make the iPhone bring up the keyboard.
-}
port mobileFocus : String -> Cmd msg


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias PagingInput =
    { max_id : String
    , since_id : String
    , min_id : String
    , limit : String
    }


emptyPagingInput : PagingInput
emptyPagingInput =
    { max_id = ""
    , since_id = ""
    , min_id = ""
    , limit = ""
    }


type PopupExplorer
    = NoPopupExplorer
    | ThreadPopupExplorer ThreadExplorerState


type Dialog
    = NoDialog
    | AlertDialog String
    | ConfirmDialog String String Msg
    | EditColumnsDialog
    | ServerDialog
    | PostDialog
    | SettingsDialog
    | KeyboardShortcutsDialog
    | SaveRestoreDialog


type Popup
    = NoPopup
    | UserNamePopup
    | GroupNamePopup
    | HashtagPopup
    | PostGroupPopup
    | PostTextPopup PostPopupSearch


type alias FocusInput =
    { x : String, y : String }


type alias FilterInput =
    { phrase : String
    , context : List FilterContext
    , irreversible : Bool
    , whole_word : Bool
    , expires_in : String
    }


emptyFilterInput : FilterInput
emptyFilterInput =
    { phrase = ""
    , context = [ HomeContext ]
    , irreversible = False
    , whole_word = False
    , expires_in = ""
    }


isFilterInputValid : FilterInput -> Bool
isFilterInputValid { phrase, context, expires_in } =
    (phrase /= "")
        && (context /= [])
        && ((expires_in == "")
                || (Nothing /= String.toInt expires_in)
           )


encodeFilterInput : FilterInput -> Value
encodeFilterInput { phrase, context, irreversible, whole_word, expires_in } =
    JE.object
        [ ( "phrase", JE.string phrase )
        , ( "context", JE.list ED.encodeFilterContext context )
        , ( "irreversible", JE.bool irreversible )
        , ( "whole_word", JE.bool whole_word )
        , ( "expires_in", JE.string expires_in )
        ]


filterInputDecoder : Decoder FilterInput
filterInputDecoder =
    JD.succeed FilterInput
        |> required "phrase" JD.string
        |> required "context" (JD.list ED.filterContextDecoder)
        |> required "irreversible" JD.bool
        |> required "whole_word" JD.bool
        |> required "expires_in" JD.string


type alias RenderEnv =
    { loginServer : Maybe String
    , style : Style
    , fontSizePct : Int
    , fontSize : String --percent
    , columnWidth : Int --pixels

    -- not persistent
    , emojis : Dict String Emoji
    , emojisList : List Emoji
    , windowSize : ( Int, Int )
    , here : Zone
    }


emptyRenderEnv : RenderEnv
emptyRenderEnv =
    { loginServer = Nothing
    , style = LightStyle
    , fontSizePct = 100
    , fontSize = "100"
    , columnWidth = 300
    , emojis = Dict.empty
    , emojisList = []
    , windowSize = ( 1024, 768 )
    , here = Time.utc
    }


type alias FeedEnv =
    { group : Maybe Group
    , list : Maybe ListEntity
    , references : Dict String Reference
    , missingReplyToAccountIds : Set String
    , headerHeight : Maybe Float
    }


emptyFeedEnv : FeedEnv
emptyFeedEnv =
    { group = Nothing
    , list = Nothing
    , references = Dict.empty
    , missingReplyToAccountIds = Set.empty
    , headerHeight = Nothing
    }


{-| server -> (feature -> available-p)
-}
type alias Features =
    Dict String (Dict String Bool)


type alias ScrollPillState =
    { showScrollPill : Bool
    , showServer : Bool
    }


initialScrollPillState : ScrollPillState
initialScrollPillState =
    { showScrollPill = True
    , showServer = True
    }


type Reference
    = ReferencedAccount Account
    | ReferencedMention Mention


type alias ReferenceDict =
    Dict String Reference


type PopupChoice
    = AccountChoice Account
    | GroupChoice Group
    | HashtagChoice String
    | PostGroupChoice Group
    | PostEmojiChoice Emoji
    | PostEmojiCharChoice EmojiChar


type alias Model =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , server : String

    -- Columns page state
    , feedSetDefinition : FeedSetDefinition
    , supportsAccountByUsername : Dict String Bool
    , postState : PostState
    , features : Features
    , scrollPillState : ScrollPillState
    , showLeftColumn : Bool

    -- API Explorer page state
    , prettify : Bool
    , selectedRequest : SelectedRequest
    , username : String
    , accountId : String
    , accountIds : String
    , showMetadata : Bool
    , q : String
    , resolve : Bool
    , following : Bool
    , groupId : String
    , showReceived : Bool
    , showEntity : Bool
    , whichGroups : WhichGroups
    , followReblogs : Bool
    , onlyMedia : Bool
    , pinned : Bool
    , excludeReplies : Bool
    , excludeReblogs : Bool
    , pagingInput : PagingInput
    , local : Bool
    , hashtag : String
    , listId : String
    , smartPaging : Bool
    , showJsonTree : Bool
    , showUpdateCredentials : Bool
    , statusId : String
    , useElmButtonNames : Bool
    , showPostStatus : Bool
    , excludedNotificationTypes : List NotificationType
    , notificationsAccountId : String
    , notificationId : String
    , muteNotifications : Bool
    , groupIds : String
    , offset : String
    , listTitle : String
    , filterId : String
    , filterInput : FilterInput
    , scheduledStatusId : String
    , userNameInput : String
    , accountInput : Maybe Account
    , groupNameInput : String
    , groupInput : Maybe Group
    , hashtagInput : String

    -- Non-persistent below here
    , initialPage : InitialPage
    , popupExplorer : PopupExplorer
    , dialog : Dialog
    , popup : Popup
    , popupElement : Maybe Dom.Element
    , popupChoices : List PopupChoice
    , searchActive : Bool
    , nextSearch : Cmd Msg
    , sideEffectCmd : Cmd Msg
    , feedSet : FeedSet
    , accountIdDict : Dict String (List AccountId)
    , dropZone : DropZone.Model
    , loadingFeeds : Set String --feed ids
    , newElements : Dict String FeedElements
    , groupDict : Dict String Group
    , feedEnvs : Dict String FeedEnv
    , showFullScrollPill : Bool
    , isTouchAware : Bool
    , bodyScroll : ScrollNotification
    , lastScroll : ( ScrollDirection, Posix )
    , featureProbeRequests : List ( String, Request )
    , movingColumn : Maybe FeedType
    , references : ReferenceDict
    , editColumnsMessage : Maybe String
    , feedsText : Maybe String
    , modelText : Maybe String
    , storageReads : Dict String (Maybe Value)
    , postDialogElement : Maybe Dom.Element
    , postTriggerCoordinatesCount : Int
    , postInputPosition : Int
    , postInputCount : Int
    , lists : List ListEntity
    , selectedList : Maybe ListEntity

    -- API Explorer state
    , keysDown : Set String
    , request : Maybe RawRequest
    , response : Maybe Value
    , entity : Maybe Entity
    , responseTree : Result JD.Error JsonTree.Node
    , responseState : JsonTree.State
    , entityTree : Result JD.Error JsonTree.Node
    , entityState : JsonTree.State
    , selectedKeyPath : JsonTree.KeyPath
    , selectedKeyValue : String
    , clipboardValue : String
    , clipboardCount : Int
    , metadata : Maybe Http.Metadata
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , tokens : Dict String String
    , account : Maybe Account
    , displayName : String
    , note : String
    , fields : List Field
    , avatarFile : Maybe File
    , headerFile : Maybe File
    , locked : Bool
    , privacy : Privacy
    , sensitive : Bool
    , language : String
    , isAccountFollowed : Bool
    , status : String
    , in_reply_to_id : String
    , quote_of_id : String
    , media_sensitive : Bool
    , spoiler_text : String
    , visibility : Maybe Visibility
    , scheduled_at : String
    , idempotencyKey : String
    , mediaFile : Maybe File
    , mediaDescription : String
    , mediaFocus : FocusInput
    , media_ids : String
    , media_id : String
    , expires_in : String
    , multiple : Bool
    , hide_totals : Bool
    , pollOptions : List String
    , groupTitle : String
    , groupDescription : String
    , groupCoverImage : Maybe File
    , reportComment : String
    , statusIds : String
    , forwardReport : Bool

    -- Not input state
    , msg : Maybe String
    , started : Started
    , funnelState : State
    , now : Posix
    , cmdQueue : List ( Int, Cmd Msg )
    }


type Page
    = HomePage
    | ColumnsPage
    | ExplorerPage


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GlobalMsg GlobalMsg
    | ColumnsUIMsg ColumnsUIMsg
    | ColumnsSendMsg ColumnsSendMsg
    | ExplorerUIMsg ExplorerUIMsg
    | ExplorerSendMsg ExplorerSendMsg
    | ScrollNotify Value
    | FocusNotify Bool
    | ApplyToModel (Model -> ( Model, Cmd Msg ))


type GlobalMsg
    = WindowResize Int Int
    | Here Zone
    | SetPage String
    | SetResponseState JsonTree.State
    | SetEntityState JsonTree.State
    | ExpandAll WhichJson
    | CollapseAll WhichJson
    | SelectTreeNode WhichJson JsonTree.KeyPath
    | SetDialog Dialog
    | OnKeyPress Bool String
    | SetServer String
    | Process Value
    | SetLoginServer
    | Login
    | Logout
    | ClearAllDialog
    | ClearAll
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | ReceiveFetchAccount (Result ( String, Error ) ( String, String, Account ))
    | ReceiveGetVerifyCredentials (Result Error Response)
    | ReceiveAccountIdRelationships Bool (Result Error Response)


type VerticalDirection
    = Up
    | Down


type ColumnsUIMsg
    = ColumnsUINoop
    | ResetFontSize
    | FontSize VerticalDirection
    | ColumnWidth VerticalDirection
    | ToggleStyle
    | ReloadAllColumns
    | MarkFeedRead FeedType
    | RefreshFeed FeedType
    | FeedRendered Value
    | Tick Posix
    | ShowEditColumnsDialog
    | ShowServerDialog
    | SimpleButtonMsg Button.Msg ColumnsUIMsg
    | ShowFullScrollPill
    | TimestampedCmd (Posix -> ColumnsUIMsg) Posix
    | ScrollPage ScrollDirection
    | ScrollPageAtTime ScrollDirection Posix
    | ClearFeatures
    | ShowPostDialog (Maybe Status)
    | PostWithMention String
    | PostWithGroupId String
    | ShowSettingsDialog
    | ShowKeyboardShortcutsDialog
    | ShowSaveRestoreDialog
    | DismissDialog
    | AddFeedColumn FeedType
    | DeleteFeedColumn FeedType
    | MoveFeedColumn FeedType
    | UserNameInput String
    | GroupNameInput String
    | HashtagInput String
    | SetSelectedList String
    | SendDelayedPopupRequest Popup String Request
    | SendDelayedPostTextPopup PostPopupSearch String
    | ReceivePostDialogElement (Result Dom.Error Dom.Element)
    | ReceiveCoordinates Coordinates
    | ReceivePopupElement (Result Dom.Error Dom.Element)
    | PopupChoose PopupChoice
    | OpenThreadExplorer Status
    | SaveThreadExplorerViewport ScrolledStatus (Result Dom.Error Dom.Viewport)
    | GetThreadPopupStatusViewport String
    | ScrollThreadPopupExplorer Float
    | GetThreadPopupExplorerHeaderHeight
    | ReceiveThreadPopupScrollInfo (Result Dom.Error ( Dom.Element, Dom.Element ))
    | ReceiveThreadExplorerHeaderElement (Result Dom.Error Dom.Element)
    | SetThreadExplorerStatus Status
    | ClosePopupExplorer
    | ReceiveHeaderElement String (Result Dom.Error Dom.Element)
    | ToggleStatusRepeat Status
    | ToggleStatusFavorite Status
    | StatusEllipsisDialog Status
    | ClearFeedsText
    | ClearModelText
    | SetFeedsText String
    | SetModelText String
    | PostGroupNameInput String
    | TogglePostGroup
    | SetPostText String
    | ChoosePostAttachment
    | PostAttachmentChosen File
    | PostDrop (DropZone.DropZoneMessage (List File))
    | PostAttachmentUrl String
    | DeletePostAttachment Int
    | TogglePostSensitive
    | ShowNewFeedStatuses FeedType
    | ShowStatusImages String
    | SetPostReplyType ReplyType
    | ClearPostStateReplyTo
    | Post
    | ProbeGroupsFeature String
    | ToggleShowLeftColumn
    | ToggleShowScrollPill
    | ToggleShowScrollPillServer


type ReceiveFeedType
    = ReceiveWholeFeed
    | ReceiveMoreFeed
    | ReceiveNewFeed


type ColumnsSendMsg
    = ColumnsSendNoop
    | ReceiveAccountByUsername (Maybe Paging) FeedType (Result Error Response)
    | ReceiveFeed Request (Maybe Paging) FeedType (Result Error Response)


type ExplorerUIMsg
    = SetWhichGroups String
    | ClearSentReceived
    | TogglePrettify
    | ToggleShowMetadata
    | ToggleShowReceived
    | ToggleShowEntity
    | SetQ String
    | ToggleResolve
    | ToggleFollowing
    | ToggleFollowReblogs
    | SetSelectedRequest SelectedRequest
    | SetUsername String
    | SetAccountId String
    | SetMaxId String
    | SetSinceId String
    | SetMinId String
    | SetLimit String
    | ToggleSmartPaging
    | ToggleShowJsonTree
    | ToggleUseElmButtonNames
    | ToggleShowUpdateCredentials
    | ToggleOnlyMedia
    | TogglePinned
    | ToggleExcludeReplies
    | ToggleExcludeReblogs
    | SetAccountIds String
    | SetDisplayName String
    | SetNote String
    | SetField Int Bool String
    | GetAvatarFile Bool
    | ReceiveAvatarFile File
    | GetHeaderFile Bool
    | ReceiveHeaderFile File
    | SetPrivacy Privacy
    | ToggleLocked
    | ToggleSensitive
    | SetLanguage String
    | SetFilterId String
    | ToggleFilterInputContext FilterContext
    | SetFilterPhrase String
    | ToggleFilterIrreversible
    | ToggleFilterWholeWord
    | SetFilterExpiresIn String
    | SetGroupId String
    | SetGroupIds String
    | SetOffset String
    | SetGroupTitle String
    | SetGroupDescription String
    | ReceiveGroupCoverImage File
    | GetGroupCoverImage Bool
    | SetReportCommentString String
    | SetStatusIds String
    | SetReportComment String
    | ToggleForwardReport
    | SetScheduledStatusId String
    | SetListTitle String
    | SetStatusId String
    | ToggleExcludedNotificationType NotificationType
    | IncludeAllNotifications
    | IncludeOnlyMentionNotifications
    | SetNotificationsAccountId String
    | SetNotificationId String
    | ToggleShowPostStatus
    | SetStatus String
    | SetInReplyToId String
    | SetInQuoteOfId String
    | ToggleMediaSensitive
    | SetSpoilerText String
    | SetVisibility (Maybe Visibility)
    | SetScheduledAt String
    | SetIdempotencyKey String
    | GetMediaFile Bool
    | ReceiveMediaFile File
    | SetMediaDescription String
    | SetMediaFocusX String
    | SetMediaFocusY String
    | SetMediaId String
    | SetMediaIds String
    | SetExpiresIn String
    | ToggleMultiple
    | ToggleHideTotals
    | RemovePollOption
    | AddPollOption
    | SetPollOption Int String
    | ToggleLocal
    | SetHashtag String
    | SetListId String
    | ToggleMuteNotifications


{-| If you add a message here, it should also be added to the
`buttonNameAlist` and `dollarButtonNameDict` values near the
bottom of the file.
-}
type ExplorerSendMsg
    = SendNothing
    | SendGetInstance
    | SendGetActivity
    | SendGetPeers
    | SendGetTrends
    | SendGetVerifyCredentials
    | SendGetAccountByUsername
    | SendGetAccount
    | SendGetFollowers
    | SendGetFollowing
    | SendGetStatuses
    | SendGetRelationships
    | SendGetScheduledStatuses
    | SendGetScheduledStatus
    | SendPutScheduledStatus
    | SendDeleteScheduledStatus
    | SendGetSearchAccounts
    | SendPostFollow
    | SendPostUnfollow
    | SendPatchUpdateCredentials
    | SendGetBlocks
    | SendPostBlock
    | SendPostUnblock
    | SendGetCustomEmojis
    | SendGetEndorsements
    | SendPostPinAccount
    | SendPostUnpinAccount
    | SendGetFavourites
    | SendPostFavourite
    | SendPostUnfavourite
    | SendGetFilters
    | SendGetFilter
    | SendPostFilter
    | SendPutFilter
    | SendDeleteFilter
    | SendGetFollowRequests
    | SendPostAuthorizeFollow
    | SendPostRejectFollow
    | SendGetFollowSuggestions
    | SendDeleteFollowSuggestions
    | SendGetGroups
    | SendGetGroupRemovedAccounts
    | SendGetGroup
    | SendGetGroupAccounts
    | SendGetGroupRelationships
    | SendPostGroupJoin
    | SendDeleteGroupJoin
    | SendPostGroupRemovedAccounts
    | SendDeleteGroupRemovedAccounts
    | SendPatchGroupAddAdministrator
    | SendDeleteGroupStatus
    | SendPostGroup
    | SendPutGroup
    | SendGetLists
    | SendGetList
    | SendGetListAccounts
    | SendGetAccountLists
    | SendPostList
    | SendPutList
    | SendDeleteList
    | SendPostListAccounts
    | SendDeleteListAccounts
    | SendGetAccountMutes
    | SendPostAccountMute
    | SendPostAccountUnmute
    | SendPostStatusMute
    | SendPostStatusUnmute
    | SendGetSearch
    | SendGetNotifications
    | SendGetNotification
    | SendPostDismissNotification
    | SendPostClearNotifications
    | SendPostReports
    | SendGetStatus
    | SendGetStatusContext
    | SendGetStatusCard
    | SendGetStatusRebloggedBy
    | SendGetStatusFavouritedBy
    | SendDeleteStatus
    | SendPostReblogStatus
    | SendPostUnreblogStatus
    | SendPostPinStatus
    | SendPostUnpinStatus
    | SendPostMedia
    | SendPutMedia
    | SendPostStatus
    | SendGetHomeTimeline
    | SendGetConversations
    | SendGetProTimeline
    | SendGetPublicTimeline
    | SendGetTagTimeline
    | SendGetListTimeline
    | SendGetGroupTimeline
      -- All of the `SendXXX` messages receive their results via `ReceiveResponse`.
    | ReceiveResponse Request (Result Error Response)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


keyMsgDict : Dict String ColumnsUIMsg
keyMsgDict =
    Dict.fromList
        [ ( "p", ShowPostDialog Nothing )
        , ( "r", ReloadAllColumns )
        , ( "R", ReloadAllColumns )
        , ( ".", ShowSettingsDialog )
        , ( "t", ToggleStyle )
        , ( ",", ShowSettingsDialog )
        , ( "?", ShowKeyboardShortcutsDialog )
        , ( "o", ShowSaveRestoreDialog )
        , ( "j", ScrollPage ScrollLeft )
        , ( "a", ScrollPage ScrollLeft )
        , ( "l", ScrollPage ScrollRight )
        , ( "d", ScrollPage ScrollRight )
        , ( "Escape", DismissDialog )
        ]


keyboard =
    { control = "Control"
    , alt = "Alt"
    , meta = "Meta"
    , escape = "Escape"
    , shift = "Shift"
    }


isKeyDown : String -> Model -> Bool
isKeyDown key model =
    Set.member key model.keysDown


isSpecialKeyDown : Model -> Bool
isSpecialKeyDown model =
    let
        keysDown =
            model.keysDown
    in
    Set.member keyboard.control keysDown
        || Set.member keyboard.alt keysDown
        || Set.member keyboard.meta keysDown


keyDecoder : Bool -> Decoder Msg
keyDecoder keyDown =
    JD.field "key" JD.string
        |> JD.map (GlobalMsg << OnKeyPress keyDown)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions (GlobalMsg << Process) model
        , Events.onResize (\w h -> GlobalMsg <| WindowResize w h)
        , scrollNotify ScrollNotify
        , focusNotify FocusNotify
        , Events.onKeyDown <| keyDecoder True
        , Events.onKeyUp <| keyDecoder False
        ]


emptyElement : String
emptyElement =
    "foo"


emptyUrl : Url
emptyUrl =
    { protocol = Url.Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/" ++ emptyElement
    , query = Nothing
    , fragment = Nothing
    }


type alias CodeErrorState =
    { code : Maybe String
    , error : Maybe String
    , state : Maybe String
    }


parseQuery : String -> CodeErrorState
parseQuery queryString =
    let
        url =
            { emptyUrl | query = Just queryString }

        qp =
            QP.map3 CodeErrorState
                (QP.string "code")
                (QP.string "error")
                (QP.string "state")
    in
    Parser.parse (Parser.s emptyElement <?> qp) url
        |> Maybe.withDefault (CodeErrorState Nothing Nothing Nothing)


type alias InitialPage =
    { page : Maybe Page
    , request : Maybe SelectedRequest
    }


emptyInitialPage : InitialPage
emptyInitialPage =
    InitialPage Nothing Nothing


parseInitialPage : Url -> InitialPage
parseInitialPage urlin =
    case urlin.query of
        Nothing ->
            emptyInitialPage

        Just queryString ->
            let
                url =
                    { emptyUrl | query = Just queryString }

                parser page request =
                    { page =
                        case page of
                            Nothing ->
                                Nothing

                            Just p ->
                                case p of
                                    "splash" ->
                                        Just HomePage

                                    "columns" ->
                                        Just ColumnsPage

                                    "api" ->
                                        Just ExplorerPage

                                    _ ->
                                        Nothing
                    , request =
                        case request of
                            Nothing ->
                                Nothing

                            Just r ->
                                Dict.get r selectedRequestFromUrlDict
                    }

                qp =
                    QP.map2 parser
                        (QP.string "page")
                        (QP.string "api")
            in
            Parser.parse (Parser.s emptyElement <?> qp) url
                |> Maybe.withDefault emptyInitialPage


type CodeAndState
    = CodeAndState String (Maybe String)
    | CodeErrorAndState String (Maybe String)
    | NoCode


{-| This recognizes `?code=<code>&state=<state>` or `?error=<error>&state=<state>`

in the URL from the redirect from authentication.

-}
receiveCodeAndState : Url -> CodeAndState
receiveCodeAndState url =
    case url.query of
        Nothing ->
            NoCode

        Just q ->
            case parseQuery q of
                { code, error, state } ->
                    case code of
                        Just cod ->
                            case state of
                                Just st ->
                                    CodeAndState cod state

                                Nothing ->
                                    CodeErrorAndState "Missing state with code" code

                        Nothing ->
                            case error of
                                Just err ->
                                    CodeErrorAndState err state

                                Nothing ->
                                    NoCode


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        hideClientId =
            case JD.decodeValue JD.bool value of
                Err _ ->
                    False

                Ok hide ->
                    hide

        ( code, state, msg ) =
            case receiveCodeAndState url of
                CodeAndState cod stat ->
                    ( Just cod, stat, Nothing )

                CodeErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                NoCode ->
                    ( Nothing, Nothing, Nothing )

        initialPage =
            Debug.log "initialPage" <| parseInitialPage url
    in
    { renderEnv = emptyRenderEnv
    , page = HomePage
    , token = Nothing
    , server = ""
    , feedSetDefinition = Types.emptyFeedSetDefinition
    , supportsAccountByUsername = Dict.empty
    , postState = initialPostState
    , features = Dict.empty
    , scrollPillState = initialScrollPillState
    , showLeftColumn = True
    , prettify = True
    , selectedRequest = LoginSelected
    , username = ""
    , accountId = ""
    , accountIds = ""
    , showMetadata = False
    , q = ""
    , resolve = False
    , following = False
    , groupId = ""
    , showReceived = True
    , showEntity = False
    , whichGroups = Request.MemberGroups
    , followReblogs = True
    , onlyMedia = False
    , pinned = False
    , excludeReplies = False
    , excludeReblogs = False
    , pagingInput = emptyPagingInput
    , local = False
    , hashtag = ""
    , listId = ""
    , smartPaging = False
    , showJsonTree = True
    , showUpdateCredentials = False
    , showPostStatus = False
    , statusId = ""
    , useElmButtonNames = False
    , excludedNotificationTypes = []
    , notificationsAccountId = ""
    , notificationId = ""
    , muteNotifications = True
    , groupIds = ""
    , offset = ""
    , listTitle = ""
    , filterId = ""
    , filterInput = emptyFilterInput
    , scheduledStatusId = ""
    , userNameInput = ""
    , accountInput = Nothing
    , groupNameInput = ""
    , groupInput = Nothing
    , hashtagInput = ""

    -- Non-persistent below here
    , initialPage = initialPage
    , popupExplorer = NoPopupExplorer
    , dialog = NoDialog
    , popup = NoPopup
    , popupElement = Nothing
    , popupChoices = []
    , searchActive = False
    , nextSearch = Cmd.none
    , sideEffectCmd = Cmd.none
    , feedSet = Types.emptyFeedSet
    , accountIdDict = Dict.empty
    , dropZone = DropZone.init
    , loadingFeeds = Set.empty
    , newElements = Dict.empty
    , groupDict = Dict.empty
    , feedEnvs = Dict.empty
    , showFullScrollPill = False
    , isTouchAware = False
    , bodyScroll = emptyScrollNotification
    , lastScroll = ( ScrollLeft, Time.millisToPosix 0 )
    , featureProbeRequests = []
    , movingColumn = Nothing
    , references = Dict.empty
    , editColumnsMessage = Nothing
    , feedsText = Nothing
    , modelText = Nothing
    , storageReads = Dict.empty
    , postDialogElement = Nothing
    , postTriggerCoordinatesCount = 0
    , postInputPosition = 0
    , postInputCount = 0
    , lists = []
    , selectedList = Nothing
    , keysDown = Set.empty
    , request = Nothing
    , response = Nothing
    , entity = Nothing
    , responseTree = emptyJsonTree
    , responseState = JsonTree.defaultState
    , entityTree = emptyJsonTree
    , entityState = JsonTree.defaultState
    , selectedKeyPath = ""
    , selectedKeyValue = ""
    , clipboardValue = ""
    , clipboardCount = 0
    , metadata = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , tokens = Dict.empty
    , account = Nothing
    , displayName = ""
    , note = ""
    , fields = []
    , avatarFile = Nothing
    , headerFile = Nothing
    , locked = False
    , privacy = PublicPrivacy
    , sensitive = False
    , language = ""
    , isAccountFollowed = False
    , status = ""
    , in_reply_to_id = ""
    , quote_of_id = ""
    , media_sensitive = False
    , spoiler_text = ""
    , visibility = Nothing
    , scheduled_at = ""
    , idempotencyKey = ""
    , mediaFile = Nothing
    , mediaDescription = ""
    , mediaFocus = { x = "", y = "" }
    , media_ids = ""
    , media_id = ""
    , expires_in = ""
    , multiple = False
    , hide_totals = False
    , pollOptions = [ "", "" ]
    , groupTitle = ""
    , groupDescription = ""
    , groupCoverImage = Nothing
    , reportComment = ""
    , statusIds = ""
    , forwardReport = True
    , msg = msg
    , started = NotStarted
    , funnelState = initialFunnelState
    , now = Time.millisToPosix 0
    , cmdQueue = []
    }
        -- As soon as the localStorage module reports in,
        -- we'll load the saved model,
        -- and then all the saved tokens.
        -- See `storageHandler` below, `get pk.model`.
        |> withCmds
            [ Navigation.replaceUrl key url.path
            , case ( code, state ) of
                ( Just cod, Just st ) ->
                    Login.getTokenTask { code = cod, state = st }
                        |> Task.attempt (GlobalMsg << ReceiveAuthorization)

                _ ->
                    Cmd.none
            , Task.perform getViewport Dom.getViewport
            , Task.perform (GlobalMsg << Here) Time.here
            , makeScrollRequestWithId "body" True
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp1 =
            Debug.log "viewport" viewport

        vp =
            vp1.viewport
    in
    GlobalMsg <| WindowResize (round vp.width) (round vp.height)


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch
                    [ get pk.model
                    , listKeysLabeled pk.token (pk.token ++ ".")
                    ]

            else
                Cmd.none

        lbl label =
            case label of
                Nothing ->
                    ""

                Just l ->
                    "[" ++ l ++ "] "
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            let
                mdl2 =
                    if
                        (label == Just pk.token)
                            || String.startsWith pk.accountIds key
                    then
                        -- Don't save tokens or accountIds
                        mdl

                    else
                        let
                            val =
                                if key /= pk.model then
                                    value

                                else
                                    case value of
                                        Nothing ->
                                            value

                                        Just v ->
                                            case JD.decodeValue savedModelDecoder v of
                                                Err _ ->
                                                    value

                                                Ok savedModel ->
                                                    encodeSavedModel
                                                        { savedModel | token = Nothing }
                                                        |> Just
                        in
                        { mdl
                            | storageReads =
                                Dict.insert ("get " ++ lbl label ++ key)
                                    val
                                    mdl.storageReads
                        }
            in
            handleGetResponse label key value mdl2

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            let
                keysv =
                    Just <| JE.list JE.string keys

                mdl2 =
                    { mdl
                        | storageReads =
                            Dict.insert ("list " ++ lbl label ++ prefix)
                                keysv
                                mdl.storageReads
                    }
            in
            handleListKeysResponse label prefix keys mdl2

        _ ->
            mdl |> withCmd cmd


getInstance : Model -> Cmd Msg
getInstance model =
    let
        serverInfo =
            { server = model.server
            , token = Nothing
            }
    in
    Request.serverRequest (\id -> GlobalMsg << ReceiveInstance)
        []
        serverInfo
        ()
        (InstanceRequest Request.GetInstance)


getVerifyCredentials : Model -> ( Model, Cmd Msg )
getVerifyCredentials model =
    case model.renderEnv.loginServer of
        Nothing ->
            model |> withNoCmd

        Just server ->
            let
                ( mdl, cmd ) =
                    mergeAccountId Types.emptyAccountId server model

                cmd2 =
                    case model.token of
                        Nothing ->
                            sendRequest (InstanceRequest Request.GetInstance) model
                                |> Tuple.second

                        Just token ->
                            Cmd.batch
                                [ checkAccountByUsername server mdl
                                , Request.serverRequest
                                    (\_ -> GlobalMsg << ReceiveGetVerifyCredentials)
                                    []
                                    { server = server
                                    , token = Just token
                                    }
                                    ()
                                  <|
                                    AccountsRequest Request.GetVerifyCredentials
                                ]
            in
            mdl |> withCmds [ cmd, cmd2 ]


handleListKeysResponse : Maybe String -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse maybeLabel prefix keys model =
    case maybeLabel of
        Nothing ->
            model |> withNoCmd

        Just label ->
            -- label will be pk.token,
            -- but we won't care about that until the value comes in
            -- to handleGetResponse below.
            model |> withCmds (List.map (getLabeled label) keys)


handleGetModel : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModel maybeValue model =
    let
        ( mdl, cmd ) =
            handleGetModelInternal maybeValue model

        { page, request } =
            mdl.initialPage
    in
    { mdl
        | page =
            case page of
                Nothing ->
                    if request == Nothing then
                        mdl.page

                    else
                        ExplorerPage

                Just p ->
                    p
        , selectedRequest =
            case request of
                Nothing ->
                    mdl.selectedRequest

                Just r ->
                    r
    }
        |> withCmds
            [ cmd
            , if mdl.page == HomePage then
                focusId LoginServerId

              else
                Cmd.none
            , case mdl.renderEnv.loginServer of
                Just server ->
                    getFeedSetDefinition server

                _ ->
                    Cmd.none
            ]


handleGetModelInternal : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModelInternal maybeValue model =
    case maybeValue of
        Nothing ->
            { model
                | started = Started
                , msg = Nothing
            }
                |> withNoCmd

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model
                        | started = Started
                        , msg =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> withNoCmd

                Ok savedModel ->
                    let
                        mdl =
                            savedModelToModel savedModel model

                        mdl2 =
                            { mdl
                                | started = Started
                                , msg = Nothing
                            }
                    in
                    case mdl2.renderEnv.loginServer of
                        Nothing ->
                            ( mdl2
                            , Task.perform (GlobalMsg << SetServer) <|
                                Task.succeed mdl2.server
                            )

                        Just server ->
                            let
                                ( mdl3, cmd3 ) =
                                    sendCustomEmojisRequest mdl2

                                ( mdl4, cmd4 ) =
                                    getVerifyCredentials mdl3

                                ( mdl5, cmd5 ) =
                                    sendRequest (ListsRequest Request.GetLists) mdl4
                            in
                            mdl5 |> withCmds [ cmd3, cmd4, cmd5, fetchFeatures server mdl3 ]


sendCustomEmojisRequest : Model -> ( Model, Cmd Msg )
sendCustomEmojisRequest model =
    sendRequest (CustomEmojisRequest Request.GetCustomEmojis)
        model


handleGetFeedSetDefinition : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetFeedSetDefinition maybeValue model =
    let
        ( feedSetDefinition, mdl, getGroupsCmd ) =
            case maybeValue of
                Nothing ->
                    ( Types.defaultFeedSetDefinition, model, Cmd.none )

                Just value ->
                    case JD.decodeValue MED.feedSetDefinitionDecoder value of
                        Err _ ->
                            ( Types.defaultFeedSetDefinition, model, Cmd.none )

                        Ok fsd ->
                            let
                                folder : FeedType -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                                folder feedType ( mdl2, cmd2 ) =
                                    case feedType of
                                        GroupFeed group_id ->
                                            let
                                                ( mdl3, cmd3 ) =
                                                    maybeLoadGroup group_id mdl2
                                            in
                                            mdl3 |> withCmds [ cmd2, cmd3 ]

                                        _ ->
                                            mdl2 |> withCmd cmd2

                                ( mdl4, cmd4 ) =
                                    List.foldl folder ( model, Cmd.none ) fsd.feedTypes
                            in
                            ( fsd, mdl4, cmd4 )

        mdl5 =
            List.foldl addFeedEnv model feedSetDefinition.feedTypes

        feedSet =
            Types.feedSetDefinitionToFeedSet feedSetDefinition

        reloadCmd =
            if model.page == ColumnsPage then
                Task.perform ColumnsUIMsg <|
                    Task.succeed ReloadAllColumns

            else
                Cmd.none
    in
    { mdl
        | feedSetDefinition = Debug.log "handleGetFeedSetDefinition" feedSetDefinition
        , feedSet = feedSet
    }
        |> withCmds [ getGroupsCmd, reloadCmd ]


getFeedEnv : FeedType -> Model -> FeedEnv
getFeedEnv feedType model =
    let
        feedId =
            Types.feedID feedType
    in
    case Dict.get feedId model.feedEnvs of
        Just feedEnv ->
            feedEnv

        Nothing ->
            emptyFeedEnv


addFeedEnv : FeedType -> Model -> Model
addFeedEnv feedType model =
    let
        feedId =
            Types.feedID feedType
    in
    case feedType of
        GroupFeed group_id ->
            case getGroup group_id model of
                Nothing ->
                    model

                Just group ->
                    let
                        feedEnv =
                            { emptyFeedEnv | group = Just group }
                    in
                    { model
                        | feedEnvs =
                            Dict.insert feedId feedEnv model.feedEnvs
                    }

        ListFeed list_id ->
            case LE.find (\list -> list_id == list.id) model.lists of
                Nothing ->
                    model

                Just list ->
                    let
                        feedEnv =
                            { emptyFeedEnv | list = Just list }
                    in
                    { model
                        | feedEnvs =
                            Dict.insert feedId feedEnv model.feedEnvs
                    }

        _ ->
            model


removeFeedEnv : FeedType -> Model -> Model
removeFeedEnv feedType model =
    let
        feedId =
            Types.feedID feedType
    in
    { model | feedEnvs = Dict.remove feedId model.feedEnvs }


handleGetToken : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetToken key value model =
    case JD.decodeValue JD.string value of
        Err err ->
            let
                ignore =
                    Debug.log ("Error decoding " ++ key) err
            in
            model |> withNoCmd

        Ok token ->
            let
                tokens =
                    model.tokens

                server =
                    Debug.log "Received token for server" <|
                        tokenStorageKeyServer key
            in
            { model | tokens = Dict.insert server token tokens }
                |> withCmd (fetchFeatures server model)


pkAccountIdsLength : Int
pkAccountIdsLength =
    String.length pk.accountIds


pkFeedSetDefinitionLength : Int
pkFeedSetDefinitionLength =
    String.length pk.feedSetDefinition


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if Debug.log "handleGetResponse, key" key == pk.model then
                handleGetModel maybeValue model

            else if pk.feedSetDefinition == String.left pkFeedSetDefinitionLength key then
                handleGetFeedSetDefinition maybeValue model

            else if pk.accountIds == String.left pkAccountIdsLength key then
                handleGetAccountIds key maybeValue model

            else
                model |> withNoCmd

        Just label ->
            case maybeValue of
                Nothing ->
                    model |> withNoCmd

                Just value ->
                    if label == pk.token then
                        handleGetToken key value model

                    else
                        model |> withNoCmd


handleGetAccountIds : String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetAccountIds key maybeValue model =
    let
        server =
            String.dropLeft (pkAccountIdsLength + 1) key

        maybeMerge : Dict String (List AccountId) -> Model -> ( Model, Cmd Msg )
        maybeMerge dict mdl =
            case Dict.get server dict of
                Just [ acctId ] ->
                    mergeAccountId acctId server mdl

                _ ->
                    mdl |> withNoCmd
    in
    case maybeValue of
        Nothing ->
            maybeMerge model.accountIdDict model

        Just value ->
            case JD.decodeValue MED.accountIdsDecoder value of
                Err _ ->
                    maybeMerge model.accountIdDict model

                Ok accountIds ->
                    maybeMerge model.accountIdDict
                        { model
                            | accountIdDict =
                                Dict.insert server accountIds model.accountIdDict
                        }


socketHandler : WebSocket.Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        WebSocket.ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (WebSocket.ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | msg = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse received ->
            model |> withNoCmd

        WebSocket.ClosedResponse { expected, reason } ->
            model
                |> withNoCmd

        WebSocket.ConnectedResponse _ ->
            model |> withNoCmd

        _ ->
            model |> withNoCmd


emptyJsonTree : Result JD.Error JsonTree.Node
emptyJsonTree =
    JsonTree.parseString "[]"


updateJsonTrees : Model -> Model
updateJsonTrees model =
    let
        parse : Maybe Value -> ( Result JD.Error JsonTree.Node, JsonTree.State )
        parse value =
            case value of
                Nothing ->
                    ( emptyJsonTree, JsonTree.defaultState )

                Just v ->
                    let
                        result =
                            JsonTree.parseValue v
                    in
                    ( result
                    , case result of
                        Err _ ->
                            JsonTree.defaultState

                        Ok root ->
                            JsonTree.collapseToDepth 1 root JsonTree.defaultState
                    )

        ( responseTree, responseState ) =
            parse model.response

        ( entityTree, entityState ) =
            case model.entity of
                Nothing ->
                    parse Nothing

                Just entity ->
                    parse (Just <| ED.encodeEntity entity)
    in
    { model
        | responseTree = responseTree
        , responseState = responseState
        , entityTree = entityTree
        , entityState = entityState
        , selectedKeyPath = ""
        , selectedKeyValue = ""
    }


updatePatchCredentialsInputs : Model -> Model
updatePatchCredentialsInputs mdl =
    let
        model =
            updateJsonTrees mdl
    in
    case model.account of
        Nothing ->
            { model
                | displayName = ""
                , note = ""
                , locked = False
                , privacy = PublicPrivacy
                , sensitive = False
                , language = ""
            }

        Just account ->
            let
                ( ( privacy, sensitive, language ), ( note, fields ) ) =
                    case account.source of
                        Nothing ->
                            ( ( PublicPrivacy, False, "" )
                            , ( "", [] )
                            )

                        Just source ->
                            ( ( source.privacy
                              , source.sensitive
                              , Maybe.withDefault "" source.language
                              )
                            , ( source.note, source.fields )
                            )

                locked =
                    account.locked
            in
            { model
                | displayName = account.display_name
                , note = note
                , fields = extendFields fields
                , locked = locked
                , privacy = privacy
                , sensitive = sensitive
                , language = language
            }


extendFields : List Field -> List Field
extendFields fields =
    List.concat
        [ fields
        , List.repeat (4 - List.length fields) emptyField
        ]


emptyField : Field
emptyField =
    { name = ""
    , value = ""
    , verified_at = Nothing
    }


imageMimeTypes : List String
imageMimeTypes =
    [ "image/jpeg"
    , "image/gif"
    , "image/png"
    ]


nothingIfBlank : String -> Maybe String
nothingIfBlank x =
    if x == "" then
        Nothing

    else
        Just x


pagingInputToPaging : PagingInput -> Maybe Paging
pagingInputToPaging pagingInput =
    let
        { max_id, since_id, min_id, limit } =
            pagingInput

        ilimit =
            Maybe.withDefault -1 <| String.toInt limit
    in
    if max_id == "" && since_id == "" && min_id == "" && ilimit < 0 then
        Nothing

    else
        Just
            { max_id = nothingIfBlank max_id
            , since_id = nothingIfBlank since_id
            , min_id = nothingIfBlank min_id
            , limit =
                if ilimit < 0 then
                    Nothing

                else
                    Just ilimit
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model2, cmd ) =
            updateInternal msg model

        savedModel =
            modelToSavedModel model2

        needsSaving =
            if model2.started /= Started then
                False

            else
                case model2.savedModel of
                    Nothing ->
                        True

                    Just sm ->
                        savedModel /= sm
    in
    { model2
        | savedModel =
            if needsSaving then
                Just savedModel

            else
                model2.savedModel
    }
        |> withCmds
            [ cmd
            , if
                (model2.page == ExplorerPage)
                    && ((model.page /= ExplorerPage)
                            || (model2.selectedRequest /= model.selectedRequest)
                       )
              then
                let
                    query =
                        Builder.string "api" <|
                            selectedRequestToUrlValue model2.selectedRequest
                in
                Navigation.replaceUrl model.key <|
                    model.url.path
                        ++ Builder.toQuery [ query ]

              else if
                (model2.page /= ExplorerPage)
                    && (model.page == ExplorerPage)
              then
                Navigation.replaceUrl model.key model.url.path

              else
                Cmd.none
            , if needsSaving then
                put pk.model (Just <| encodeSavedModel savedModel)

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal _ ->
                    model |> withNoCmd

                External url ->
                    model |> withCmd (openWindow <| JE.string url)

        OnUrlChange url ->
            model |> withNoCmd

        GlobalMsg m ->
            globalMsg m model

        ColumnsUIMsg m ->
            columnsUIMsg m model

        ColumnsSendMsg m ->
            columnsSendMsg m model

        ExplorerUIMsg m ->
            explorerUIMsg m model

        ExplorerSendMsg m ->
            explorerSendMsg m model

        ScrollNotify value ->
            processScroll value model

        FocusNotify focused ->
            (if focused then
                { model | keysDown = Set.empty }

             else
                model
            )
                |> withNoCmd

        ApplyToModel f ->
            f model


scrollNotificationDecoder : Decoder ScrollNotification
scrollNotificationDecoder =
    JD.succeed ScrollNotification
        |> required "id" JD.string
        |> required "scrollLeft" JD.float
        |> required "scrollTop" JD.float
        |> required "scrollWidth" JD.float
        |> required "scrollHeight" JD.float
        |> required "clientWidth" JD.float
        |> required "clientHeight" JD.float


emptyScrollNotification : ScrollNotification
emptyScrollNotification =
    { id = ""
    , scrollLeft = 0
    , scrollTop = 0
    , scrollWidth = 0
    , scrollHeight = 0
    , clientWidth = 0
    , clientHeight = 0
    }


processScroll : Value -> Model -> ( Model, Cmd Msg )
processScroll value model =
    case JD.decodeValue scrollNotificationDecoder value of
        Err _ ->
            model |> withNoCmd

        Ok notification ->
            let
                id =
                    notification.id
            in
            if id == "body" then
                { model | bodyScroll = notification }
                    |> withNoCmd

            else if Set.member id model.loadingFeeds then
                model |> withNoCmd

            else
                let
                    scrollHeight =
                        notification.scrollHeight

                    clientHeight =
                        notification.clientHeight

                    overhang =
                        scrollHeight
                            - (notification.scrollTop + clientHeight)
                in
                if clientHeight / 4 <= overhang then
                    model |> withNoCmd

                else
                    loadMoreCmd id model


{-| Process global messages.
-}
globalMsg : GlobalMsg -> Model -> ( Model, Cmd Msg )
globalMsg msg model =
    let
        renderEnv =
            model.renderEnv
    in
    case msg of
        WindowResize w h ->
            { model
                | renderEnv =
                    { renderEnv | windowSize = Debug.log "windowSize" ( w, h ) }
            }
                |> withNoCmd

        Here zone ->
            { model
                | renderEnv =
                    { renderEnv | here = zone }
            }
                |> withNoCmd

        SetPage pageString ->
            let
                page =
                    stringToPage pageString
            in
            { model
                | page = page
                , dialog = NoDialog
                , popup = NoPopup
            }
                |> withCmd
                    (if page == ColumnsPage then
                        Cmd.batch
                            [ if feedsNeedLoading model then
                                Task.perform ColumnsUIMsg <|
                                    Task.succeed ReloadAllColumns

                              else
                                Cmd.none
                            ]

                     else if page == HomePage then
                        focusId LoginServerId

                     else
                        Cmd.none
                    )

        SetResponseState state ->
            { model | responseState = state }
                |> withNoCmd

        SetEntityState state ->
            { model | entityState = state }
                |> withNoCmd

        ExpandAll whichJson ->
            (case whichJson of
                ResponseJson ->
                    { model
                        | responseState =
                            JsonTree.expandAll model.responseState
                    }

                DecodedJson ->
                    { model
                        | entityState =
                            JsonTree.expandAll model.entityState
                    }
            )
                |> withNoCmd

        CollapseAll whichJson ->
            (case whichJson of
                ResponseJson ->
                    case model.responseTree of
                        Ok root ->
                            { model
                                | responseState =
                                    JsonTree.collapseToDepth 1 root model.responseState
                            }

                        Err _ ->
                            model

                DecodedJson ->
                    case model.entityTree of
                        Ok root ->
                            { model
                                | entityState =
                                    JsonTree.collapseToDepth 1 root model.entityState
                            }

                        Err _ ->
                            model
            )
                |> withNoCmd

        SelectTreeNode whichJson keyPath ->
            let
                result =
                    case whichJson of
                        ResponseJson ->
                            model.responseTree

                        DecodedJson ->
                            model.entityTree

                value =
                    case result of
                        Err _ ->
                            ""

                        Ok root ->
                            case findKeyPath keyPath root of
                                Nothing ->
                                    ""

                                Just taggedValue ->
                                    taggedValueToString taggedValue
            in
            { model
                | selectedKeyPath = keyPath
                , selectedKeyValue = value
                , clipboardValue = value
                , clipboardCount = model.clipboardCount + 1
            }
                |> withNoCmd

        SetDialog dialog ->
            { model
                | dialog = dialog
                , movingColumn = Nothing
            }
                |> withCmd
                    (if model.dialog == NoDialog then
                        Cmd.none

                     else
                        focusId CancelButtonId
                    )

        OnKeyPress isDown key ->
            let
                mdl =
                    { model
                        | keysDown =
                            if isDown then
                                Set.insert key model.keysDown

                            else
                                Set.remove key model.keysDown
                    }
            in
            mdl
                |> withCmd
                    (if
                        not isDown
                            || ((key /= keyboard.escape)
                                    && (isSpecialKeyDown mdl
                                            || (model.dialog /= NoDialog)
                                       )
                               )
                            || (model.page /= ColumnsPage)
                     then
                        Cmd.none

                     else
                        case Dict.get key keyMsgDict of
                            Nothing ->
                                Cmd.none

                            Just cmd ->
                                Task.perform ColumnsUIMsg <| Task.succeed cmd
                    )

        SetServer server ->
            let
                mdl =
                    { model | server = server }
            in
            mdl
                |> withCmd
                    (if String.contains "." server then
                        getInstance mdl

                     else
                        Cmd.none
                    )

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok res ->
                    res

        SetLoginServer ->
            if model.server == "" then
                { model
                    | msg = Nothing
                    , renderEnv =
                        { renderEnv | loginServer = Nothing }
                    , request = Nothing
                    , response = Nothing
                    , entity = Nothing
                    , metadata = Nothing
                    , selectedKeyPath = ""
                    , selectedKeyValue = ""
                }
                    |> withNoCmd

            else
                let
                    server =
                        model.server

                    mdl =
                        { model
                            | renderEnv =
                                { renderEnv | loginServer = Just server }
                            , token = Nothing
                            , account = Nothing
                        }
                            |> updatePatchCredentialsInputs

                    ( mdl2, cmd ) =
                        sendRequest (InstanceRequest Request.GetInstance) mdl

                    acctIdsCmd =
                        case Dict.get server model.accountIdDict of
                            Nothing ->
                                getAccountIds server

                            _ ->
                                Cmd.none
                in
                mdl2
                    |> withCmds
                        [ acctIdsCmd
                        , cmd
                        ]

        Login ->
            let
                url =
                    model.url

                sau =
                    { client_name = "mammudeck"
                    , server = model.server
                    , applicationUri =
                        { url
                            | fragment = Nothing
                            , query = Nothing
                        }
                            |> Url.toString
                    }

                mdl =
                    { model | dialog = NoDialog }
            in
            case Login.loginTask sau <| Dict.get model.server model.tokens of
                Redirect task ->
                    ( mdl, Task.attempt (GlobalMsg << ReceiveRedirect) task )

                FetchAccount task ->
                    ( mdl, Task.attempt (GlobalMsg << ReceiveFetchAccount) task )

        Logout ->
            case model.renderEnv.loginServer of
                Nothing ->
                    model |> withNoCmd

                Just server ->
                    { model
                        | server = server
                        , account = Nothing
                        , tokens = Dict.remove server model.tokens
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , metadata = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
                        |> withCmd (putToken server Nothing)

        ClearAllDialog ->
            model
                |> withCmd
                    (Task.perform (GlobalMsg << SetDialog) <|
                        Task.succeed
                            (ConfirmDialog
                                "Do you really want to erase everything?"
                                "Erase"
                                (GlobalMsg ClearAll)
                            )
                    )

        ClearAll ->
            let
                mdl =
                    { model
                        | dialog = NoDialog
                        , popup = NoPopup
                        , editColumnsMessage = Nothing
                        , tokens = Dict.empty
                        , server = ""
                        , renderEnv =
                            { renderEnv | loginServer = Nothing }
                        , account = Nothing
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , metadata = Nothing
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
            in
            { mdl | savedModel = Just <| modelToSavedModel mdl }
                |> withCmd clear

        ReceiveRedirect result ->
            case result of
                Err ( server, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, app, cmd ) ->
                    { model | msg = Nothing }
                        |> withCmds [ cmd, fetchFeatures server model ]

        ReceiveAuthorization result ->
            case result of
                Err ( server, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, authorization, account ) ->
                    let
                        ( mdl, cmd ) =
                            saveAuthorization server authorization model

                        serverInfo =
                            { server = server
                            , token = Just authorization.token
                            }

                        mdl2 =
                            { mdl
                                | msg = Nothing
                                , page = switchHomeToColumns model.page
                                , token = Just authorization.token
                                , renderEnv =
                                    { renderEnv | loginServer = Just server }
                                , account = Just account
                                , request =
                                    -- Fake the request
                                    Just <|
                                        Request.requestToRawRequest []
                                            serverInfo
                                            (AccountsRequest Request.GetVerifyCredentials)
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updatePatchCredentialsInputs

                        accountId =
                            Types.accountToAccountId account

                        ( mdl3, cmd3 ) =
                            mergeAccountId accountId server mdl2

                        ( mdl4, cmd4 ) =
                            sendCustomEmojisRequest mdl3

                        ( mdl5, cmd5 ) =
                            sendRequest (ListsRequest Request.GetLists) mdl4
                    in
                    mdl5
                        |> withCmds
                            [ cmd
                            , cmd3
                            , cmd4
                            , cmd5
                            , checkAccountByUsername server mdl4
                            , getAccountIdRelationships False mdl4
                            , getFeedSetDefinition server
                            , fetchFeatures server mdl4
                            ]

        ReceiveFetchAccount result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok ( loginServer, token, account ) ->
                    let
                        serverInfo =
                            { server = loginServer
                            , token = Just token
                            }

                        request =
                            -- Fake the request
                            Request.requestToRawRequest []
                                serverInfo
                                (AccountsRequest Request.GetVerifyCredentials)

                        mdl =
                            { model
                                | msg = Nothing
                                , page = switchHomeToColumns model.page
                                , server = loginServer
                                , renderEnv =
                                    { renderEnv | loginServer = Just loginServer }
                                , token = Just token
                                , account = Just account
                                , request = Just request
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updatePatchCredentialsInputs

                        ( mdl2, cmd2 ) =
                            let
                                accountId =
                                    Types.accountToAccountId account
                            in
                            mergeAccountId accountId loginServer mdl

                        ( mdl3, cmd3 ) =
                            sendCustomEmojisRequest mdl2
                    in
                    mdl3
                        |> withCmds
                            [ cmd2
                            , cmd3
                            , checkAccountByUsername loginServer mdl3
                            , getAccountIdRelationships False mdl3
                            , getFeedSetDefinition loginServer
                            , fetchFeatures loginServer mdl3
                            ]

        ReceiveInstance result ->
            case result of
                Err _ ->
                    -- We'll get lots of errors, for non-existant domains
                    model |> withNoCmd

                Ok response ->
                    case response.entity of
                        InstanceEntity instance ->
                            { model
                                | msg = Nothing
                                , request = Just response.rawRequest
                                , metadata = Just response.metadata
                                , response = Just instance.v
                                , entity = Just response.entity
                            }
                                |> updateJsonTrees
                                |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveGetVerifyCredentials result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok response ->
                    case response.entity of
                        AccountEntity account ->
                            let
                                mdl =
                                    { model
                                        | msg = Nothing
                                        , request = Just response.rawRequest
                                        , metadata = Just response.metadata
                                        , response = Just account.v
                                        , entity = Just response.entity
                                        , account = Just account
                                    }
                                        |> updatePatchCredentialsInputs

                                accountId =
                                    Types.accountToAccountId account

                                ( mdl2, mergeIdCmd ) =
                                    case model.renderEnv.loginServer of
                                        Nothing ->
                                            ( mdl, Cmd.none )

                                        Just server ->
                                            mergeAccountId accountId server mdl
                            in
                            mdl2
                                |> withCmds
                                    [ getAccountIdRelationships False mdl
                                    , mergeIdCmd
                                    ]

                        _ ->
                            model |> withNoCmd

        ReceiveAccountIdRelationships showResult result ->
            case result of
                Err _ ->
                    ( if showResult then
                        { model
                            | metadata = Nothing
                            , request = Nothing
                            , response = Nothing
                            , entity = Nothing
                            , selectedKeyPath = ""
                            , selectedKeyValue = ""
                        }

                      else
                        model
                    , Cmd.none
                    )

                Ok response ->
                    case response.entity of
                        RelationshipListEntity relationships ->
                            case
                                LE.find (\r -> r.id == model.accountId)
                                    relationships
                            of
                                Nothing ->
                                    model |> withNoCmd

                                Just relationship ->
                                    -- Maybe we should handle blocked_by
                                    let
                                        mdl =
                                            if not showResult then
                                                model

                                            else
                                                { model
                                                    | metadata =
                                                        Just response.metadata
                                                    , request =
                                                        Just response.rawRequest
                                                    , response =
                                                        Just relationship.v
                                                    , entity =
                                                        Just response.entity
                                                }
                                                    |> updateJsonTrees
                                    in
                                    { mdl
                                        | isAccountFollowed =
                                            relationship.following
                                    }
                                        |> withNoCmd

                        _ ->
                            model |> withNoCmd


switchHomeToColumns : Page -> Page
switchHomeToColumns page =
    if page == HomePage then
        ColumnsPage

    else
        page


checkAccountByUsername : String -> Model -> Cmd Msg
checkAccountByUsername server model =
    case Dict.get server model.supportsAccountByUsername of
        Nothing ->
            Task.perform ExplorerSendMsg <| Task.succeed SendGetAccountByUsername

        Just _ ->
            Cmd.none


featureNames =
    { groups = "groups"
    , quote = "quote"
    , proFeed = "proFeed"
    }


{-| Fetch features if we don't know them already.

Currently, the only feature is the existence of groups.

-}
fetchFeatures : String -> Model -> Cmd Msg
fetchFeatures server model =
    case LE.find (\( s, _ ) -> s == server) model.featureProbeRequests of
        Just _ ->
            Cmd.none

        Nothing ->
            case serverKnowsFeature (Just server) featureNames.groups model of
                Just _ ->
                    Cmd.none

                Nothing ->
                    Task.perform ColumnsUIMsg <|
                        (Task.succeed <|
                            ProbeGroupsFeature
                                (Debug.log "ProbeGroupsFeature" server)
                        )


{-| Merge account into server's accountIdDict entry.

Write if it changed. Read if it started empty.

-}
mergeAccountId : AccountId -> String -> Model -> ( Model, Cmd Msg )
mergeAccountId accountId server model =
    let
        id =
            accountId.id
    in
    case Dict.get server model.accountIdDict of
        Nothing ->
            { model
                | accountIdDict =
                    Dict.insert server [ accountId ] model.accountIdDict
            }
                |> withCmd (getAccountIds server)

        Just accountIds ->
            let
                putNew acctIds =
                    { model
                        | accountIdDict =
                            Dict.insert server acctIds model.accountIdDict
                    }
                        |> withCmd (putAccountIds server acctIds)
            in
            case LE.find (.id >> (==) id) accountIds of
                Nothing ->
                    putNew <| accountId :: accountIds

                Just acctId ->
                    if acctId == accountId then
                        let
                            putIdsCmd =
                                case accountIds of
                                    [ _ ] ->
                                        putAccountIds server accountIds

                                    _ ->
                                        Cmd.none
                        in
                        model |> withCmd putIdsCmd

                    else
                        putNew <| accountId :: LE.remove acctId accountIds


findFeed : FeedType -> FeedSet -> Maybe Feed
findFeed feedType feedSet =
    LE.find (\feed -> feedType == feed.feedType) feedSet.feeds


replaceFeed : Feed -> FeedSet -> FeedSet
replaceFeed feed feedSet =
    { feedSet
        | feeds =
            LE.updateIf (\f -> feed.feedType == f.feedType)
                (\_ -> feed)
                feedSet.feeds
    }


loadMoreCmd : String -> Model -> ( Model, Cmd Msg )
loadMoreCmd id model =
    case Types.feedIdToType id of
        Nothing ->
            model |> withNoCmd

        Just feedType ->
            case findFeed feedType model.feedSet of
                Nothing ->
                    model |> withNoCmd

                Just feed ->
                    extendFeed id feed model


extendFeed : String -> Feed -> Model -> ( Model, Cmd Msg )
extendFeed feedId feed model =
    let
        id =
            case feed.elements of
                StatusElements statuses ->
                    case LE.last statuses of
                        Nothing ->
                            ""

                        Just status ->
                            status.id

                NotificationElements notifications ->
                    case LE.last notifications of
                        Nothing ->
                            ""

                        Just notification ->
                            notification.id

                ConversationsElements conversations ->
                    case LE.last conversations of
                        Nothing ->
                            ""

                        Just conversation ->
                            conversation.id

                _ ->
                    ""
    in
    if id == "" then
        model |> withNoCmd

    else
        let
            ( mdl, cmd ) =
                reloadFeedPaging
                    (Just { emptyPaging | max_id = Just id })
                    feed
                    model
        in
        markFeedRead feed.feedType mdl
            |> Tuple.first
            |> withCmd cmd


feedsNeedLoading : Model -> Bool
feedsNeedLoading model =
    (model.account /= Nothing)
        && feedSetIsEmpty model.feedSet


feedSetIsEmpty : FeedSet -> Bool
feedSetIsEmpty feedSet =
    List.map feedLength feedSet.feeds
        |> List.filter ((/=) 0)
        |> (==) []


feedLength : Feed -> Int
feedLength feed =
    case feed.elements of
        StatusElements list ->
            List.length list

        NotificationElements list ->
            List.length list

        AccountElements list ->
            List.length list

        ConversationsElements list ->
            List.length list

        ResultsElements list ->
            List.length list


{-| Send a request to the JS listening on the scrollRequest port.

One of these is kept active for each column displayed.

-}
makeScrollRequest : FeedType -> Bool -> Cmd Msg
makeScrollRequest feedType enable =
    makeScrollRequestWithId (Types.feedID feedType) enable


makeScrollRequestWithId : String -> Bool -> Cmd Msg
makeScrollRequestWithId id enable =
    JE.object
        [ ( "id", JE.string id )
        , ( "enable", JE.bool enable )
        ]
        |> scrollRequest


secondFeedElementId : FeedElements -> Maybe String
secondFeedElementId elements =
    case elements of
        StatusElements statuses ->
            case statuses of
                _ :: s :: _ ->
                    Just s.id

                _ ->
                    Nothing

        NotificationElements notifications ->
            case notifications of
                _ :: n :: _ ->
                    Just n.id

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Process UI messages from the columns page.

These change the Model, but don't send anything over the wire to any instances.

-}
columnsUIMsg : ColumnsUIMsg -> Model -> ( Model, Cmd Msg )
columnsUIMsg msg model =
    let
        renderEnv =
            model.renderEnv
    in
    case msg of
        ColumnsUINoop ->
            model |> withNoCmd

        ResetFontSize ->
            { model
                | renderEnv =
                    { renderEnv
                        | fontSizePct = 100
                        , fontSize = "100"
                    }
            }
                |> withNoCmd

        FontSize direction ->
            let
                delta =
                    case direction of
                        Up ->
                            5

                        Down ->
                            -5

                fontSizePct =
                    max 50 renderEnv.fontSizePct + delta
            in
            { model
                | renderEnv =
                    { renderEnv
                        | fontSizePct = fontSizePct
                        , fontSize =
                            fontSizePct |> String.fromInt
                    }
            }
                |> withNoCmd

        ColumnWidth direction ->
            -- pixels -> column count -> + delta -> pixels
            let
                columnCount =
                    List.length model.feedSet.feeds

                delta =
                    case direction of
                        Up ->
                            -1.0

                        Down ->
                            1.0

                leftColWid =
                    0

                columnWidth =
                    renderEnv.columnWidth |> toFloat

                windowWidth =
                    -- The 10 is the padding in the inner div in renderCenteredScreen
                    -- subtracted 2 because that works.
                    (renderEnv.windowSize |> Tuple.first) - 8 - leftColWid |> toFloat

                colMargin =
                    -- 1 should be 0, but works.
                    columnsBorderSpacing + 1 |> toFloat

                cols =
                    (windowWidth / (columnWidth + colMargin))
                        + delta
                        |> (case direction of
                                Up ->
                                    \x -> floor <| x + 0.9

                                Down ->
                                    \x -> ceiling <| x - 0.9
                           )
                        |> max 1
                        |> toFloat

                newColumnWidth =
                    (windowWidth / cols - colMargin)
                        |> round
                        |> max 100
            in
            { model
                | renderEnv =
                    { renderEnv
                        | columnWidth = newColumnWidth
                    }
            }
                |> withNoCmd

        ToggleStyle ->
            { model
                | renderEnv =
                    { renderEnv
                        | style =
                            if renderEnv.style == LightStyle then
                                DarkStyle

                            else
                                LightStyle
                    }
            }
                |> withNoCmd

        ReloadAllColumns ->
            let
                getFeed : Feed -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                getFeed feed ( mdl, cmds ) =
                    let
                        ( mdl2, cmd ) =
                            columnsUIMsg (RefreshFeed feed.feedType) mdl
                    in
                    ( mdl2, Cmd.batch [ cmd, cmds ] )

                ( mdl3, cmd3 ) =
                    dismissDialog model

                ( mdl4, cmd4 ) =
                    List.foldl getFeed ( mdl3, Cmd.none ) mdl3.feedSet.feeds
            in
            mdl4 |> withCmds [ cmd3, cmd4 ]

        MarkFeedRead feedType ->
            markFeedRead feedType model

        RefreshFeed feedType ->
            case findFeed feedType model.feedSet of
                Nothing ->
                    model |> withNoCmd

                Just feed ->
                    case secondFeedElementId feed.elements of
                        Nothing ->
                            reloadFeed feed model

                        justId ->
                            if isKeyDown keyboard.shift model then
                                reloadFeed feed model

                            else
                                reloadFeedPaging
                                    (Just { emptyPaging | since_id = justId })
                                    feed
                                    model

        FeedRendered v ->
            case JD.decodeValue JD.string v of
                Err _ ->
                    model |> withNoCmd

                Ok id ->
                    model
                        |> withCmds
                            [ makeScrollRequestWithId id True
                            , Task.attempt (ColumnsUIMsg << ReceiveHeaderElement id)
                                (Dom.getElement <| headerFeedId id)
                            ]

        Tick now ->
            let
                millis =
                    Time.posixToMillis now

                folder qe ( cmds, queue ) =
                    let
                        ( time, cmd ) =
                            qe
                    in
                    if time <= millis then
                        ( Cmd.batch [ cmd, cmds ], queue )

                    else
                        ( cmds, qe :: queue )

                ( cmds2, queue2 ) =
                    List.foldl folder ( Cmd.none, [] ) model.cmdQueue
            in
            { model | now = now }
                |> withCmd cmds2

        ShowEditColumnsDialog ->
            { model | dialog = EditColumnsDialog }
                |> sendRequest (ListsRequest Request.GetLists)

        ShowServerDialog ->
            { model | dialog = ServerDialog }
                |> withCmd (focusId LoginServerId)

        TimestampedCmd wrapper now ->
            columnsUIMsg (wrapper now) model

        ScrollPage direction ->
            model |> withCmd (timestampCmd <| ScrollPageAtTime direction)

        ScrollPageAtTime direction now ->
            scrollPage direction now model

        ShowFullScrollPill ->
            { model | showFullScrollPill = True } |> withNoCmd

        SimpleButtonMsg m cuiMsg ->
            let
                ( isClick, but, _ ) =
                    Button.update (\bm -> SimpleButtonMsg bm cuiMsg)
                        m
                        simpleButton

                mdl =
                    { model | isTouchAware = Button.isTouchAware but }
            in
            if isClick then
                { model | showFullScrollPill = False }
                    -- 1 millisecond to collapse the scroll pill
                    |> withCmd
                        (Delay.after 1 Delay.Millisecond <|
                            ColumnsUIMsg cuiMsg
                        )

            else
                model |> withNoCmd

        ClearFeatures ->
            { model | features = Dict.empty } |> withNoCmd

        ShowPostDialog maybeStatus ->
            let
                postState =
                    model.postState

                me =
                    case model.account of
                        Nothing ->
                            ""

                        Just account ->
                            account.username

                ( ( replyTo, replyType ), ( groupName, group_id ) ) =
                    case maybeStatus of
                        Nothing ->
                            ( ( postState.replyTo, postState.replyType )
                            , ( postState.groupName, postState.group_id )
                            )

                        Just status ->
                            ( ( maybeStatus, ReplyToPost )
                            , case status.group of
                                Nothing ->
                                    ( postState.groupName, postState.group_id )

                                Just group ->
                                    ( group.title, Nothing )
                            )
            in
            { model
                | dialog = PostDialog
                , postState =
                    { postState
                        | replyTo = replyTo
                        , replyType = replyType
                        , groupName = groupName
                        , group_id = group_id
                    }
                        |> addPostStateMentions me
            }
                |> withCmds
                    [ focusId PostDialogTextId
                    , Task.attempt (ColumnsUIMsg << ReceivePostDialogElement)
                        (Dom.getElement nodeIds.postDialogText)
                    ]

        PostWithMention nameAtServer ->
            let
                postState =
                    model.postState
            in
            columnsUIMsg (ShowPostDialog Nothing)
                { model
                    | postState =
                        { postState
                            | text =
                                "@" ++ nameAtServer ++ " " ++ postState.text
                        }
                }

        PostWithGroupId group_id ->
            let
                postState =
                    model.postState

                groupName =
                    case Dict.get group_id model.groupDict of
                        Just group ->
                            group.title

                        Nothing ->
                            "Group ID " ++ group_id
            in
            columnsUIMsg (ShowPostDialog Nothing)
                { model
                    | postState =
                        { postState
                            | groupName = groupName
                            , group_id = Just group_id
                        }
                }

        ShowSettingsDialog ->
            { model | dialog = SettingsDialog }
                |> withNoCmd

        ShowKeyboardShortcutsDialog ->
            { model | dialog = KeyboardShortcutsDialog }
                |> withNoCmd

        ShowSaveRestoreDialog ->
            { model | dialog = SaveRestoreDialog }
                |> withNoCmd

        DismissDialog ->
            dismissDialog model

        AddFeedColumn feedType ->
            addFeedType (fillinFeedType feedType model) model

        DeleteFeedColumn feedType ->
            deleteFeedType feedType model

        MoveFeedColumn feedType ->
            case model.movingColumn of
                Nothing ->
                    { model
                        | movingColumn = Just feedType
                        , editColumnsMessage = Nothing
                    }
                        |> withNoCmd

                Just movingFeedType ->
                    moveFeedType movingFeedType feedType model

        UserNameInput userNameInput ->
            { model
                | userNameInput = userNameInput
                , accountInput = Nothing
                , popupChoices =
                    if userNameInput == "" then
                        []

                    else
                        model.popupChoices
            }
                |> initializePopup UserNamePopup userNameInput

        GroupNameInput groupNameInput ->
            { model
                | groupNameInput = groupNameInput
                , groupInput = Nothing
                , popupChoices =
                    if groupNameInput == "" then
                        []

                    else
                        model.popupChoices
            }
                |> initializePopup GroupNamePopup groupNameInput

        HashtagInput hashtagInput ->
            { model
                | hashtagInput = hashtagInput
                , popupChoices =
                    if hashtagInput == "" then
                        []

                    else
                        model.popupChoices
            }
                |> initializePopup HashtagPopup hashtagInput

        SetSelectedList id ->
            { model
                | selectedList =
                    LE.find (\list -> id == list.id) model.lists
            }
                |> withNoCmd

        SendDelayedPopupRequest popup input request ->
            sendDelayedPopupRequest popup input request model

        SendDelayedPostTextPopup search postText ->
            sendDelayedPostTextPopup search postText model

        ReceivePostDialogElement result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok element ->
                    { model | postDialogElement = Just element }
                        |> withNoCmd

        ReceiveCoordinates coordinates ->
            if coordinates.selectionStart /= coordinates.selectionEnd then
                model |> withNoCmd

            else
                case model.popup of
                    PostTextPopup _ ->
                        let
                            { top, left, lineheight } =
                                coordinates.caretCoordinates
                        in
                        case model.postDialogElement of
                            Nothing ->
                                model |> withNoCmd

                            Just element ->
                                let
                                    elel =
                                        element.element
                                in
                                { model
                                    | popupElement =
                                        Just
                                            { element
                                                | element =
                                                    { elel
                                                        | x =
                                                            toFloat left
                                                                + elel.x
                                                        , y =
                                                            toFloat top
                                                                + elel.y
                                                        , height =
                                                            Maybe.withDefault 20
                                                                lineheight
                                                                |> toFloat
                                                    }
                                            }
                                }
                                    |> withNoCmd

                    _ ->
                        model |> withNoCmd

        ReceivePopupElement result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok element ->
                    { model | popupElement = Just element }
                        |> withNoCmd

        PopupChoose choice ->
            popupChoose choice model

        OpenThreadExplorer status ->
            openThreadExplorer status model

        SaveThreadExplorerViewport scrolledStatus result ->
            saveThreadExplorerViewport scrolledStatus result model

        GetThreadPopupStatusViewport id ->
            let
                task =
                    Dom.getElement id
                        |> Task.andThen
                            (\element ->
                                Dom.getElement
                                    nodeIds.threadExplorerStatusDiv
                                    |> Task.andThen
                                        (\scrollElement ->
                                            Task.succeed
                                                ( element, scrollElement )
                                        )
                            )
            in
            model
                |> withCmds
                    [ Task.attempt (ColumnsUIMsg << ReceiveThreadPopupScrollInfo)
                        task
                    , Task.perform ColumnsUIMsg <|
                        Task.succeed GetThreadPopupExplorerHeaderHeight
                    ]

        ScrollThreadPopupExplorer scroll ->
            model
                |> withCmd
                    (Dom.setViewportOf nodeIds.threadExplorerStatusDiv 0 scroll
                        |> Task.attempt (\_ -> Noop)
                    )

        GetThreadPopupExplorerHeaderHeight ->
            model
                |> withCmd
                    (Dom.getElement nodeIds.threadExplorerHeader
                        |> Task.attempt
                            (ColumnsUIMsg << ReceiveThreadExplorerHeaderElement)
                    )

        ReceiveThreadPopupScrollInfo result ->
            receiveThreadPopupScrollInfo result model

        ReceiveThreadExplorerHeaderElement result ->
            receiveThreadExplorerHeaderElement result model

        SetThreadExplorerStatus status ->
            setThreadExplorerStatus status model

        ClosePopupExplorer ->
            { model | popupExplorer = NoPopupExplorer }
                |> withNoCmd

        ReceiveHeaderElement id result ->
            let
                height =
                    case result of
                        Err _ ->
                            Nothing

                        Ok element ->
                            Just element.element.height

                feedEnv =
                    case Dict.get id model.feedEnvs of
                        Nothing ->
                            emptyFeedEnv

                        Just env ->
                            env

                mdl =
                    if feedEnv.headerHeight == height then
                        model

                    else
                        { model
                            | feedEnvs =
                                Dict.insert id
                                    { feedEnv | headerHeight = height }
                                    model.feedEnvs
                        }
            in
            mdl |> withNoCmd

        ToggleStatusRepeat status ->
            let
                mes =
                    if status.reblogged then
                        SendPostUnreblogStatus

                    else
                        SendPostReblogStatus

                mdl =
                    { model | statusId = status.id }
            in
            explorerSendMsg mes mdl

        ToggleStatusFavorite status ->
            let
                mes =
                    if status.favourited then
                        SendPostUnfavourite

                    else
                        SendPostFavourite

                mdl =
                    { model | statusId = status.id }
            in
            explorerSendMsg mes mdl

        StatusEllipsisDialog status ->
            { model
                | dialog =
                    AlertDialog "Ellipsis Dialog not yet supported."
            }
                |> withNoCmd

        ClearFeedsText ->
            { model | feedsText = Nothing }
                |> withNoCmd

        ClearModelText ->
            { model | modelText = Nothing }
                |> withNoCmd

        SetFeedsText feedsText ->
            { model | feedsText = Just feedsText }
                |> withNoCmd

        SetModelText modelText ->
            { model | modelText = Just modelText }
                |> withNoCmd

        PostGroupNameInput groupName ->
            let
                postState =
                    model.postState
            in
            { model
                | postState =
                    { postState
                        | groupName = groupName
                        , group_id = Nothing
                    }
                , popupChoices =
                    if groupName == "" then
                        []

                    else
                        model.popupChoices
            }
                |> initializePopup PostGroupPopup groupName

        TogglePostGroup ->
            let
                postState =
                    model.postState

                ( groupName, group_id, clearPopup ) =
                    case postState.group_id of
                        Nothing ->
                            case findGroup postState.groupName model of
                                Nothing ->
                                    ( postState.groupName, Nothing, False )

                                Just group ->
                                    ( group.title, Just group.id, True )

                        Just _ ->
                            ( postState.groupName, Nothing, True )

                mdl =
                    if not clearPopup then
                        model

                    else
                        { model
                            | popup = NoPopup
                            , popupElement = Nothing
                        }
            in
            { mdl
                | postState =
                    { postState
                        | groupName = groupName
                        , group_id = group_id
                    }
            }
                |> withNoCmd

        SetPostText string ->
            let
                postState =
                    model.postState
            in
            { model
                | postState =
                    { postState | text = string }
            }
                |> initializePostTextPopup postState.text string

        ChoosePostAttachment ->
            model
                |> withCmd
                    (File.Select.file imageMimeTypes
                        (ColumnsUIMsg << PostAttachmentChosen)
                    )

        PostAttachmentChosen file ->
            let
                postState =
                    model.postState

                ( mdl, cmd ) =
                    { model
                        | postState =
                            { postState
                                | fileNames =
                                    postState.fileNames
                                        ++ [ File.name file ]
                            }
                    }
                        |> sendRequest
                            (MediaAttachmentsRequest <|
                                Request.PostMedia
                                    { file = file
                                    , description = Nothing
                                    , focus = Nothing
                                    }
                            )
            in
            mdl
                |> withCmds
                    [ cmd
                    , Task.perform (ColumnsUIMsg << PostAttachmentUrl) <|
                        File.toUrl file
                    ]

        PostDrop dzmsg ->
            let
                dropZone =
                    model.dropZone

                mdl =
                    { model | dropZone = DropZone.update dzmsg dropZone }
            in
            case dzmsg of
                DropZone.Drop files ->
                    mdl
                        |> withCmds
                            (List.map
                                (\file ->
                                    Task.perform ColumnsUIMsg
                                        (Task.succeed <| PostAttachmentChosen file)
                                )
                                files
                            )

                _ ->
                    mdl |> withNoCmd

        PostAttachmentUrl url ->
            let
                postState =
                    model.postState
            in
            { model
                | postState =
                    { postState
                        | fileUrls = postState.fileUrls ++ [ url ]
                    }
            }
                |> withNoCmd

        DeletePostAttachment index ->
            let
                postState =
                    model.postState
            in
            { model
                | postState =
                    { postState
                        | media_ids = LE.removeAt index postState.media_ids
                        , fileNames = LE.removeAt index postState.fileNames
                        , fileUrls = LE.removeAt index postState.fileUrls
                    }
            }
                |> withNoCmd

        TogglePostSensitive ->
            let
                postState =
                    model.postState
            in
            { model
                | postState =
                    { postState
                        | sensitive = not postState.sensitive
                    }
            }
                |> withNoCmd

        ShowNewFeedStatuses feedType ->
            -- TODO
            model |> withNoCmd

        ShowStatusImages id ->
            modifyColumnsStatus id (\s -> { s | sensitive = False }) model
                |> withNoCmd

        SetPostReplyType replyType ->
            let
                postState =
                    model.postState
            in
            { model
                | postState =
                    { postState | replyType = replyType }
            }
                |> withNoCmd

        ClearPostStateReplyTo ->
            let
                postState =
                    model.postState

                text =
                    if postState.text == postState.mentionsString then
                        ""

                    else
                        postState.text
            in
            { model
                | postState =
                    { postState
                        | replyTo = Nothing
                        , replyType = NoReply
                        , text = text
                        , mentionsString = ""
                    }
            }
                |> withNoCmd

        Post ->
            let
                postState =
                    model.postState

                replyType =
                    postState.replyType

                replyTo =
                    case replyType of
                        NoReply ->
                            Nothing

                        _ ->
                            postState.replyTo

                ( in_reply_to_id, quote_id ) =
                    case replyTo of
                        Nothing ->
                            ( Nothing, Nothing )

                        Just status ->
                            case replyType of
                                ReplyToPost ->
                                    ( Just status.id, Nothing )

                                _ ->
                                    ( Nothing, Just status.id )

                post =
                    { status = nothingIfBlank postState.text
                    , in_reply_to_id = in_reply_to_id
                    , group_id = postState.group_id
                    , quote_of_id = quote_id
                    , media_ids = postState.media_ids
                    , poll = Nothing
                    , sensitive = postState.sensitive
                    , spoiler_text = Nothing
                    , visibility = Just PublicVisibility
                    , scheduled_at = Nothing
                    , language = Nothing
                    , idempotencyKey = Nothing
                    }
            in
            sendRequest
                (StatusesRequest <| Request.PostStatus post)
                model

        ProbeGroupsFeature server ->
            probeGroupsFeature server model

        ToggleShowLeftColumn ->
            { model | showLeftColumn = not model.showLeftColumn }
                |> withNoCmd

        ToggleShowScrollPill ->
            let
                scrollPillState =
                    model.scrollPillState
            in
            { model
                | scrollPillState =
                    { scrollPillState
                        | showScrollPill = not scrollPillState.showScrollPill
                    }
            }
                |> withNoCmd

        ToggleShowScrollPillServer ->
            let
                scrollPillState =
                    model.scrollPillState
            in
            { model
                | scrollPillState =
                    { scrollPillState
                        | showServer = not scrollPillState.showServer
                    }
            }
                |> withNoCmd


markFeedRead : FeedType -> Model -> ( Model, Cmd Msg )
markFeedRead feedType model =
    let
        feedSet =
            model.feedSet

        feeds =
            List.map
                (\feed ->
                    if
                        (feedType == feed.feedType)
                            && (feed.newElements > 0)
                    then
                        { feed | newElements = 0 }

                    else
                        feed
                )
                feedSet.feeds
    in
    { model
        | feedSet =
            { feedSet | feeds = feeds }
    }
        |> withNoCmd


receiveThreadPopupScrollInfo : Result Dom.Error ( Dom.Element, Dom.Element ) -> Model -> ( Model, Cmd Msg )
receiveThreadPopupScrollInfo result model =
    case result of
        Err _ ->
            model |> withNoCmd

        Ok ( { element }, scrollElement ) ->
            let
                height =
                    scrollElement.element.height

                y =
                    scrollElement.element.y

                oneThird =
                    min (height / 3) element.height
            in
            if height - (element.y - y) < oneThird then
                let
                    scroll =
                        element.y - y - (height - oneThird)

                    task =
                        Dom.setViewportOf nodeIds.threadExplorerStatusDiv
                            0
                            scroll
                in
                model
                    |> withCmd
                        (Task.attempt (\_ -> Noop) task)

            else
                model |> withNoCmd


receiveThreadExplorerHeaderElement : Result Dom.Error Dom.Element -> Model -> ( Model, Cmd Msg )
receiveThreadExplorerHeaderElement result model =
    case model.popupExplorer of
        ThreadPopupExplorer state ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok { element } ->
                    { model
                        | popupExplorer =
                            ThreadPopupExplorer
                                { state
                                    | headerHeight = Debug.log "receive headerHeight" <| Just element.height
                                }
                    }
                        |> withNoCmd

        _ ->
            model |> withNoCmd


setThreadExplorerStatus : Status -> Model -> ( Model, Cmd Msg )
setThreadExplorerStatus status model =
    case model.popupExplorer of
        ThreadPopupExplorer state ->
            let
                id =
                    status.id
            in
            case LE.findIndex (\ss -> id == ss.status.id) state.ribbon of
                Nothing ->
                    -- May have gone to an ancestor of the initial status
                    case List.head state.ribbon of
                        Nothing ->
                            model |> withNoCmd

                        Just ss ->
                            case LE.find (\s -> id == s.id) ss.displayed of
                                Nothing ->
                                    model |> withNoCmd

                                Just _ ->
                                    openThreadExplorer status
                                        { model | popupExplorer = NoPopupExplorer }

                Just idx ->
                    let
                        newState =
                            { state
                                | ribbon =
                                    List.drop idx state.ribbon
                            }
                    in
                    newThreadPopupExplorer newState model

        _ ->
            model |> withNoCmd


type alias ScrolledStatus =
    { status : Status
    , visited : Set String
    , displayed : List Status
    , scroll : Maybe Float
    }


makeScrolledStatus status =
    { status = status
    , visited = Set.empty
    , displayed = [ status ]
    , scroll = Nothing
    }


type alias ThreadExplorerState =
    { ribbon : List ScrolledStatus
    , headerHeight : Maybe Float
    }


emptyThreadExplorerState : ThreadExplorerState
emptyThreadExplorerState =
    { ribbon = []
    , headerHeight = Nothing
    }


openThreadExplorer : Status -> Model -> ( Model, Cmd Msg )
openThreadExplorer status model =
    let
        id =
            status.id

        sendTheRequest headerHeight mdl =
            newThreadPopupExplorer
                { ribbon = [ makeScrolledStatus status ]
                , headerHeight = headerHeight
                }
                mdl
    in
    case model.popupExplorer of
        ThreadPopupExplorer { ribbon, headerHeight } ->
            case ribbon of
                [] ->
                    sendTheRequest headerHeight model

                scrolledStatus :: tail ->
                    let
                        displayed =
                            scrolledStatus.displayed
                    in
                    case LE.findIndex (\ss -> id == ss.status.id) tail of
                        Just idx ->
                            openThreadExplorer status
                                { model
                                    | popupExplorer =
                                        ThreadPopupExplorer
                                            { ribbon = List.drop idx tail
                                            , headerHeight = headerHeight
                                            }
                                }

                        Nothing ->
                            let
                                sss =
                                    scrolledStatus.status

                                containsReplyTo s slist =
                                    case s.in_reply_to_id of
                                        Nothing ->
                                            False

                                        Just rtid ->
                                            List.any (\s2 -> rtid == s2.id) slist
                            in
                            if id == sss.id then
                                newThreadPopupExplorer
                                    { ribbon =
                                        { scrolledStatus | status = status }
                                            :: tail
                                    , headerHeight = headerHeight
                                    }
                                    model

                            else
                                case LE.find (\s -> id == s.id) displayed of
                                    Nothing ->
                                        sendTheRequest headerHeight model

                                    Just _ ->
                                        -- We're pushing a new ribbon entry.
                                        -- Need to get the old one's scroll position.
                                        ( model
                                        , Dom.getViewportOf
                                            nodeIds.threadExplorerStatusDiv
                                            |> Task.attempt
                                                (ColumnsUIMsg
                                                    << SaveThreadExplorerViewport
                                                        (makeScrolledStatus status)
                                                )
                                        )

        _ ->
            sendTheRequest Nothing model


newThreadPopupExplorer : ThreadExplorerState -> Model -> ( Model, Cmd Msg )
newThreadPopupExplorer state model =
    case state.ribbon of
        [] ->
            { model | popupExplorer = NoPopupExplorer }
                |> withNoCmd

        { status, scroll } :: _ ->
            let
                ( mdl, cmd ) =
                    ( { model
                        | popupExplorer =
                            ThreadPopupExplorer state
                      }
                    , case scroll of
                        Nothing ->
                            Cmd.none

                        Just s ->
                            delayedCmd 1 <|
                                ColumnsUIMsg <|
                                    ScrollThreadPopupExplorer s
                    )

                ( mdl2, cmd2 ) =
                    sendRequest
                        (StatusesRequest <|
                            Request.GetStatusContext { id = status.id }
                        )
                        mdl
            in
            mdl2 |> withCmds [ cmd, cmd2 ]


saveThreadExplorerViewport : ScrolledStatus -> Result Dom.Error Dom.Viewport -> Model -> ( Model, Cmd Msg )
saveThreadExplorerViewport scrolledStatus result model =
    let
        res state =
            let
                ribbon =
                    case state.ribbon of
                        [] ->
                            []

                        ss :: tail ->
                            { ss
                                | visited =
                                    Debug.log "saveThreadExplorerViewport" <|
                                        Set.insert scrolledStatus.status.id
                                            ss.visited
                            }
                                :: tail
            in
            newThreadPopupExplorer
                { state | ribbon = scrolledStatus :: ribbon }
                model
    in
    case model.popupExplorer of
        ThreadPopupExplorer state ->
            case state.ribbon of
                [] ->
                    res state

                ss :: tail ->
                    case result of
                        Err _ ->
                            res state

                        Ok { viewport } ->
                            res
                                { state
                                    | ribbon =
                                        { ss | scroll = Just viewport.y } :: tail
                                }

        _ ->
            res emptyThreadExplorerState


{-| Called after we receive a ContextEntity.
-}
updateThreadExplorer : String -> Entity -> Model -> Model
updateThreadExplorer contextId entity model =
    case model.popupExplorer of
        ThreadPopupExplorer state ->
            case state.ribbon of
                [] ->
                    model

                scrolledStatus :: rest ->
                    case entity of
                        ContextEntity context ->
                            updateThreadExplorerInternal
                                scrolledStatus
                                state.headerHeight
                                context
                                rest
                                model

                        _ ->
                            model

        _ ->
            model


updateThreadExplorerInternal : ScrolledStatus -> Maybe Float -> Context -> List ScrolledStatus -> Model -> Model
updateThreadExplorerInternal scrolledStatus headerHeight context ribbonTail model =
    let
        references =
            addStatusesReferences context.ancestors model.references
                |> addStatusesReferences context.descendants

        mdl =
            { model | references = references }

        status =
            scrolledStatus.status
    in
    let
        justId =
            Just status.id

        scrolledStatus2 =
            { scrolledStatus
                | displayed =
                    List.concat
                        [ context.ancestors
                        , [ status ]
                        , if isReplyChain context.descendants then
                            context.descendants

                          else
                            List.filter (\s -> justId == s.in_reply_to_id)
                                context.descendants
                        ]
            }

        newState =
            { ribbon = scrolledStatus2 :: ribbonTail
            , headerHeight = headerHeight
            }

        idx =
            List.length context.ancestors + 1

        nodeid =
            threadExplorerStatusId idx
    in
    { mdl
        | popupExplorer = ThreadPopupExplorer newState
        , sideEffectCmd =
            if idx > 1 && scrolledStatus2.scroll == Nothing then
                delayedCmd 499 <|
                    ColumnsUIMsg <|
                        GetThreadPopupStatusViewport nodeid

            else
                Cmd.batch
                    [ case scrolledStatus2.scroll of
                        Nothing ->
                            Cmd.none

                        Just scroll ->
                            delayedCmd 1 <|
                                ColumnsUIMsg <|
                                    ScrollThreadPopupExplorer scroll
                    , delayedCmd 1 <|
                        ColumnsUIMsg GetThreadPopupExplorerHeaderHeight
                    ]
    }


isReplyChain : List Status -> Bool
isReplyChain statuses =
    let
        loop : Status -> List Status -> Bool
        loop s tail =
            case tail of
                [] ->
                    True

                ts :: rest ->
                    if ts.in_reply_to_id == Just s.id then
                        loop ts rest

                    else
                        False
    in
    case statuses of
        [] ->
            False

        status :: tail ->
            if tail == [] then
                False

            else
                loop status tail


{-| This implements the scroll keys/scroll-pill for the Thread Explorer Popup.,
-}
scrollThreadExplorer : ThreadExplorerState -> Bool -> ScrollDirection -> Model -> ( Model, Cmd Msg )
scrollThreadExplorer state allTheWay direction model =
    if Debug.log "scrollThreadExplorer" allTheWay then
        let
            ribbon =
                List.drop (List.length state.ribbon - 1) state.ribbon
        in
        case List.head ribbon of
            Nothing ->
                model |> withNoCmd

            Just { displayed } ->
                case List.head displayed of
                    Nothing ->
                        model |> withNoCmd

                    Just status ->
                        openThreadExplorer status
                            { model | popupExplorer = NoPopupExplorer }

    else
        let
            ( maybeStatus, state2 ) =
                getScrolledThreadExplorerStatus direction state
        in
        case maybeStatus of
            Nothing ->
                model |> withNoCmd

            Just status ->
                if state2.ribbon == [] then
                    openThreadExplorer status
                        { model | popupExplorer = NoPopupExplorer }

                else
                    let
                        mdl =
                            { model
                                | popupExplorer = ThreadPopupExplorer state2
                            }
                    in
                    case direction of
                        ScrollLeft ->
                            setThreadExplorerStatus status mdl

                        ScrollRight ->
                            openThreadExplorer status mdl


afterLastVisited : Set String -> List Status -> List Status
afterLastVisited visited statuses =
    let
        sesutats =
            List.reverse statuses
    in
    case LE.findIndex (\s -> Set.member s.id visited) sesutats of
        Nothing ->
            statuses

        Just idx ->
            List.take idx sesutats
                |> List.reverse


getScrolledThreadExplorerStatus : ScrollDirection -> ThreadExplorerState -> ( Maybe Status, ThreadExplorerState )
getScrolledThreadExplorerStatus direction state =
    case state.ribbon of
        [] ->
            ( Nothing, state )

        ss :: tail ->
            case direction of
                ScrollLeft ->
                    case tail of
                        [] ->
                            case ss.status.in_reply_to_id of
                                Nothing ->
                                    case List.head ss.displayed of
                                        Nothing ->
                                            ( Nothing, state )

                                        Just status ->
                                            ( Just status
                                            , { state | ribbon = [] }
                                            )

                                Just rtid ->
                                    case LE.find (\s -> rtid == s.id) ss.displayed of
                                        Nothing ->
                                            -- Only happens on initial post
                                            ( Nothing, state )

                                        Just rt ->
                                            ( Just rt, { state | ribbon = [] } )

                        ss2 :: _ ->
                            ( Just ss2.status, { state | ribbon = tail } )

                ScrollRight ->
                    let
                        outer : ScrolledStatus -> List ScrolledStatus -> ( Maybe Status, List ScrolledStatus )
                        outer scrolledStatus scrolledStatusses =
                            let
                                displayed =
                                    scrolledStatus.displayed

                                id =
                                    scrolledStatus.status.id
                            in
                            case LE.findIndex (\s -> id == s.id) displayed of
                                Nothing ->
                                    ( Nothing, [] )

                                Just idx ->
                                    case
                                        inner
                                            (List.drop (idx + 1) displayed
                                                |> afterLastVisited
                                                    scrolledStatus.visited
                                            )
                                    of
                                        Just s ->
                                            ( Just s, scrolledStatus :: scrolledStatusses )

                                        Nothing ->
                                            case scrolledStatusses of
                                                [] ->
                                                    case List.head displayed of
                                                        Nothing ->
                                                            ( Nothing, [] )

                                                        Just s ->
                                                            ( Just s
                                                            , [ { scrolledStatus
                                                                    | visited =
                                                                        Set.empty
                                                                }
                                                              ]
                                                            )

                                                ss2 :: rest ->
                                                    ( Just ss2.status
                                                    , scrolledStatusses
                                                    )

                        inner : List Status -> Maybe Status
                        inner statusses =
                            case statusses of
                                [] ->
                                    Nothing

                                status :: rest ->
                                    case status.replies_count of
                                        0 ->
                                            inner rest

                                        1 ->
                                            case rest of
                                                [] ->
                                                    Just status

                                                s :: _ ->
                                                    if
                                                        s.in_reply_to_id
                                                            == Just status.id
                                                    then
                                                        Nothing

                                                    else
                                                        Just status

                                        _ ->
                                            Just status

                        ( maybeStatus, sss ) =
                            outer ss tail
                    in
                    ( maybeStatus
                    , { state | ribbon = sss }
                    )


findGroup : String -> Model -> Maybe Group
findGroup groupName model =
    let
        lcname =
            String.toLower groupName
    in
    case
        LE.find
            (\( _, group ) -> lcname == String.toLower group.title)
            (Dict.toList model.groupDict)
    of
        Nothing ->
            Nothing

        Just ( _, group ) ->
            Just group


popupChoose : PopupChoice -> Model -> ( Model, Cmd Msg )
popupChoose choice model =
    let
        mdl =
            { model
                | popup = NoPopup
                , popupChoices = []
            }

        ( mdl2, feedType, addTheFeed ) =
            case choice of
                AccountChoice account ->
                    case model.popup of
                        PostTextPopup search ->
                            ( insertPostSearch account.username
                                search
                                model
                            , Types.defaultUserFeedType
                            , False
                            )

                        _ ->
                            ( { mdl
                                | userNameInput = account.acct
                                , accountInput = Just account
                              }
                            , Types.defaultUserFeedType
                            , True
                            )

                GroupChoice group ->
                    ( { mdl
                        | groupNameInput = group.title
                        , groupInput = Just group
                        , groupDict =
                            Dict.insert group.id group model.groupDict
                      }
                    , Types.defaultGroupFeedType
                    , True
                    )

                HashtagChoice hashtag ->
                    case model.popup of
                        PostTextPopup search ->
                            ( insertPostSearch hashtag search model
                            , Types.defaultUserFeedType
                            , False
                            )

                        _ ->
                            ( { mdl
                                | hashtagInput = hashtag
                              }
                            , Types.defaultHashtagFeedType
                            , True
                            )

                PostGroupChoice group ->
                    let
                        postState =
                            mdl.postState
                    in
                    ( { mdl
                        | postState =
                            { postState
                                | groupName = group.title
                                , group_id = Just group.id
                            }
                        , groupDict =
                            Dict.insert group.id group model.groupDict
                      }
                    , Types.defaultGroupFeedType
                    , False
                    )

                PostEmojiChoice emoji ->
                    case model.popup of
                        PostTextPopup search ->
                            ( insertPostSearch (emoji.shortcode ++ ":") search model
                            , Types.defaultUserFeedType
                            , False
                            )

                        _ ->
                            ( model, Types.defaultUserFeedType, False )

                PostEmojiCharChoice emojiChar ->
                    case model.popup of
                        PostTextPopup search ->
                            ( insertPostSearchDeletingPrefix True
                                emojiChar.char
                                search
                                model
                            , Types.defaultUserFeedType
                            , False
                            )

                        _ ->
                            ( model, Types.defaultUserFeedType, False )

        cmds =
            case model.popup of
                PostTextPopup _ ->
                    [ Task.attempt (\_ -> Noop) <|
                        -- Probably extraneous, since mobileFocus does this again.
                        Dom.focus nodeIds.postDialogText
                    , mobileFocus nodeIds.postDialogText
                    ]

                _ ->
                    []
    in
    if addTheFeed then
        addFeedType (fillinFeedType feedType mdl2) mdl2

    else
        mdl2 |> withCmds cmds


insertPostSearch : String -> PostPopupSearch -> Model -> Model
insertPostSearch =
    insertPostSearchDeletingPrefix False


insertPostSearchDeletingPrefix : Bool -> String -> PostPopupSearch -> Model -> Model
insertPostSearchDeletingPrefix deletePrefix string search model =
    let
        delta =
            if deletePrefix then
                1

            else
                0

        pos =
            search.position - delta

        searchChars =
            String.toList search.string

        slen =
            List.length searchChars

        chars =
            String.toList string

        postState =
            model.postState

        postChars =
            String.toList postState.text

        newPostText =
            List.take pos postChars
                ++ chars
                ++ List.drop (pos + slen + delta) postChars
                |> String.fromList
    in
    { model
        | popup = NoPopup
        , popupElement = Nothing
        , postState = { postState | text = newPostText }
        , postInputPosition = pos + List.length chars
        , postInputCount = model.postInputCount + 1
    }


popupToNodeId : Popup -> String
popupToNodeId popup =
    case popup of
        UserNamePopup ->
            nodeIds.userNameInput

        GroupNamePopup ->
            nodeIds.groupNameInput

        HashtagPopup ->
            nodeIds.hashtagInput

        PostGroupPopup ->
            nodeIds.postGroupInput

        _ ->
            "thereIsNoNodeWithThisIdAtLeastFnordThereHadBetterNotBe"


type PostPopupType
    = PostPopupAtsign
    | PostPopupSharp
    | PostPopupColon


type alias PostPopupSearch =
    { popupType : PostPopupType
    , string : String
    , position : Int
    }


findPostPopupSearch : String -> String -> Maybe PostPopupSearch
findPostPopupSearch oldText newText =
    let
        oldLen =
            String.length oldText

        newLen =
            String.length newText

        diff =
            newLen - oldLen
    in
    if abs diff /= 1 then
        Nothing

    else
        let
            oldChars =
                String.toList oldText

            newChars =
                String.toList newText

            loop : List Char -> List Char -> List Char -> Maybe PostPopupSearch
            loop old new prefix =
                case old of
                    [] ->
                        case new of
                            [] ->
                                Nothing

                            nc :: _ ->
                                pullPostPopupSearch <| nc :: prefix

                    oc :: otail ->
                        case new of
                            [] ->
                                pullPostPopupSearch prefix

                            nc :: ntail ->
                                if oc == nc then
                                    loop otail ntail <| nc :: prefix

                                else if diff > 0 then
                                    pullPostPopupSearch <| nc :: prefix

                                else
                                    pullPostPopupSearch prefix
        in
        loop oldChars newChars []


whiteSpaceChars : Set Char
whiteSpaceChars =
    Set.fromList [ ' ', '\t', '\n', '\u{000D}' ]


isWhiteSpace : Char -> Bool
isWhiteSpace char =
    Set.member char whiteSpaceChars


charToPostPopupType : Char -> Maybe PostPopupType
charToPostPopupType char =
    case char of
        '@' ->
            Just PostPopupAtsign

        '#' ->
            Just PostPopupSharp

        ':' ->
            Just PostPopupColon

        _ ->
            Nothing


pullPostPopupSearch : List Char -> Maybe PostPopupSearch
pullPostPopupSearch chars =
    let
        loop : List Char -> List Char -> Maybe PostPopupSearch
        loop cs res =
            case cs of
                [] ->
                    Nothing

                c :: rest ->
                    if isWhiteSpace c then
                        Nothing

                    else
                        case charToPostPopupType c of
                            Nothing ->
                                loop rest <| c :: res

                            Just popupType ->
                                Just
                                    { popupType = popupType
                                    , string = String.fromList res
                                    , position = List.length cs
                                    }
    in
    loop chars []


initializePostTextPopup : String -> String -> Model -> ( Model, Cmd Msg )
initializePostTextPopup oldText newText model =
    -- Another way to do this is to get the cursor position and
    -- search back from there.
    -- That requires a loop through `view` and `update`.
    -- text-area-tracker.js does a lot of work, so I think this
    -- single loop through old and new is better.
    let
        mdl =
            { model
                | popup = NoPopup
                , popupElement = Nothing
                , popupChoices = []
            }
    in
    case findPostPopupSearch oldText newText of
        Nothing ->
            mdl |> withNoCmd

        Just search ->
            let
                text =
                    model.postState.text
            in
            mdl
                |> withCmd
                    (delayedPopupCmd
                        (ColumnsUIMsg <|
                            SendDelayedPostTextPopup search text
                        )
                    )


sendDelayedPostTextPopup : PostPopupSearch -> String -> Model -> ( Model, Cmd Msg )
sendDelayedPostTextPopup search postText model =
    if postText /= model.postState.text then
        model |> withNoCmd

    else
        case search.popupType of
            PostPopupColon ->
                searchPostPopupColon search model

            _ ->
                let
                    searchText =
                        search.string

                    request =
                        SearchRequest <|
                            Request.GetSearch
                                { q = searchText
                                , resolve = True
                                , limit = Nothing
                                , offset = Nothing
                                , following = False
                                }

                    ( _, searchCmd ) =
                        sendRequest request model

                    ( mdl, cmd ) =
                        if model.searchActive then
                            { model | nextSearch = searchCmd } |> withNoCmd

                        else
                            { model
                                | searchActive = True
                                , nextSearch = Cmd.none
                            }
                                |> withCmd searchCmd
                in
                { mdl
                    | popup = PostTextPopup search
                    , postTriggerCoordinatesCount =
                        model.postTriggerCoordinatesCount + 1
                }
                    |> withCmd cmd


findEmojis : String -> RenderEnv -> List Emoji
findEmojis string renderEnv =
    let
        lc =
            String.toLower string
    in
    renderEnv.emojisList
        |> List.filter
            (\emoji ->
                String.startsWith lc emoji.shortcode
                    || String.contains lc emoji.shortcode
            )
        |> List.sortWith (emojiOrder string)


emojiOrder : String -> Emoji -> Emoji -> Order
emojiOrder prefix emoji1 emoji2 =
    let
        ( code1, code2 ) =
            ( emoji1.shortcode, emoji2.shortcode )
    in
    if String.startsWith prefix code1 then
        if String.startsWith prefix code2 then
            compare code1 code2

        else
            LT

    else if String.startsWith prefix code2 then
        GT

    else
        compare code1 code2


searchPostPopupColon : PostPopupSearch -> Model -> ( Model, Cmd Msg )
searchPostPopupColon search model =
    let
        string =
            search.string

        choices =
            if string == "" then
                []

            else
                List.concat
                    [ findEmojis search.string model.renderEnv
                        |> List.map PostEmojiChoice
                    , EmojiChar.findEmojiChars search.string
                        |> List.map PostEmojiCharChoice
                    ]
    in
    if choices == [] then
        { model
            | popupChoices = choices
            , popup = NoPopup
        }
            |> withNoCmd

    else
        { model
            | popupChoices = choices
            , popup = PostTextPopup search
            , postTriggerCoordinatesCount =
                model.postTriggerCoordinatesCount + 1
        }
            |> withNoCmd


initializePopup : Popup -> String -> Model -> ( Model, Cmd Msg )
initializePopup popup input model =
    if input == "" then
        { model
            | popup = NoPopup
            , popupElement = Nothing
        }
            |> withNoCmd

    else
        let
            cmd =
                if popup == model.popup then
                    Cmd.none

                else
                    Task.attempt (ColumnsUIMsg << ReceivePopupElement)
                        (Dom.getElement <| popupToNodeId popup)

            searchRequest =
                case popup of
                    UserNamePopup ->
                        AccountsRequest <|
                            Request.GetSearchAccounts
                                { q = input
                                , limit = Nothing
                                , resolve = True
                                , following = False
                                }

                    _ ->
                        SearchRequest <|
                            Request.GetSearch
                                { q = input
                                , resolve = True
                                , limit = Nothing
                                , offset = Nothing
                                , following = False
                                }

            cmd2 =
                delayedPopupCmd
                    (ColumnsUIMsg <|
                        SendDelayedPopupRequest popup input searchRequest
                    )
        in
        { model | popup = popup }
            |> withCmds [ cmd, cmd2 ]


delayedPopupCmd : Msg -> Cmd Msg
delayedPopupCmd =
    delayedCmd 500


delayedCmd : Float -> Msg -> Cmd Msg
delayedCmd millis msg =
    Delay.after millis Delay.Millisecond msg


sendDelayedPopupRequest : Popup -> String -> Request -> Model -> ( Model, Cmd Msg )
sendDelayedPopupRequest popup input request model =
    if popup /= model.popup then
        model |> withNoCmd

    else
        let
            curInput =
                case popup of
                    UserNamePopup ->
                        model.userNameInput

                    GroupNamePopup ->
                        model.groupNameInput

                    HashtagPopup ->
                        model.hashtagInput

                    PostGroupPopup ->
                        model.postState.groupName

                    _ ->
                        ""
        in
        if input /= curInput then
            model |> withNoCmd

        else
            let
                ( _, searchCmd ) =
                    sendRequest request model

                ( mdl, cmd ) =
                    if model.searchActive then
                        { model | nextSearch = searchCmd } |> withNoCmd

                    else
                        { model
                            | searchActive = True
                            , nextSearch = Cmd.none
                        }
                            |> withCmd searchCmd
            in
            mdl |> withCmd cmd


dismissDialog : Model -> ( Model, Cmd Msg )
dismissDialog model =
    let
        dismissExplorer =
            model.popup == NoPopup && model.dialog == NoDialog
    in
    { model
        | dialog = NoDialog
        , editColumnsMessage = Nothing
        , movingColumn = Nothing
        , showFullScrollPill = False
        , popupExplorer =
            if dismissExplorer then
                NoPopupExplorer

            else
                model.popupExplorer
        , popup = NoPopup
        , popupChoices = []
    }
        |> withNoCmd


probeGroupsFeature : String -> Model -> ( Model, Cmd Msg )
probeGroupsFeature server model =
    case Dict.get server model.tokens of
        Nothing ->
            model |> withNoCmd

        justToken ->
            let
                renderEnv =
                    model.renderEnv

                mdl =
                    { model
                        | renderEnv =
                            { renderEnv | loginServer = Just server }
                        , token = justToken
                    }

                request =
                    GroupsRequest <|
                        Request.GetGroups { tab = Request.AdminGroups }

                ( _, cmd ) =
                    sendRequest request mdl
            in
            { model
                | featureProbeRequests =
                    ( Debug.log "probeGroupsFeature" server, request )
                        :: model.featureProbeRequests
            }
                |> withCmd cmd


timestampCmd : (Posix -> ColumnsUIMsg) -> Cmd Msg
timestampCmd wrapper =
    Task.perform (ColumnsUIMsg << TimestampedCmd wrapper) Time.now


scrollPageNow : Bool -> ScrollDirection -> Model -> ( Model, Cmd Msg )
scrollPageNow goAllTheWay direction model =
    let
        now =
            Time.millisToPosix 1000

        lastNow =
            if goAllTheWay then
                now

            else
                Time.millisToPosix 0
    in
    scrollPage direction now { model | lastScroll = ( direction, lastNow ) }


scrollPage : ScrollDirection -> Posix -> Model -> ( Model, Cmd Msg )
scrollPage direction now model =
    let
        allTheWay =
            isScrollAllTheWay direction now model

        mdl =
            { model | lastScroll = ( direction, now ) }
    in
    case model.popupExplorer of
        NoPopupExplorer ->
            scrollPageInternal allTheWay direction mdl

        ThreadPopupExplorer state ->
            scrollThreadExplorer state allTheWay direction mdl


isScrollAllTheWay : ScrollDirection -> Posix -> Model -> Bool
isScrollAllTheWay direction now model =
    let
        ( lastDirection, lastNow ) =
            model.lastScroll

        millis =
            Time.posixToMillis now

        lastMillis =
            Time.posixToMillis lastNow
    in
    lastDirection == direction && lastMillis + 400 >= millis


scrollPageInternal : Bool -> ScrollDirection -> Model -> ( Model, Cmd Msg )
scrollPageInternal allTheWay direction model =
    let
        renderEnv =
            model.renderEnv

        ( windowWidth, _ ) =
            renderEnv.windowSize
    in
    let
        scrollLeft =
            model.bodyScroll.scrollLeft |> round

        col0Left =
            if model.showLeftColumn then
                leftColumnWidth + 5

            else
                5

        columnCnt =
            model.feedSet.feeds |> List.length

        columnWidth =
            renderEnv.columnWidth + 4

        width =
            col0Left + columnCnt * columnWidth

        rawNewScroll =
            if allTheWay then
                case direction of
                    ScrollLeft ->
                        col0Left

                    ScrollRight ->
                        width - windowWidth

            else
                case direction of
                    ScrollLeft ->
                        let
                            nominal =
                                max col0Left <| scrollLeft - windowWidth

                            proper =
                                (nominal - col0Left + columnWidth - 1) // columnWidth
                        in
                        max 0 proper * columnWidth + col0Left

                    ScrollRight ->
                        let
                            maxScroll =
                                width - windowWidth

                            nominal =
                                scrollLeft + windowWidth

                            proper =
                                (nominal - col0Left) // columnWidth
                        in
                        min maxScroll
                            ((proper * columnWidth) + col0Left)

        newScroll =
            rawNewScroll + 1

        cmd =
            if newScroll /= scrollLeft then
                Dom.setViewportOf "body" (toFloat newScroll) 0
                    |> Task.attempt (\_ -> Noop)

            else
                Cmd.none
    in
    model |> withCmd cmd


{-| If you enable this, you need to fix the update code to somehow
ignore it when the new posts come in.
-}
showReceivedPost : Bool
showReceivedPost =
    False


adjustColumnsForPost : Status -> Model -> Model
adjustColumnsForPost status model =
    let
        userFeedType =
            case model.account of
                Nothing ->
                    Types.defaultUserFeedType

                Just { username } ->
                    -- TODO: when I add flags to user feeds, fix this
                    UserFeed
                        { username = username
                        , server = ""
                        , flags = Nothing
                        }

        updateFeed feed =
            if
                List.member feed.feedType
                    [ HomeFeed
                    , PublicFeed { flags = Nothing } --TODO: non-null flags
                    , userFeedType
                    ]
            then
                let
                    elements =
                        feed.elements
                in
                case elements of
                    StatusElements statuses ->
                        { feed
                            | elements =
                                StatusElements <| status :: statuses
                        }

                    _ ->
                        feed

            else
                feed

        feedSet =
            model.feedSet
    in
    { model
        | feedSet =
            if showReceivedPost then
                { feedSet
                    | feeds = List.map updateFeed feedSet.feeds
                }

            else
                model.feedSet
        , dialog =
            case model.dialog of
                PostDialog ->
                    NoDialog

                d ->
                    d
        , postState = initialPostState
    }


fillinFeedType : FeedType -> Model -> FeedType
fillinFeedType feedType model =
    case feedType of
        UserFeed _ ->
            let
                ( username, server ) =
                    case String.split "@" model.userNameInput of
                        [] ->
                            -- Can't happen
                            ( "", "" )

                        [ name ] ->
                            ( name, "" )

                        name :: s :: _ ->
                            ( name, s )
            in
            UserFeed
                { username = username
                , server = server
                , flags = Nothing
                }

        GroupFeed _ ->
            GroupFeed <|
                case model.groupInput of
                    Nothing ->
                        model.groupNameInput

                    Just group ->
                        group.id

        HashtagFeed _ ->
            HashtagFeed model.hashtagInput

        ListFeed _ ->
            case model.selectedList of
                Nothing ->
                    feedType

                Just { id } ->
                    ListFeed id

        _ ->
            feedType


moveFeedType : FeedType -> FeedType -> Model -> ( Model, Cmd Msg )
moveFeedType movingFeedType feedType model =
    let
        -- temporary
        direction =
            1

        feedSetDefinition =
            model.feedSetDefinition

        feedTypes =
            feedSetDefinition.feedTypes

        feedSet =
            model.feedSet
    in
    case LE.findIndex ((==) feedType) feedTypes of
        Nothing ->
            model |> withNoCmd

        Just index ->
            case LE.findIndex ((==) movingFeedType) feedTypes of
                Nothing ->
                    model |> withNoCmd

                Just movingIndex ->
                    let
                        newFeedSetDefinition =
                            { feedSetDefinition
                                | feedTypes =
                                    moveElementAt movingIndex index feedTypes
                            }

                        newFeedSet =
                            { feedSet
                                | feeds =
                                    moveElementAt movingIndex index feedSet.feeds
                            }
                    in
                    { model
                        | feedSetDefinition = newFeedSetDefinition
                        , feedSet = newFeedSet
                        , movingColumn = Nothing
                    }
                        |> withCmd (maybePutFeedSetDefinition model newFeedSetDefinition)


moveElementAt : Int -> Int -> List a -> List a
moveElementAt fromIndex toIndex list =
    let
        length =
            List.length list
    in
    case LE.getAt fromIndex list of
        Nothing ->
            list

        Just a ->
            let
                listMinus =
                    List.concat
                        [ List.take fromIndex list
                        , List.drop (fromIndex + 1) list
                        ]
            in
            List.concat
                [ List.take toIndex listMinus
                , [ a ]
                , List.drop toIndex listMinus
                ]


deleteFeedType : FeedType -> Model -> ( Model, Cmd Msg )
deleteFeedType feedType model =
    let
        feedSetDefinition =
            model.feedSetDefinition

        feedTypes =
            feedSetDefinition.feedTypes

        feedSet =
            model.feedSet

        newFeedSetDefinition =
            { feedSetDefinition
                | feedTypes =
                    List.filter ((/=) feedType) feedTypes
            }

        newFeedSet =
            { feedSet
                | feeds =
                    List.filter (.feedType >> (/=) feedType) feedSet.feeds
            }

        mdl2 =
            removeFeedEnv feedType model
    in
    { mdl2
        | feedSetDefinition = newFeedSetDefinition
        , feedSet = newFeedSet
        , editColumnsMessage = Nothing
    }
        |> withCmds
            [ maybePutFeedSetDefinition model newFeedSetDefinition
            , makeScrollRequest feedType False
            ]


addFeedType : FeedType -> Model -> ( Model, Cmd Msg )
addFeedType feedType model =
    let
        feedSetDefinition =
            model.feedSetDefinition

        feedTypes =
            feedSetDefinition.feedTypes
    in
    if List.member feedType feedTypes then
        -- feedType must be unique in the list.
        -- It might be nice to inform the user of the mistake.
        { model | editColumnsMessage = Just "Duplicate feed." }
            |> withNoCmd

    else
        let
            newFeedSetDefinition =
                { feedSetDefinition
                    | feedTypes = List.append feedTypes [ feedType ]
                }

            feedSet =
                model.feedSet

            newFeed =
                { feedType = Debug.log "addFeedType" feedType
                , elements = Types.feedTypeToElements feedType
                , newElements = 0
                }

            newFeedSet =
                { feedSet
                    | feeds =
                        List.append feedSet.feeds [ newFeed ]
                }

            userNameInput =
                case feedType of
                    UserFeed _ ->
                        ""

                    _ ->
                        model.userNameInput

            ( groupNameInput, mdl, loadGroupCmd ) =
                case feedType of
                    GroupFeed group_id ->
                        let
                            ( mdl2, cmd ) =
                                maybeLoadGroup group_id model
                        in
                        ( "", mdl2, cmd )

                    _ ->
                        ( model.groupNameInput, model, Cmd.none )

            hashtagInput =
                case feedType of
                    HashtagFeed _ ->
                        ""

                    _ ->
                        model.hashtagInput

            ( mdl3, cmd3 ) =
                case feedType of
                    UserFeed _ ->
                        case model.accountInput of
                            Nothing ->
                                ( mdl, Cmd.none )

                            Just account ->
                                let
                                    accountId =
                                        Types.accountToAccountId account
                                in
                                mergeAccountId accountId
                                    mdl.server
                                    { mdl | accountInput = Nothing }

                    GroupFeed _ ->
                        { mdl | groupInput = Nothing }
                            |> withNoCmd

                    _ ->
                        mdl |> withNoCmd

            mdl4 =
                addFeedEnv feedType mdl3

            cmd5 =
                Delay.after 100 Delay.Millisecond <|
                    (ApplyToModel <| scrollPageNow True ScrollRight)
        in
        { mdl4
            | feedSetDefinition = newFeedSetDefinition
            , feedSet = newFeedSet
            , userNameInput = userNameInput
            , groupNameInput = groupNameInput
            , hashtagInput = hashtagInput
            , editColumnsMessage = Nothing
            , popup = NoPopup
            , popupChoices = []
        }
            |> reloadFeed newFeed
            |> addCmd (maybePutFeedSetDefinition model newFeedSetDefinition)
            |> addCmd loadGroupCmd
            |> addCmd cmd3
            |> addCmd cmd5


maybeLoadGroup : String -> Model -> ( Model, Cmd Msg )
maybeLoadGroup group_id model =
    case getGroup group_id model of
        Nothing ->
            let
                ( mdl2, cmd ) =
                    sendRequest
                        (GroupsRequest <|
                            Request.GetGroup { id = group_id }
                        )
                        model
            in
            ( mdl2, cmd )

        Just _ ->
            ( model, Cmd.none )


getGroup : String -> Model -> Maybe Group
getGroup group_id model =
    Dict.get group_id model.groupDict


usernameAtServer : String -> String -> RenderEnv -> String
usernameAtServer username server renderEnv =
    if server == "" || Just server == renderEnv.loginServer then
        username

    else
        username ++ "@" ++ server


{-| TODO:

This needs to be sent to `server`, not `model.renderEnv.loginServer`.
Or maybe that should be a parameter.

-}
startReloadUserFeed : Maybe Paging -> UserFeedParams -> Model -> Request
startReloadUserFeed paging params model =
    let
        { username, server } =
            params

        accountsRequest () =
            AccountsRequest <|
                Request.GetSearchAccounts
                    { q = username
                    , limit = Nothing
                    , resolve = False
                    , following = False
                    }
    in
    case model.renderEnv.loginServer of
        Nothing ->
            accountsRequest ()

        Just loginServer ->
            case Dict.get loginServer model.accountIdDict of
                Nothing ->
                    accountsRequest ()

                Just acctIds ->
                    let
                        nameAtServer =
                            usernameAtServer username server model.renderEnv
                    in
                    case LE.find (.username >> (==) nameAtServer) acctIds of
                        Nothing ->
                            let
                                supportsAccountByUsername =
                                    case
                                        Dict.get loginServer
                                            model.supportsAccountByUsername
                                    of
                                        Just supports ->
                                            supports

                                        Nothing ->
                                            False
                            in
                            if supportsAccountByUsername then
                                AccountsRequest <|
                                    Request.GetAccountByUsername
                                        { username = username }

                            else
                                accountsRequest ()

                        Just acctId ->
                            getStatusesRequest paging acctId.id params


{-| This processes the result of the `GetSearchAccounts` request above.
-}
continueReloadUserFeed : Maybe Paging -> FeedType -> List Account -> Model -> ( Model, Cmd Msg )
continueReloadUserFeed paging feedType accounts model =
    case feedType of
        UserFeed params ->
            let
                { username, server, flags } =
                    params

                userAtServer =
                    if (server == "") || (Just server == model.renderEnv.loginServer) then
                        username

                    else
                        username ++ "@" ++ server
            in
            case LE.find (.acct >> (==) userAtServer) accounts of
                Nothing ->
                    model |> withNoCmd

                Just account ->
                    let
                        { id } =
                            account

                        req =
                            getStatusesRequest paging id params

                        ( mdl, cmd ) =
                            case model.renderEnv.loginServer of
                                Just loginServer ->
                                    mergeAccountId (Types.accountToAccountId account)
                                        loginServer
                                        model

                                _ ->
                                    model |> withNoCmd

                        ( mdl2, cmd2 ) =
                            sendGeneralRequest
                                (ColumnsSendMsg
                                    << ReceiveFeed req paging (UserFeed params)
                                )
                                req
                                mdl
                    in
                    mdl2 |> withCmds [ cmd, cmd2 ]

        _ ->
            model |> withNoCmd


getStatusesRequest : Maybe Paging -> String -> UserFeedParams -> Request
getStatusesRequest paging id params =
    let
        ( ( only_media, pinned ), ( exclude_replies, exclude_reblogs ) ) =
            case params.flags of
                Nothing ->
                    ( ( False, False ), ( False, False ) )

                Just flgs ->
                    ( ( flgs.only_media, flgs.pinned )
                    , ( not flgs.replies, not flgs.reblogs )
                    )
    in
    AccountsRequest <|
        Request.GetStatuses
            { id = id
            , only_media = only_media
            , pinned = pinned
            , exclude_replies = exclude_replies
            , exclude_reblogs = exclude_reblogs
            , paging = paging
            }


reloadFeed : Feed -> Model -> ( Model, Cmd Msg )
reloadFeed feed model =
    reloadFeedPaging Nothing feed model


reloadFeedPaging : Maybe Paging -> Feed -> Model -> ( Model, Cmd Msg )
reloadFeedPaging paging feed model =
    let
        feedType =
            Debug.log "reloadFeedPaging" feed.feedType

        request =
            case feedType of
                HomeFeed ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetHomeTimeline { paging = paging }

                UserFeed params ->
                    Just <| startReloadUserFeed paging params model

                GroupFeed group_id ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetGroupTimeline
                                { group_id = group_id
                                , paging = paging
                                }

                HashtagFeed hashtag ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetTagTimeline
                                { hashtag = hashtag
                                , local = False
                                , only_media = False
                                , paging = paging
                                }

                ListFeed list_id ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetListTimeline
                                { list_id = list_id
                                , paging = paging
                                }

                ProFeed { flags } ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetProTimeline <|
                                case flags of
                                    Nothing ->
                                        { only_media = False
                                        , paging = paging
                                        }

                                    Just { only_media } ->
                                        { only_media = only_media
                                        , paging = paging
                                        }

                PublicFeed { flags } ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetPublicTimeline <|
                                case flags of
                                    Nothing ->
                                        { local = True
                                        , only_media = False
                                        , paging = paging
                                        }

                                    Just { local, only_media } ->
                                        { local = local
                                        , only_media = only_media
                                        , paging = paging
                                        }

                NotificationFeed { accountId, exclusions } ->
                    Just <|
                        NotificationsRequest <|
                            Request.GetNotifications
                                { paging = paging
                                , exclude_types = exclusions
                                , account_id = accountId
                                }

                _ ->
                    Nothing
    in
    case request of
        Nothing ->
            model |> withNoCmd

        Just req ->
            let
                id =
                    Debug.log "Loading feed" <|
                        Types.feedID feed.feedType

                renderEnv =
                    model.renderEnv

                mdl =
                    { model
                        | loadingFeeds =
                            Set.insert id model.loadingFeeds
                    }

                cmd =
                    makeScrollRequest feedType False

                sendReq =
                    case req of
                        AccountsRequest (Request.GetAccountByUsername _) ->
                            ColumnsSendMsg
                                << ReceiveAccountByUsername paging feedType

                        _ ->
                            ColumnsSendMsg
                                << ReceiveFeed req paging feedType

                ( mdl2, cmd2 ) =
                    sendGeneralRequest sendReq req mdl

                cmd3 =
                    -- It would be nice to delay scrolling until the
                    -- response comes in, but that creates race
                    -- conditions with the scroll detection code, and
                    -- putting it here does not.
                    case pagingToReceiveType paging of
                        ReceiveMoreFeed ->
                            Cmd.none

                        _ ->
                            Dom.setViewportOf id 0 0
                                |> Task.attempt (\_ -> Noop)
            in
            mdl2 |> withCmds [ cmd, cmd2, cmd3 ]


pagingToReceiveType : Maybe Paging -> ReceiveFeedType
pagingToReceiveType paging =
    case paging of
        Nothing ->
            ReceiveWholeFeed

        Just p ->
            if p.max_id == Nothing then
                if p.since_id == Nothing then
                    ReceiveWholeFeed

                else
                    ReceiveNewFeed

            else
                ReceiveMoreFeed


{-| Process Requests sent from the columns page.

These send requests over the wire to instances.

-}
columnsSendMsg : ColumnsSendMsg -> Model -> ( Model, Cmd Msg )
columnsSendMsg msg model =
    case msg of
        ColumnsSendNoop ->
            model |> withNoCmd

        ReceiveAccountByUsername paging feedType result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok response ->
                    case response.request of
                        AccountsRequest (Request.GetAccountByUsername _) ->
                            case response.entity of
                                AccountEntity account ->
                                    continueReloadUserFeed
                                        paging
                                        feedType
                                        [ account ]
                                        model

                                _ ->
                                    model |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveFeed request paging feedType result ->
            receiveFeed request paging feedType result model


fillinMissingReplyToAccountIds : Model -> Model
fillinMissingReplyToAccountIds model =
    let
        references =
            model.references

        fillin1 : String -> ( Dict String Reference, Set String ) -> ( Dict String Reference, Set String )
        fillin1 id ( feedRefs, missing ) =
            case Dict.get id references of
                Nothing ->
                    ( feedRefs, missing )

                Just ref ->
                    ( Dict.insert id ref feedRefs, Set.remove id missing )

        fillin : String -> FeedEnv -> FeedEnv
        fillin k env =
            let
                ( refs, miss ) =
                    Set.foldl fillin1
                        ( env.references, env.missingReplyToAccountIds )
                        env.missingReplyToAccountIds
            in
            if refs == env.references && miss == env.missingReplyToAccountIds then
                -- Preserve EQ
                env

            else
                { env
                    | references = refs
                    , missingReplyToAccountIds = miss
                }

        feedEnvs =
            Dict.map fillin model.feedEnvs
                |> debugFeedEnvsMissing
    in
    { model | feedEnvs = feedEnvs }


debugFeedEnvsMissing : Dict String FeedEnv -> Dict String FeedEnv
debugFeedEnvsMissing feedEnvs =
    let
        missing =
            Dict.foldl
                (\k env miss ->
                    Set.union env.missingReplyToAccountIds miss
                )
                Set.empty
                feedEnvs

        size =
            Debug.log "Total missing replyToAccountIds" <|
                Set.size missing
    in
    feedEnvs


receiveFeed : Request -> Maybe Paging -> FeedType -> Result Error Response -> Model -> ( Model, Cmd Msg )
receiveFeed request paging feedType result model =
    let
        renderEnv =
            model.renderEnv

        feedId =
            Types.feedID feedType

        model2 =
            { model
                | loadingFeeds =
                    Set.remove (Debug.log "ReceiveFeed" feedId)
                        model.loadingFeeds
            }

        ( mdl, cmd ) =
            receiveResponse request result model2
    in
    case mdl.msg of
        Just _ ->
            mdl |> withCmd cmd

        Nothing ->
            case mdl.entity of
                Nothing ->
                    mdl |> withCmd cmd

                Just e ->
                    let
                        ( elements, references ) =
                            case e of
                                StatusListEntity statuses ->
                                    ( Just <| StatusElements statuses
                                    , addStatusesReferences statuses
                                        model.references
                                    )

                                NotificationListEntity notifications ->
                                    ( Just <| NotificationElements notifications
                                    , addNotificationsReferences notifications
                                        model.references
                                    )

                                AccountListEntity accounts ->
                                    ( Just <| AccountElements accounts
                                    , model.references
                                    )

                                _ ->
                                    ( Nothing, model.references )

                        feedSet =
                            mdl.feedSet

                        feedEnvs =
                            model.feedEnvs

                        feedEnv =
                            case Dict.get feedId model.feedEnvs of
                                Nothing ->
                                    emptyFeedEnv

                                Just env ->
                                    env

                        ( feeds, ( mdl2, cmd2 ), feedEnv2 ) =
                            case elements of
                                Nothing ->
                                    ( feedSet.feeds
                                    , ( mdl, Cmd.none )
                                    , feedEnv
                                    )

                                Just elem ->
                                    case elem of
                                        AccountElements accounts ->
                                            ( feedSet.feeds
                                            , continueReloadUserFeed
                                                paging
                                                feedType
                                                accounts
                                                mdl
                                            , feedEnv
                                            )

                                        _ ->
                                            let
                                                receiveType =
                                                    pagingToReceiveType paging
                                            in
                                            ( LE.updateIf
                                                (\feed ->
                                                    feedType == feed.feedType
                                                )
                                                (\feed ->
                                                    updateReceivedFeed
                                                        receiveType
                                                        elem
                                                        feed
                                                )
                                                feedSet.feeds
                                            , ( mdl, Cmd.none )
                                            , updateFeedEnvReferences
                                                receiveType
                                                elem
                                                model.references
                                                feedEnv
                                            )

                        missing =
                            feedEnv2.missingReplyToAccountIds

                        mdl3 =
                            { mdl2
                                | feedEnvs =
                                    Dict.insert feedId feedEnv2 feedEnvs
                            }

                        mdl4 =
                            if Set.size mdl3.loadingFeeds > 0 then
                                mdl3

                            else
                                fillinMissingReplyToAccountIds mdl3
                    in
                    { mdl4
                        | feedSet =
                            { feedSet | feeds = feeds }
                        , references = references
                    }
                        |> withCmds
                            [ cmd
                            , cmd2
                            ]


updateFeedEnvReferences : ReceiveFeedType -> FeedElements -> ReferenceDict -> FeedEnv -> FeedEnv
updateFeedEnvReferences receiveType feedElements references feedEnv =
    let
        referencesAndMissing =
            case receiveType of
                ReceiveWholeFeed ->
                    ( Dict.empty, Set.empty )

                _ ->
                    ( feedEnv.references, feedEnv.missingReplyToAccountIds )

        processReplyTo maybeId ( refs, missing ) =
            case maybeId of
                Nothing ->
                    ( refs, missing )

                Just id ->
                    case Dict.get id references of
                        Just ref ->
                            ( Dict.insert id ref refs, missing )

                        Nothing ->
                            ( refs, Set.insert id missing )

        processStatus status refsMissing =
            processReplyTo status.in_reply_to_account_id refsMissing
                |> processWrappedStatus status.reblog
                |> processWrappedStatus status.quote

        processWrappedStatus wstat refsMissing =
            case wstat of
                Nothing ->
                    refsMissing

                Just (WrappedStatus stat) ->
                    processStatus stat refsMissing

        processNotification notification refsMissing =
            case notification.status of
                Nothing ->
                    refsMissing

                Just stat ->
                    processStatus stat refsMissing

        ( feedReferences, missingReplyToAccountIds ) =
            case feedElements of
                StatusElements statuses ->
                    List.foldl processStatus referencesAndMissing statuses

                NotificationElements notifications ->
                    List.foldl processNotification referencesAndMissing notifications

                _ ->
                    referencesAndMissing
    in
    { feedEnv
        | references = feedReferences
        , missingReplyToAccountIds = missingReplyToAccountIds
    }


addStatusesReferences : List Status -> ReferenceDict -> ReferenceDict
addStatusesReferences statuses references =
    List.foldl addStatusReferences references statuses


addStatusReferences : Status -> ReferenceDict -> ReferenceDict
addStatusReferences status references =
    addAccountReference status.account references
        |> addWrappedStatusReferences status.reblog
        |> addWrappedStatusReferences status.quote
        |> (\refs ->
                List.foldl addMentionReference refs status.mentions
           )


addAccountReference : Account -> ReferenceDict -> ReferenceDict
addAccountReference account references =
    Dict.insert account.id (ReferencedAccount account) references


addWrappedStatusReferences : Maybe WrappedStatus -> ReferenceDict -> ReferenceDict
addWrappedStatusReferences wrappedStatus references =
    case wrappedStatus of
        Nothing ->
            references

        Just (WrappedStatus status) ->
            addStatusReferences status references


addMentionReference : Mention -> ReferenceDict -> ReferenceDict
addMentionReference mention references =
    case Dict.get mention.id references of
        Just (ReferencedAccount _) ->
            references

        _ ->
            Dict.insert mention.id (ReferencedMention mention) references


addNotificationsReferences : List Notification -> ReferenceDict -> ReferenceDict
addNotificationsReferences notifications references =
    List.foldl addNotificationReferences references notifications


addNotificationReferences : Notification -> ReferenceDict -> ReferenceDict
addNotificationReferences notification references =
    addAccountReference notification.account references
        |> (case notification.status of
                Nothing ->
                    identity

                Just status ->
                    addStatusReferences status
           )


updateReceivedFeed : ReceiveFeedType -> FeedElements -> Feed -> Feed
updateReceivedFeed receiveType elements feed =
    let
        ( elements2, newElements ) =
            case receiveType of
                ReceiveWholeFeed ->
                    ( elements, 0 )

                ReceiveMoreFeed ->
                    ( appendFeedElements feed.elements elements feed.elements
                        |> Tuple.first
                    , feed.newElements
                    )

                ReceiveNewFeed ->
                    appendFeedElementsTruncated (Just 100)
                        True
                        elements
                        feed.elements
                        feed.elements
    in
    if
        (elements2 == feed.elements)
            && (newElements == feed.newElements)
    then
        feed

    else
        { feed
            | elements = elements2
            , newElements = newElements
        }


{-| Prepend the first list to the second, stopping when you get to
a matching element. Currently throws away the rest of the second list.
-}
merge : (a -> comparable) -> List a -> List a -> ( List a, Int )
merge keyfun prefix suffix =
    let
        dict : Dict comparable Int
        dict =
            List.indexedMap (\idx a -> ( keyfun a, idx )) suffix
                |> Dict.fromList

        loop : Int -> List a -> List a -> Int -> List a -> ( List a, Int )
        loop preidx pre suf residx res =
            let
                newresidx idx =
                    if residx >= 0 then
                        residx

                    else
                        idx
            in
            case pre of
                [] ->
                    if res == [] then
                        ( suf, 0 )

                    else
                        ( List.reverse res, List.length res )

                p :: ptail ->
                    case Dict.get (keyfun p) dict of
                        Nothing ->
                            loop (preidx + 1)
                                ptail
                                suf
                                (residx + 1)
                            <|
                                (p :: res)

                        Just _ ->
                            if ptail == [] then
                                ( List.concat
                                    [ List.concat [ pre, res ]
                                        |> List.reverse
                                    , listCdr suf
                                    ]
                                , preidx
                                )

                            else
                                -- Probably wont' ever happen
                                ( List.append (List.reverse res) pre
                                , preidx + List.length pre
                                )
    in
    loop 0 prefix suffix -1 []


listCdr : List a -> List a
listCdr list =
    case List.tail list of
        Nothing ->
            []

        Just l ->
            l


appendFeedElements : FeedElements -> FeedElements -> FeedElements -> ( FeedElements, Int )
appendFeedElements =
    appendFeedElementsTruncated Nothing False


appendFeedElementsTruncated : Maybe Int -> Bool -> FeedElements -> FeedElements -> FeedElements -> ( FeedElements, Int )
appendFeedElementsTruncated maybeMax useMerge fe1 fe2 default =
    let
        truncate list =
            case maybeMax of
                Nothing ->
                    list

                Just max ->
                    List.take max list
    in
    case fe1 of
        StatusElements els1 ->
            case fe2 of
                StatusElements els2 ->
                    let
                        ( l, newcnt ) =
                            if useMerge then
                                merge .id els1 els2

                            else
                                ( List.append els1 els2, List.length els1 )
                    in
                    ( truncate l
                        |> StatusElements
                    , newcnt
                    )

                _ ->
                    ( default, 0 )

        NotificationElements els1 ->
            case fe2 of
                NotificationElements els2 ->
                    let
                        ( l, newcnt ) =
                            if useMerge then
                                merge .id els1 els2

                            else
                                ( List.append els1 els2, List.length els1 )
                    in
                    ( truncate l
                        |> NotificationElements
                    , 0
                    )

                _ ->
                    ( default, 0 )

        AccountElements els1 ->
            case fe2 of
                AccountElements els2 ->
                    ( AccountElements <| List.append els1 els2, 0 )

                _ ->
                    ( default, 0 )

        ConversationsElements els1 ->
            case fe2 of
                ConversationsElements els2 ->
                    ( ConversationsElements <| List.append els1 els2, 0 )

                _ ->
                    ( default, 0 )

        ResultsElements els1 ->
            case fe2 of
                ResultsElements els2 ->
                    ( ResultsElements <| List.append els1 els2, 0 )

                _ ->
                    ( default, 0 )


{-| Process UI messages from the API Explorer page.

These change the Model, but don't send anything over the wire to any instances.

-}
explorerUIMsg : ExplorerUIMsg -> Model -> ( Model, Cmd Msg )
explorerUIMsg msg model =
    case msg of
        SetWhichGroups s ->
            let
                whichGroups =
                    case s of
                        "Featured" ->
                            Request.FeaturedGroups

                        "Admin" ->
                            Request.AdminGroups

                        _ ->
                            Request.MemberGroups
            in
            { model | whichGroups = whichGroups }
                |> withNoCmd

        ClearSentReceived ->
            { model
                | request = Nothing
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> withNoCmd

        TogglePrettify ->
            { model | prettify = not model.prettify }
                |> withNoCmd

        ToggleShowMetadata ->
            { model | showMetadata = not model.showMetadata }
                |> withNoCmd

        ToggleShowReceived ->
            { model | showReceived = not model.showReceived }
                |> withNoCmd

        ToggleShowEntity ->
            { model | showEntity = not model.showEntity }
                |> withNoCmd

        SetQ q ->
            { model | q = q }
                |> withNoCmd

        ToggleResolve ->
            { model | resolve = not model.resolve }
                |> withNoCmd

        ToggleFollowing ->
            { model | following = not model.following }
                |> withNoCmd

        ToggleFollowReblogs ->
            { model | followReblogs = not model.followReblogs }
                |> withNoCmd

        SetSelectedRequest selectedRequest ->
            { model | selectedRequest = selectedRequest }
                |> withNoCmd

        SetUsername username ->
            { model | username = username }
                |> withNoCmd

        SetAccountId accountId ->
            let
                mdl =
                    { model | accountId = accountId }
            in
            mdl
                |> withCmd (getAccountIdRelationships True mdl)

        SetMaxId maxId ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | max_id = maxId } }
                |> withNoCmd

        SetSinceId sinceId ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | since_id = sinceId } }
                |> withNoCmd

        SetMinId minId ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | min_id = minId } }
                |> withNoCmd

        SetLimit limit ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | limit = limit } }
                |> withNoCmd

        ToggleSmartPaging ->
            { model | smartPaging = not model.smartPaging }
                |> withNoCmd

        ToggleShowJsonTree ->
            let
                mdl =
                    if model.showJsonTree then
                        { model
                            | selectedKeyPath = ""
                            , selectedKeyValue = ""
                        }

                    else
                        model
            in
            { mdl | showJsonTree = not model.showJsonTree }
                |> withNoCmd

        ToggleUseElmButtonNames ->
            { model | useElmButtonNames = not model.useElmButtonNames }
                |> withNoCmd

        ToggleShowUpdateCredentials ->
            { model | showUpdateCredentials = not model.showUpdateCredentials }
                |> withNoCmd

        ToggleOnlyMedia ->
            { model | onlyMedia = not model.onlyMedia }
                |> withNoCmd

        TogglePinned ->
            { model | pinned = not model.pinned }
                |> withNoCmd

        ToggleExcludeReplies ->
            { model | excludeReplies = not model.excludeReplies }
                |> withNoCmd

        ToggleExcludeReblogs ->
            { model | excludeReblogs = not model.excludeReblogs }
                |> withNoCmd

        SetAccountIds accountIds ->
            { model | accountIds = accountIds }
                |> withNoCmd

        SetDisplayName displayName ->
            { model | displayName = displayName }
                |> withNoCmd

        SetNote note ->
            { model | note = note }
                |> withNoCmd

        SetField index updateValue string ->
            let
                mdl =
                    case LE.getAt index model.fields of
                        Nothing ->
                            model

                        Just field ->
                            let
                                fld =
                                    if updateValue then
                                        { field | value = string }

                                    else
                                        { field | name = string }
                            in
                            { model | fields = LE.setAt index fld model.fields }
            in
            mdl |> withNoCmd

        GetAvatarFile clearAvatar ->
            if clearAvatar then
                { model | avatarFile = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd
                        (File.Select.file imageMimeTypes
                            (ExplorerUIMsg << ReceiveAvatarFile)
                        )

        ReceiveAvatarFile file ->
            { model | avatarFile = Just file }
                |> withNoCmd

        GetHeaderFile clearHeader ->
            if clearHeader then
                { model | headerFile = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd
                        (File.Select.file imageMimeTypes
                            (ExplorerUIMsg << ReceiveHeaderFile)
                        )

        ReceiveHeaderFile file ->
            { model | headerFile = Just file }
                |> withNoCmd

        SetPrivacy privacy ->
            { model | privacy = privacy }
                |> withNoCmd

        ToggleLocked ->
            { model | locked = not model.locked }
                |> withNoCmd

        ToggleSensitive ->
            { model | sensitive = not model.sensitive }
                |> withNoCmd

        SetLanguage language ->
            { model | language = language }
                |> withNoCmd

        SetFilterId filterId ->
            { model | filterId = filterId }
                |> withNoCmd

        ToggleFilterInputContext context ->
            let
                filterInput =
                    model.filterInput

                isSet =
                    List.member context filterInput.context
            in
            { model
                | filterInput =
                    { filterInput
                        | context =
                            if isSet then
                                List.filter ((/=) context) filterInput.context

                            else
                                context :: filterInput.context
                    }
            }
                |> withNoCmd

        SetFilterPhrase phrase ->
            let
                filterInput =
                    model.filterInput
            in
            { model | filterInput = { filterInput | phrase = phrase } }
                |> withNoCmd

        ToggleFilterIrreversible ->
            let
                filterInput =
                    model.filterInput
            in
            { model
                | filterInput =
                    { filterInput | irreversible = not filterInput.irreversible }
            }
                |> withNoCmd

        ToggleFilterWholeWord ->
            let
                filterInput =
                    model.filterInput
            in
            { model
                | filterInput =
                    { filterInput | whole_word = not filterInput.whole_word }
            }
                |> withNoCmd

        SetFilterExpiresIn expires_in ->
            let
                filterInput =
                    model.filterInput
            in
            { model | filterInput = { filterInput | expires_in = expires_in } }
                |> withNoCmd

        SetGroupId groupId ->
            { model | groupId = groupId }
                |> withNoCmd

        SetGroupIds groupIds ->
            { model | groupIds = groupIds }
                |> withNoCmd

        SetOffset offset ->
            { model | offset = offset }
                |> withNoCmd

        SetGroupTitle groupTitle ->
            { model | groupTitle = groupTitle }
                |> withNoCmd

        SetGroupDescription groupDescription ->
            { model | groupDescription = groupDescription }
                |> withNoCmd

        GetGroupCoverImage clearCoverImage ->
            if clearCoverImage then
                { model | groupCoverImage = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd
                        (File.Select.file imageMimeTypes
                            (ExplorerUIMsg << ReceiveGroupCoverImage)
                        )

        SetReportCommentString reportComment ->
            { model | reportComment = reportComment }
                |> withNoCmd

        SetStatusIds statusIds ->
            { model | statusIds = statusIds }
                |> withNoCmd

        SetReportComment reportComment ->
            { model | reportComment = reportComment }
                |> withNoCmd

        ToggleForwardReport ->
            { model | forwardReport = not model.forwardReport }
                |> withNoCmd

        SetScheduledStatusId scheduledStatusId ->
            { model | scheduledStatusId = scheduledStatusId }
                |> withNoCmd

        ReceiveGroupCoverImage groupCoverImage ->
            { model | groupCoverImage = Just groupCoverImage }
                |> withNoCmd

        SetListTitle listTitle ->
            { model | listTitle = listTitle }
                |> withNoCmd

        SetStatusId statusId ->
            { model | statusId = statusId }
                |> withNoCmd

        ToggleMuteNotifications ->
            { model | muteNotifications = not model.muteNotifications }
                |> withNoCmd

        ToggleExcludedNotificationType notificationType ->
            let
                types =
                    model.excludedNotificationTypes

                newTypes =
                    if List.member notificationType types then
                        List.filter ((/=) notificationType) types

                    else
                        notificationType :: types
            in
            { model | excludedNotificationTypes = newTypes }
                |> withNoCmd

        IncludeAllNotifications ->
            { model | excludedNotificationTypes = [] }
                |> withNoCmd

        IncludeOnlyMentionNotifications ->
            { model
                | excludedNotificationTypes =
                    [ FollowNotification
                    , ReblogNotification
                    , FavouriteNotification
                    , PollNotification
                    ]
            }
                |> withNoCmd

        SetNotificationsAccountId notificationsAccountId ->
            { model | notificationsAccountId = notificationsAccountId }
                |> withNoCmd

        SetNotificationId notificationId ->
            { model | notificationId = notificationId }
                |> withNoCmd

        ToggleShowPostStatus ->
            { model | showPostStatus = not model.showPostStatus }
                |> withNoCmd

        SetStatus status ->
            { model | status = status }
                |> withNoCmd

        SetInReplyToId in_reply_to_id ->
            { model | in_reply_to_id = in_reply_to_id }
                |> withNoCmd

        SetInQuoteOfId quote_of_id ->
            { model | quote_of_id = quote_of_id }
                |> withNoCmd

        ToggleMediaSensitive ->
            { model | media_sensitive = not model.media_sensitive }
                |> withNoCmd

        SetSpoilerText spoiler_text ->
            { model | spoiler_text = spoiler_text }
                |> withNoCmd

        SetVisibility visibility ->
            { model | visibility = visibility }
                |> withNoCmd

        SetScheduledAt scheduled_at ->
            { model | scheduled_at = scheduled_at }
                |> withNoCmd

        SetIdempotencyKey idempotencyKey ->
            { model | idempotencyKey = idempotencyKey }
                |> withNoCmd

        GetMediaFile clearMedia ->
            if clearMedia then
                { model | mediaFile = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd
                        (File.Select.file imageMimeTypes
                            (ExplorerUIMsg << ReceiveMediaFile)
                        )

        ReceiveMediaFile file ->
            { model | mediaFile = Just file }
                |> withNoCmd

        SetMediaDescription mediaDescription ->
            { model | mediaDescription = mediaDescription }
                |> withNoCmd

        SetMediaFocusX x ->
            let
                mediaFocus =
                    model.mediaFocus
            in
            { model | mediaFocus = { mediaFocus | x = x } }
                |> withNoCmd

        SetMediaFocusY y ->
            let
                mediaFocus =
                    model.mediaFocus
            in
            { model | mediaFocus = { mediaFocus | y = y } }
                |> withNoCmd

        SetMediaId media_id ->
            { model | media_id = media_id }
                |> withNoCmd

        SetMediaIds media_ids ->
            { model | media_ids = media_ids }
                |> withNoCmd

        SetExpiresIn expires_in ->
            { model | expires_in = expires_in }
                |> withNoCmd

        ToggleMultiple ->
            { model | multiple = not model.multiple }
                |> withNoCmd

        ToggleHideTotals ->
            { model | hide_totals = not model.hide_totals }
                |> withNoCmd

        RemovePollOption ->
            let
                pollOptions =
                    model.pollOptions

                len =
                    max 2 <| List.length pollOptions - 1
            in
            { model | pollOptions = List.take len pollOptions }
                |> withNoCmd

        AddPollOption ->
            let
                pollOptions =
                    model.pollOptions
            in
            ( if List.length pollOptions >= 4 then
                model

              else
                { model | pollOptions = List.concat [ pollOptions, [ "" ] ] }
            , Cmd.none
            )

        SetPollOption idx option ->
            { model | pollOptions = LE.setAt idx option model.pollOptions }
                |> withNoCmd

        ToggleLocal ->
            { model | local = not model.local }
                |> withNoCmd

        SetHashtag hashtag ->
            { model | hashtag = hashtag }
                |> withNoCmd

        SetListId listId ->
            { model | listId = listId }
                |> withNoCmd


{-| Process Requests sent from the columns page.

These send requests over the wire to instances.

-}
explorerSendMsg : ExplorerSendMsg -> Model -> ( Model, Cmd Msg )
explorerSendMsg msg model =
    case msg of
        ReceiveResponse request result ->
            receiveResponse request result model

        SendNothing ->
            model |> withNoCmd

        SendGetInstance ->
            sendRequest (InstanceRequest Request.GetInstance) model

        SendGetActivity ->
            sendRequest (InstanceRequest Request.GetActivity) model

        SendGetPeers ->
            sendRequest (InstanceRequest Request.GetPeers) model

        SendGetTrends ->
            sendRequest (TrendsRequest Request.GetTrends) model

        SendGetVerifyCredentials ->
            sendRequest (AccountsRequest Request.GetVerifyCredentials) model

        SendGetAccountByUsername ->
            sendRequest
                (AccountsRequest <|
                    Request.GetAccountByUsername
                        { username = getUsername model }
                )
                model

        SendGetAccount ->
            sendRequest
                (AccountsRequest <|
                    Request.GetAccount
                        { id = getAccountId model }
                )
                model

        SendGetFollowers ->
            sendRequest
                (AccountsRequest <|
                    Request.GetFollowers
                        { id = getAccountId model
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendGetFollowing ->
            sendRequest
                (AccountsRequest <|
                    Request.GetFollowing
                        { id = getAccountId model
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendGetStatuses ->
            sendRequest
                (AccountsRequest <|
                    Request.GetStatuses
                        { id = model.accountId
                        , only_media = model.onlyMedia
                        , pinned = model.pinned
                        , exclude_replies = model.excludeReplies
                        , paging = pagingInputToPaging model.pagingInput
                        , exclude_reblogs = model.excludeReblogs
                        }
                )
                model

        SendGetRelationships ->
            sendRequest
                (AccountsRequest <|
                    Request.GetRelationships
                        { ids =
                            String.split "," model.accountIds
                                |> List.map String.trim
                        }
                )
                model

        SendGetScheduledStatuses ->
            sendRequest
                (ScheduledStatusesRequest Request.GetScheduledStatuses)
                model

        SendGetScheduledStatus ->
            sendRequest
                (ScheduledStatusesRequest <|
                    Request.GetScheduledStatus
                        { id = model.scheduledStatusId }
                )
                model

        SendPutScheduledStatus ->
            sendRequest
                (ScheduledStatusesRequest <|
                    Request.PutScheduledStatus
                        { id = model.scheduledStatusId
                        , scheduled_at = nothingIfBlank model.scheduled_at
                        }
                )
                model

        SendDeleteScheduledStatus ->
            sendRequest
                (ScheduledStatusesRequest <|
                    Request.DeleteScheduledStatus { id = model.scheduledStatusId }
                )
                model

        SendGetSearchAccounts ->
            sendRequest
                (AccountsRequest <|
                    Request.GetSearchAccounts
                        { q = model.q
                        , limit = String.toInt model.pagingInput.limit
                        , resolve = model.resolve
                        , following = model.following
                        }
                )
                model

        SendPostFollow ->
            sendRequest
                (AccountsRequest <|
                    Request.PostFollow
                        { id = model.accountId
                        , reblogs = model.followReblogs
                        }
                )
                model

        SendPostUnfollow ->
            sendRequest
                (AccountsRequest <|
                    Request.PostUnfollow { id = model.accountId }
                )
                model

        SendPatchUpdateCredentials ->
            sendPatchUpdateCredentials model

        SendGetBlocks ->
            sendRequest
                (BlocksRequest <|
                    Request.GetBlocks
                        { limit = String.toInt model.pagingInput.limit }
                )
                model

        SendPostBlock ->
            sendRequest
                (BlocksRequest <|
                    Request.PostBlock { id = model.accountId }
                )
                model

        SendPostUnblock ->
            sendRequest
                (BlocksRequest <|
                    Request.PostUnblock { id = model.accountId }
                )
                model

        SendGetCustomEmojis ->
            sendRequest (CustomEmojisRequest Request.GetCustomEmojis)
                model

        SendGetEndorsements ->
            sendRequest (EndorsementsRequest Request.GetEndorsements)
                model

        SendPostPinAccount ->
            sendRequest
                (EndorsementsRequest <|
                    Request.PostPinAccount { id = model.accountId }
                )
                model

        SendPostUnpinAccount ->
            sendRequest
                (EndorsementsRequest <|
                    Request.PostUnpinAccount { id = model.accountId }
                )
                model

        SendGetFavourites ->
            sendRequest
                (FavouritesRequest <|
                    Request.GetFavourites
                        { limit = String.toInt model.pagingInput.limit }
                )
                model

        SendPostFavourite ->
            sendRequest
                (FavouritesRequest <|
                    Request.PostFavourite { id = model.statusId }
                )
                model

        SendPostUnfavourite ->
            sendRequest
                (FavouritesRequest <|
                    Request.PostUnfavourite { id = model.statusId }
                )
                model

        SendGetFilters ->
            sendRequest (FiltersRequest Request.GetFilters)
                model

        SendGetFilter ->
            sendRequest
                (FiltersRequest <|
                    Request.GetFilter { id = model.filterId }
                )
                model

        SendPostFilter ->
            let
                { phrase, context, irreversible, whole_word, expires_in } =
                    model.filterInput
            in
            sendRequest
                (FiltersRequest <|
                    Request.PostFilter
                        { phrase = phrase
                        , context = context
                        , irreversible = irreversible
                        , whole_word = whole_word
                        , expires_in = String.toInt expires_in
                        }
                )
                model

        SendPutFilter ->
            let
                { phrase, context, irreversible, whole_word, expires_in } =
                    model.filterInput
            in
            sendRequest
                (FiltersRequest <|
                    Request.PutFilter
                        { id = model.filterId
                        , phrase = phrase
                        , context = context
                        , irreversible = irreversible
                        , whole_word = whole_word
                        , expires_in = String.toInt expires_in
                        }
                )
                model

        SendDeleteFilter ->
            sendRequest
                (FiltersRequest <|
                    Request.DeleteFilter { id = model.filterId }
                )
                model

        SendGetFollowRequests ->
            sendRequest
                (FollowRequestsRequest <|
                    Request.GetFollowRequests
                        { limit = String.toInt model.pagingInput.limit }
                )
                model

        SendPostAuthorizeFollow ->
            sendRequest
                (FollowRequestsRequest <|
                    Request.PostAuthorizeFollow { id = model.accountId }
                )
                model

        SendPostRejectFollow ->
            sendRequest
                (FollowRequestsRequest <|
                    Request.PostRejectFollow { id = model.accountId }
                )
                model

        SendGetFollowSuggestions ->
            sendRequest
                (FollowSuggestionsRequest <|
                    Request.GetFollowSuggestions
                )
                model

        SendDeleteFollowSuggestions ->
            sendRequest
                (FollowSuggestionsRequest <|
                    Request.DeleteFollowSuggestions
                        { account_id = model.accountId }
                )
                model

        SendGetGroups ->
            sendRequest
                (GroupsRequest <| Request.GetGroups { tab = model.whichGroups })
                model

        SendGetGroupRemovedAccounts ->
            sendRequest
                (GroupsRequest <|
                    Request.GetGroupRemovedAccounts { id = model.groupId }
                )
                model

        SendGetGroup ->
            sendRequest
                (GroupsRequest <| Request.GetGroup { id = model.groupId })
                model

        SendGetGroupAccounts ->
            sendRequest
                (GroupsRequest <| Request.GetGroupAccounts { id = model.groupId })
                model

        SendGetGroupRelationships ->
            sendRequest
                (GroupsRequest <|
                    Request.GetGroupRelationships
                        { ids =
                            String.split "," model.groupIds
                                |> List.map String.trim
                        }
                )
                model

        SendPostGroupJoin ->
            sendRequest
                (GroupsRequest <|
                    Request.PostGroupJoin { id = model.groupId }
                )
                model

        SendDeleteGroupJoin ->
            sendRequest
                (GroupsRequest <|
                    Request.DeleteGroupJoin { id = model.groupId }
                )
                model

        SendPostGroupRemovedAccounts ->
            sendRequest
                (GroupsRequest <|
                    Request.PostGroupRemovedAccounts
                        { id = model.groupId
                        , account_id = model.accountId
                        }
                )
                model

        SendDeleteGroupRemovedAccounts ->
            sendRequest
                (GroupsRequest <|
                    Request.DeleteGroupRemovedAccounts
                        { id = model.groupId
                        , account_id = model.accountId
                        }
                )
                model

        SendPatchGroupAddAdministrator ->
            sendRequest
                (GroupsRequest <|
                    Request.PatchGroupAddAdministrator
                        { id = model.groupId
                        , account_id = model.accountId
                        }
                )
                model

        SendDeleteGroupStatus ->
            sendRequest
                (GroupsRequest <|
                    Request.DeleteGroupStatus
                        { id = model.groupId
                        , status_id = model.statusId
                        }
                )
                model

        SendPostGroup ->
            sendRequest
                (GroupsRequest <|
                    Request.PostGroup
                        { title = model.groupTitle
                        , description = model.groupDescription
                        , cover_image = model.groupCoverImage
                        }
                )
                model

        SendPutGroup ->
            sendRequest
                (GroupsRequest <|
                    Request.PutGroup
                        { id = model.groupId
                        , title = nothingIfBlank model.groupTitle
                        , description = nothingIfBlank model.groupDescription
                        , cover_image = model.groupCoverImage
                        }
                )
                model

        SendGetLists ->
            sendRequest
                (ListsRequest Request.GetLists)
                model

        SendGetList ->
            sendRequest
                (ListsRequest <|
                    Request.GetList { id = model.listId }
                )
                model

        SendGetListAccounts ->
            sendRequest
                (ListsRequest <|
                    Request.GetListAccounts
                        { id = model.listId
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendGetAccountLists ->
            sendRequest
                (ListsRequest <|
                    Request.GetAccountLists { id = model.accountId }
                )
                model

        SendPostList ->
            sendRequest
                (ListsRequest <|
                    Request.PostList { title = model.listTitle }
                )
                model

        SendPutList ->
            sendRequest
                (ListsRequest <|
                    Request.PutList
                        { id = model.listId
                        , title = model.listTitle
                        }
                )
                model

        SendDeleteList ->
            sendRequest
                (ListsRequest <|
                    Request.DeleteList { id = model.listId }
                )
                model

        SendPostListAccounts ->
            sendRequest
                (ListsRequest <|
                    Request.PostListAccounts
                        { id = model.listId
                        , account_ids =
                            String.split "," model.accountIds
                                |> List.map String.trim
                        }
                )
                model

        SendDeleteListAccounts ->
            sendRequest
                (ListsRequest <|
                    Request.DeleteListAccounts
                        { id = model.listId
                        , account_ids =
                            String.split "," model.accountIds
                                |> List.map String.trim
                        }
                )
                model

        SendGetAccountMutes ->
            sendRequest
                (MutesRequest <|
                    Request.GetAccountMutes
                        { limit = String.toInt model.pagingInput.limit }
                )
                model

        SendPostAccountMute ->
            sendRequest
                (MutesRequest <|
                    Request.PostAccountMute
                        { id = model.accountId
                        , notifications = model.muteNotifications
                        }
                )
                model

        SendPostAccountUnmute ->
            sendRequest
                (MutesRequest <|
                    Request.PostAccountUnmute { id = model.accountId }
                )
                model

        SendPostStatusMute ->
            sendRequest
                (MutesRequest <|
                    Request.PostStatusMute { id = model.statusId }
                )
                model

        SendPostStatusUnmute ->
            sendRequest
                (MutesRequest <|
                    Request.PostStatusUnmute { id = model.statusId }
                )
                model

        SendGetSearch ->
            sendRequest
                (SearchRequest <|
                    Request.GetSearch
                        { q = model.q
                        , resolve = model.resolve
                        , limit = String.toInt model.pagingInput.limit
                        , offset = String.toInt model.offset
                        , following = model.following
                        }
                )
                model

        SendGetNotifications ->
            sendRequest
                (NotificationsRequest <|
                    Request.GetNotifications
                        { paging = pagingInputToPaging model.pagingInput
                        , exclude_types = model.excludedNotificationTypes
                        , account_id = nothingIfBlank model.notificationsAccountId
                        }
                )
                model

        SendGetNotification ->
            sendRequest
                (NotificationsRequest <|
                    Request.GetNotification { id = model.notificationId }
                )
                model

        SendPostDismissNotification ->
            sendRequest
                (NotificationsRequest <|
                    Request.PostDismissNotification { id = model.notificationId }
                )
                model

        SendPostClearNotifications ->
            sendRequest
                (NotificationsRequest <|
                    Request.PostClearNotifications
                )
                model

        SendPostReports ->
            sendRequest
                (ReportsRequest <|
                    Request.PostReports
                        { account_id = model.accountId
                        , status_ids =
                            String.split "," model.statusIds
                                |> List.map String.trim
                        , comment = nothingIfBlank model.reportComment
                        , forward = model.forwardReport
                        }
                )
                model

        SendGetStatus ->
            sendRequest
                (StatusesRequest <| Request.GetStatus { id = model.statusId })
                model

        SendGetStatusContext ->
            sendRequest
                (StatusesRequest <| Request.GetStatusContext { id = model.statusId })
                model

        SendGetStatusCard ->
            sendRequest
                (StatusesRequest <| Request.GetStatusCard { id = model.statusId })
                model

        SendGetStatusRebloggedBy ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusRebloggedBy
                        { id = model.statusId
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendGetStatusFavouritedBy ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusFavouritedBy
                        { id = model.statusId
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendDeleteStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.DeleteStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostReblogStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostReblogStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostUnreblogStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostUnreblogStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostPinStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostPinStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostUnpinStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostUnpinStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostStatus
                        { status = nothingIfBlank model.status
                        , in_reply_to_id = nothingIfBlank model.in_reply_to_id
                        , group_id = nothingIfBlank model.groupId
                        , quote_of_id = nothingIfBlank model.quote_of_id
                        , media_ids =
                            splitMediaIds model.media_ids
                        , poll = pollDefinition model
                        , sensitive = model.media_sensitive
                        , spoiler_text = nothingIfBlank model.spoiler_text
                        , visibility = model.visibility
                        , scheduled_at = nothingIfBlank model.scheduled_at
                        , language = nothingIfBlank model.language
                        , idempotencyKey = nothingIfBlank model.idempotencyKey
                        }
                )
                { model | dialog = NoDialog }

        SendPostMedia ->
            case ( model.mediaFile, parseFocus model.mediaFocus ) of
                ( Just file, Just focus ) ->
                    sendRequest
                        (MediaAttachmentsRequest <|
                            Request.PostMedia
                                { file = file
                                , description = nothingIfBlank model.mediaDescription
                                , focus = focus
                                }
                        )
                        model

                _ ->
                    { model | msg = Just "Missing media file or malformed focus." }
                        |> withNoCmd

        SendPutMedia ->
            case parseFocus model.mediaFocus of
                Nothing ->
                    model |> withNoCmd

                Just focus ->
                    sendRequest
                        (MediaAttachmentsRequest <|
                            Request.PutMedia
                                { id = model.media_id
                                , description = nothingIfBlank model.mediaDescription
                                , focus = focus
                                }
                        )
                        model

        SendGetHomeTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetHomeTimeline
                        { paging = pagingInputToPaging model.pagingInput }
                )
                model

        SendGetConversations ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetConversations
                        { paging = pagingInputToPaging model.pagingInput }
                )
                model

        SendGetProTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetProTimeline
                        { only_media = False --not supported, yet
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetPublicTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetPublicTimeline
                        { local = model.local
                        , only_media = model.onlyMedia
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetTagTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetTagTimeline
                        { hashtag = model.hashtag
                        , local = model.local
                        , only_media = model.onlyMedia
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetListTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetListTimeline
                        { list_id = model.listId
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetGroupTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetGroupTimeline
                        { group_id = model.groupId
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model


taggedValueToString : TaggedValue -> String
taggedValueToString taggedValue =
    case taggedValue of
        TString string ->
            string

        TFloat float ->
            String.fromFloat float

        TBool bool ->
            if bool then
                "true"

            else
                "false"

        TNull ->
            "null"

        _ ->
            ""


findKeyPath : String -> JsonTree.Node -> Maybe JsonTree.TaggedValue
findKeyPath keyPath node =
    if keyPath == node.keyPath then
        Just node.value

    else
        let
            loop nodes =
                case nodes of
                    [] ->
                        Nothing

                    first :: rest ->
                        case findKeyPath keyPath first of
                            Nothing ->
                                loop rest

                            res ->
                                res
        in
        case node.value of
            TList nodes ->
                loop nodes

            TDict dict ->
                Dict.values dict
                    |> loop

            _ ->
                Nothing


sendPatchUpdateCredentials model =
    case model.account of
        Nothing ->
            model |> withNoCmd

        Just account ->
            let
                { displayName, note, avatarFile, headerFile } =
                    model

                { locked, privacy, sensitive, language } =
                    model

                ( maybeSourceUpdate, sourceNote, fields ) =
                    case account.source of
                        -- May need to compare with model fields.
                        -- If so, need to source from them as well.
                        Nothing ->
                            ( Nothing, "", [] )

                        Just source ->
                            let
                                sourceUpdate =
                                    { privacy =
                                        ifNotEqual privacy source.privacy
                                    , sensitive =
                                        ifNotEqual sensitive source.sensitive
                                    , language =
                                        let
                                            lang =
                                                if language == "" then
                                                    Nothing

                                                else
                                                    Just language
                                        in
                                        ifNotEqual lang source.language
                                    }
                            in
                            if
                                (sourceUpdate.privacy == Nothing)
                                    && (sourceUpdate.sensitive == Nothing)
                                    && (sourceUpdate.language == Nothing)
                            then
                                ( Nothing, source.note, source.fields )

                            else
                                ( Just sourceUpdate, source.note, source.fields )

                request =
                    AccountsRequest <|
                        Request.PatchUpdateCredentials
                            { display_name =
                                ifNotEqual displayName account.display_name
                            , note =
                                ifNotEqual note sourceNote
                            , avatar =
                                model.avatarFile
                            , header =
                                model.headerFile
                            , locked =
                                ifNotEqual locked account.locked
                            , source =
                                maybeSourceUpdate
                            , fields_attributes =
                                let
                                    flds =
                                        extendFields fields
                                in
                                if Debug.log "fields" flds == Debug.log "model.fields" model.fields then
                                    Nothing

                                else
                                    Just <|
                                        List.map fieldToUpdate model.fields
                            }
            in
            -- This sends the request even if nothing is updated.
            -- Maybe it shouldn't.
            sendRequest request model


fieldToUpdate : Field -> FieldUpdate
fieldToUpdate { name, value } =
    { name = name
    , value = value
    }


ifNotEqual : a -> a -> Maybe a
ifNotEqual new old =
    if new == old then
        Nothing

    else
        Just new


getAccountIdRelationships : Bool -> Model -> Cmd Msg
getAccountIdRelationships showResult model =
    case model.account of
        Nothing ->
            Cmd.none

        Just account ->
            case model.renderEnv.loginServer of
                Nothing ->
                    Cmd.none

                Just server ->
                    case model.accountId of
                        "" ->
                            Cmd.none

                        accountId ->
                            Request.serverRequest
                                (\sr res ->
                                    GlobalMsg <|
                                        ReceiveAccountIdRelationships sr res
                                )
                                []
                                { server = server
                                , token = model.token
                                }
                                showResult
                                (AccountsRequest <|
                                    Request.GetRelationships
                                        { ids = [ accountId ] }
                                )


getUsername : Model -> String
getUsername model =
    let
        username =
            model.username
    in
    if username /= "" then
        username

    else
        case model.account of
            Just account ->
                account.username

            Nothing ->
                ""


getAccountId : Model -> String
getAccountId model =
    let
        id =
            model.accountId
    in
    if id /= "" then
        id

    else
        case model.account of
            Just account ->
                account.id

            Nothing ->
                ""


handleFeatureProbeError : Request -> Error -> Model -> ( Model, Bool )
handleFeatureProbeError request error model =
    case LE.find (\( _, r ) -> r == request) model.featureProbeRequests of
        Nothing ->
            ( model, False )

        Just pair ->
            let
                ( server, _ ) =
                    pair

                mdl =
                    setServerHasFeature
                        (Debug.log "Server does NOT support groups" <|
                            Just server
                        )
                        featureNames.groups
                        False
                        model
            in
            ( { mdl
                | featureProbeRequests =
                    LE.remove pair mdl.featureProbeRequests
              }
            , True
            )


{-| Here when we get a response back from sending a Request.

Errors are not always errors. If the request was Post(Un)ReblogStatus
or Post(Un)FavouriteStatus, an error may mean that you changed it in
another browser. Pleroma FE assumes that. We should reget the status
and updateColumnsStatus with the result.

Let them report it as a bug.

-}
receiveResponse : Request -> Result Error Response -> Model -> ( Model, Cmd Msg )
receiveResponse request result model =
    case result of
        Err err ->
            let
                threeStrikes =
                    ( Nothing, Nothing, Nothing )

                ( msg, ( response, entity, metadata ) ) =
                    case err of
                        BadUrl url ->
                            ( Just <| "BadUrl: " ++ url, threeStrikes )

                        Timeout ->
                            ( Just "Timeout", threeStrikes )

                        NetworkError ->
                            ( Just "Network Error", threeStrikes )

                        BadStatus meta status ->
                            ( Just <| "Bad status: " ++ status, ( Nothing, Nothing, Just meta ) )

                        BadBody meta jderr json ->
                            let
                                res =
                                    case JD.decodeString JD.value json of
                                        Err _ ->
                                            Nothing

                                        Ok v ->
                                            Just v

                                m =
                                    Just <| "BadBody: " ++ decodeErrorToString jderr
                            in
                            ( m, ( res, Nothing, Just meta ) )

                ( supportsAccountByUsername, msg2 ) =
                    let
                        dict =
                            model.supportsAccountByUsername
                    in
                    case model.renderEnv.loginServer of
                        Nothing ->
                            ( dict, msg )

                        Just server ->
                            case request of
                                AccountsRequest (Request.GetAccountByUsername _) ->
                                    ( Dict.insert server False dict, Nothing )

                                _ ->
                                    ( dict, msg )

                ( mdl3, msg3 ) =
                    -- This code handles (un)Reblog or (un)Favorite when
                    -- that state is displayed differently than it is,
                    -- becase it was changed in another client.
                    if msg2 == Nothing then
                        ( model, msg2 )

                    else
                        case err of
                            BadStatus _ _ ->
                                let
                                    ( postReblogOrFavorite, id ) =
                                        splitReblogOrFavoriteRequest request
                                in
                                case postReblogOrFavorite of
                                    Nothing ->
                                        ( model, msg2 )

                                    Just rof ->
                                        ( fixReblogOrFavorite
                                            id
                                            rof
                                            model
                                        , Nothing
                                        )

                            _ ->
                                ( model, msg2 )

                ( mdl4, msg4 ) =
                    if msg3 == Nothing then
                        ( mdl3, msg3 )

                    else
                        case fixPostStateMedia request model of
                            Nothing ->
                                ( mdl3, msg3 )

                            Just m ->
                                ( m, Nothing )

                ( mdl5, wasFeatureProbe ) =
                    handleFeatureProbeError request err mdl4

                msg5 =
                    if wasFeatureProbe then
                        Nothing

                    else
                        msg4

                handleFailedSearch mdl6 msg6 =
                    if mdl6.popup /= NoPopup then
                        ( { mdl6
                            | nextSearch = Cmd.none
                            , searchActive = mdl6.nextSearch /= Cmd.none
                          }
                        , mdl6.nextSearch
                        , Nothing
                        )

                    else
                        ( mdl6, Cmd.none, msg6 )

                ( mdl7, cmd7, msg7 ) =
                    case Debug.log "Error request" request of
                        TimelinesRequest (Request.GetPublicTimeline _) ->
                            ( { mdl5 | pagingInput = emptyPagingInput }
                            , Task.perform ExplorerSendMsg <|
                                Task.succeed <|
                                    Debug.log "  " SendGetProTimeline
                            , Nothing
                            )

                        SearchRequest (Request.GetSearch _) ->
                            handleFailedSearch mdl5 msg5

                        AccountsRequest (Request.GetSearchAccounts _) ->
                            handleFailedSearch mdl5 msg5

                        _ ->
                            ( mdl5, Cmd.none, msg5 )
            in
            { mdl7
                | msg = msg7
                , response = response
                , entity = entity
                , metadata = metadata
                , selectedKeyPath = ""
                , selectedKeyValue = ""
                , supportsAccountByUsername = supportsAccountByUsername
            }
                |> updateJsonTrees
                |> withCmd cmd7

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model

                cmd =
                    mdl.sideEffectCmd
            in
            { mdl
                | msg = Nothing
                , metadata = Just response.metadata
                , response = Just <| ED.entityValue response.entity
                , entity = Just response.entity
                , sideEffectCmd = Cmd.none
            }
                |> updateJsonTrees
                |> withCmd cmd


fixPostStateMedia : Request -> Model -> Maybe Model
fixPostStateMedia request model =
    case request of
        MediaAttachmentsRequest req ->
            case req of
                Request.PostMedia _ ->
                    -- Remove the file.
                    -- May want to support retry.
                    let
                        postState =
                            model.postState

                        fileNames =
                            postState.fileNames

                        len =
                            List.length fileNames

                        fileUrls =
                            postState.fileUrls
                    in
                    Just
                        { model
                            | postState =
                                { postState
                                    | fileNames = List.take (len - 1) fileNames
                                    , fileUrls =
                                        if List.length fileUrls == len then
                                            List.take (len - 1) fileUrls

                                        else
                                            fileUrls
                                }
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


type PostReblogOrFavorite
    = PostReblog
    | PostUnreblog
    | PostFavorite
    | PostUnfavorite


splitReblogOrFavoriteRequest : Request -> ( Maybe PostReblogOrFavorite, String )
splitReblogOrFavoriteRequest request =
    case request of
        StatusesRequest req ->
            case req of
                Request.PostReblogStatus { id } ->
                    ( Just PostReblog, id )

                Request.PostUnreblogStatus { id } ->
                    ( Just PostUnreblog, id )

                _ ->
                    ( Nothing, "" )

        FavouritesRequest req ->
            case req of
                Request.PostFavourite { id } ->
                    ( Just PostFavorite, id )

                Request.PostUnfavourite { id } ->
                    ( Just PostUnfavorite, id )

                _ ->
                    ( Nothing, "" )

        _ ->
            ( Nothing, "" )


{-| Fix reblogged or favorited in all the Statuses in the model's feedSet.
-}
fixReblogOrFavorite : String -> PostReblogOrFavorite -> Model -> Model
fixReblogOrFavorite id postReblogOrFavorite model =
    let
        modifier status =
            case postReblogOrFavorite of
                PostReblog ->
                    if status.reblogged then
                        status

                    else
                        { status
                            | reblogged = True
                            , reblogs_count = status.reblogs_count + 1
                        }

                PostUnreblog ->
                    if status.reblogged then
                        { status
                            | reblogged = False
                            , reblogs_count = status.reblogs_count - 1
                        }

                    else
                        status

                PostFavorite ->
                    if status.favourited then
                        status

                    else
                        { status
                            | favourited = True
                            , favourites_count = status.favourites_count + 1
                        }

                PostUnfavorite ->
                    if status.favourited then
                        { status
                            | favourited = False
                            , favourites_count = status.favourites_count - 1
                        }

                    else
                        status
    in
    modifyColumnsStatus id modifier model


decodeErrorToString : JD.Error -> String
decodeErrorToString error =
    case error of
        JD.Field field err ->
            "Error on field \"" ++ field ++ "\": " ++ decodeErrorToString err

        JD.Index idx err ->
            "Error on index " ++ String.fromInt idx ++ "\": " ++ decodeErrorToString err

        JD.OneOf errors ->
            case errors of
                [] ->
                    "OneOf []"

                err :: _ ->
                    decodeErrorToString err

        JD.Failure fail value ->
            -- TODO: encode some part of `value`.
            fail


applyProTimelineSideEffects : Response -> Model -> Model
applyProTimelineSideEffects response model =
    let
        loginServer =
            model.renderEnv.loginServer

        mdl =
            if
                serverHasFeature loginServer
                    featureNames.proFeed
                    model
            then
                model

            else
                setServerHasFeature loginServer
                    featureNames.proFeed
                    True
                    model

        feedSet =
            mdl.feedSet

        publicFeedType =
            PublicFeed { flags = Nothing }

        proFeedType =
            ProFeed { flags = Nothing }
    in
    case findFeed proFeedType model.feedSet of
        Just _ ->
            mdl

        Nothing ->
            case findFeed publicFeedType model.feedSet of
                Nothing ->
                    mdl

                Just feed ->
                    case response.entity of
                        StatusListEntity statuses ->
                            let
                                proFeed =
                                    { feedType = proFeedType
                                    , elements =
                                        StatusElements statuses
                                    , newElements = 0
                                    }

                                newFeedSet =
                                    { feedSet
                                        | feeds =
                                            LE.setIf ((==) feed)
                                                proFeed
                                                feedSet.feeds
                                    }

                                feedSetDefinition =
                                    model.feedSetDefinition

                                newFeedSetDefinition =
                                    { feedSetDefinition
                                        | feedTypes =
                                            LE.setIf ((==) publicFeedType)
                                                proFeedType
                                                feedSetDefinition.feedTypes
                                    }
                            in
                            { mdl
                                | feedSet = newFeedSet
                                , feedSetDefinition = newFeedSetDefinition
                            }

                        _ ->
                            mdl


applyResponseSideEffects : Response -> Model -> Model
applyResponseSideEffects response model =
    case response.request of
        AccountsRequest Request.GetVerifyCredentials ->
            case response.entity of
                AccountEntity account ->
                    updatePatchCredentialsInputs model

                _ ->
                    model

        AccountsRequest (Request.GetAccountByUsername _) ->
            let
                mdl =
                    case model.renderEnv.loginServer of
                        Nothing ->
                            model

                        Just server ->
                            { model
                                | supportsAccountByUsername =
                                    Dict.insert server
                                        True
                                        model.supportsAccountByUsername
                            }
            in
            case response.entity of
                AccountEntity { id } ->
                    { mdl | accountId = id }

                _ ->
                    mdl

        AccountsRequest (Request.PostFollow _) ->
            { model | isAccountFollowed = True }

        AccountsRequest (Request.PostUnfollow _) ->
            { model | isAccountFollowed = False }

        AccountsRequest (Request.PatchUpdateCredentials _) ->
            case response.entity of
                AccountEntity account ->
                    { model
                        | account = Just account
                        , avatarFile = Nothing
                        , headerFile = Nothing
                    }
                        |> updatePatchCredentialsInputs

                _ ->
                    model

        AccountsRequest (Request.GetStatuses { paging }) ->
            statusSmartPaging response.entity paging model

        AccountsRequest (Request.GetSearchAccounts _) ->
            -- Similar to `SearchRequest (Request.GetSearch)` below.
            case response.entity of
                AccountListEntity results ->
                    let
                        mdl =
                            { model
                                | sideEffectCmd = model.nextSearch
                                , nextSearch = Cmd.none
                                , searchActive = model.nextSearch /= Cmd.none
                            }
                    in
                    case mdl.popup of
                        UserNamePopup ->
                            let
                                choices =
                                    List.map AccountChoice results
                            in
                            { mdl
                                | popupChoices = choices
                                , popup =
                                    if choices == [] then
                                        NoPopup

                                    else
                                        mdl.popup
                            }

                        _ ->
                            mdl

                _ ->
                    model

        CustomEmojisRequest Request.GetCustomEmojis ->
            case response.entity of
                EmojiListEntity emojis ->
                    let
                        names =
                            Debug.log "emojis" <|
                                List.map .shortcode (List.take 20 emojis)

                        renderEnv =
                            model.renderEnv

                        list =
                            List.map (\emoji -> ( emoji.shortcode, emoji )) emojis

                        dict =
                            Dict.fromList list
                    in
                    { model
                        | renderEnv =
                            { renderEnv
                                | emojis = dict
                                , emojisList = List.map Tuple.second list
                            }
                    }

                _ ->
                    model

        FiltersRequest (Request.PostFilter _) ->
            case response.entity of
                FilterEntity { id } ->
                    { model | filterId = id }

                _ ->
                    model

        GroupsRequest (Request.GetGroups _) ->
            setServerHasFeature
                (Debug.log "Server supports groups" model.renderEnv.loginServer)
                featureNames.groups
                True
                model

        GroupsRequest (Request.GetGroup _) ->
            case response.entity of
                GroupEntity group ->
                    let
                        mdl =
                            { model
                                | groupDict = Dict.insert group.id group model.groupDict
                            }

                        feedType =
                            GroupFeed group.id
                    in
                    case findFeed feedType model.feedSet of
                        Nothing ->
                            mdl

                        Just _ ->
                            addFeedEnv feedType mdl

                _ ->
                    model

        GroupsRequest (Request.PostGroup _) ->
            case response.entity of
                GroupEntity { id } ->
                    { model
                        | groupId = id
                        , groupTitle = ""
                        , groupDescription = ""
                        , groupCoverImage = Nothing
                    }

                _ ->
                    model

        GroupsRequest (Request.PutGroup _) ->
            { model
                | groupTitle = ""
                , groupDescription = ""
                , groupCoverImage = Nothing
            }

        NotificationsRequest (Request.GetNotifications { paging }) ->
            notificationsSmartPaging response.entity paging model

        ListsRequest Request.GetLists ->
            case response.entity of
                ListEntityListEntity lists ->
                    let
                        mdl =
                            { model
                                | lists = lists
                                , selectedList =
                                    case model.selectedList of
                                        Nothing ->
                                            Nothing

                                        Just { id } ->
                                            case
                                                LE.find (\list -> id == list.id)
                                                    lists
                                            of
                                                Nothing ->
                                                    Nothing

                                                justList ->
                                                    justList
                            }
                    in
                    List.foldl addFeedEnv mdl mdl.feedSetDefinition.feedTypes

                _ ->
                    model

        SearchRequest (Request.GetSearch _) ->
            -- Similar to `AccountsRequest (Request.GetSearchAccounts)` above.
            case response.entity of
                ResultsEntity results ->
                    let
                        mdl =
                            { model
                                | sideEffectCmd = model.nextSearch
                                , nextSearch = Cmd.none
                                , searchActive = model.nextSearch /= Cmd.none
                            }

                        choices =
                            case mdl.popup of
                                GroupNamePopup ->
                                    List.map GroupChoice results.groups

                                HashtagPopup ->
                                    List.map HashtagChoice results.hashtags

                                PostGroupPopup ->
                                    List.map PostGroupChoice results.groups

                                PostTextPopup search ->
                                    case search.popupType of
                                        PostPopupAtsign ->
                                            List.map AccountChoice results.accounts

                                        PostPopupSharp ->
                                            List.map HashtagChoice results.hashtags

                                        _ ->
                                            []

                                _ ->
                                    []
                    in
                    { mdl
                        | popupChoices = choices
                        , popup =
                            if choices == [] then
                                NoPopup

                            else
                                mdl.popup
                    }

                _ ->
                    model

        StatusesRequest (Request.PostStatus _) ->
            case response.entity of
                StatusEntity status ->
                    { model
                        | statusId = status.id
                        , status = ""
                        , in_reply_to_id = ""
                        , quote_of_id = ""
                        , media_sensitive = False
                        , spoiler_text = ""
                        , scheduled_at = ""
                        , idempotencyKey = ""
                        , pollOptions = [ "", "" ]
                    }
                        |> adjustColumnsForPost status

                _ ->
                    model

        MediaAttachmentsRequest mediaReq ->
            case response.entity of
                AttachmentEntity { id } ->
                    let
                        postState =
                            model.postState

                        ( mdl, isExplorer ) =
                            case mediaReq of
                                Request.PostMedia { file } ->
                                    if Just (File.name file) == LE.last postState.fileNames then
                                        -- From the post dialog on the
                                        -- Columns page.
                                        ( { model
                                            | postState =
                                                { postState
                                                    | media_ids =
                                                        postState.media_ids ++ [ id ]
                                                }
                                          }
                                        , False
                                        )

                                    else
                                        -- From the "POST media" button on the
                                        -- API Explorer page.
                                        ( { model
                                            | media_id = id
                                            , media_ids =
                                                splitMediaIds model.media_ids
                                                    |> (\ids ->
                                                            List.concat [ ids, [ id ] ]
                                                       )
                                                    |> String.join ","
                                          }
                                        , True
                                        )

                                _ ->
                                    ( model, False )
                    in
                    if isExplorer then
                        { mdl
                            | mediaFile = Nothing
                            , mediaDescription = ""
                            , mediaFocus = { x = "", y = "" }
                        }

                    else
                        mdl

                _ ->
                    model

        TimelinesRequest req ->
            case req of
                Request.GetHomeTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetConversations { paging } ->
                    case response.entity of
                        ConversationListEntity conversations ->
                            smartPaging conversations .id paging model

                        _ ->
                            model

                Request.GetProTimeline { paging } ->
                    let
                        mdl2 =
                            statusSmartPaging response.entity paging model
                    in
                    applyProTimelineSideEffects response mdl2

                Request.GetPublicTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetTagTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetListTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetGroupTimeline { paging } ->
                    statusSmartPaging response.entity paging model

        StatusesRequest (Request.PostReblogStatus _) ->
            -- The entity is the reblog, not the reblogged
            -- We need to udpate the reblogged
            case response.entity of
                StatusEntity status ->
                    case status.reblog of
                        Nothing ->
                            model

                        Just (WrappedStatus reblog) ->
                            modifyColumnsStatus reblog.id (\_ -> reblog) model

                _ ->
                    model

        StatusesRequest (Request.PostUnreblogStatus _) ->
            updateColumnsStatus response.entity model

        StatusesRequest (Request.GetStatusContext { id }) ->
            updateThreadExplorer id response.entity model

        FavouritesRequest (Request.PostFavourite _) ->
            updateColumnsStatus response.entity model

        FavouritesRequest (Request.PostUnfavourite _) ->
            updateColumnsStatus response.entity model

        _ ->
            model


{-| Just received an update to a StatusEntity.

That happens when a reply is posted (on refetch of the replied-to
post), or the user clicks on a posts's reblog or favorite button.

Change the status in all the places it appears in a feed.

-}
updateColumnsStatus : Entity -> Model -> Model
updateColumnsStatus entity model =
    case entity of
        StatusEntity status ->
            modifyColumnsStatus status.id (\_ -> status) model

        _ ->
            model


modifyColumnsStatus : String -> (Status -> Status) -> Model -> Model
modifyColumnsStatus id modifier model =
    let
        feedSet =
            model.feedSet

        popupExplorer =
            case model.popupExplorer of
                ThreadPopupExplorer state ->
                    let
                        updateScrolledStatus scrolledStatus =
                            { scrolledStatus
                                | status =
                                    modifyStatus id modifier scrolledStatus.status
                                        |> Tuple.first
                                , displayed =
                                    modifyStatuses id modifier scrolledStatus.displayed
                                        |> Tuple.first
                            }
                    in
                    { state
                        | ribbon =
                            List.map updateScrolledStatus state.ribbon
                    }
                        |> ThreadPopupExplorer

                explorer ->
                    explorer
    in
    { model
        | feedSet =
            { feedSet
                | feeds =
                    List.map (modifyFeedStatus id modifier) feedSet.feeds
            }
        , popupExplorer = popupExplorer
    }


{-| Replace a `Status` everywhere it appears in a `Feed`.

If it does not appear, the result will be the input Feed, not a copy of it.

-}
modifyFeedStatus : String -> (Status -> Status) -> Feed -> Feed
modifyFeedStatus id modifier feed =
    let
        ( elements, changed ) =
            modifyFeedElements id modifier feed.elements
    in
    if changed then
        { feed | elements = elements }

    else
        feed


modifyFeedElements : String -> (Status -> Status) -> FeedElements -> ( FeedElements, Bool )
modifyFeedElements id modifier elements =
    case elements of
        StatusElements statuses ->
            let
                ( stats, changed ) =
                    modifyStatuses id modifier statuses
            in
            if changed then
                ( StatusElements stats, True )

            else
                ( elements, False )

        NotificationElements notifications ->
            let
                ( nots, changed ) =
                    modifyNotifications id modifier notifications
            in
            if changed then
                ( NotificationElements nots, True )

            else
                ( elements, False )

        _ ->
            ( elements, False )


modifyStatus : String -> (Status -> Status) -> Status -> ( Status, Bool )
modifyStatus id modifier oldStatus =
    if id == oldStatus.id then
        ( modifier oldStatus, True )

    else
        let
            ( oldStat2, chngd2 ) =
                case oldStatus.reblog of
                    Nothing ->
                        ( oldStatus, False )

                    Just (WrappedStatus reblog) ->
                        let
                            ( reblog3, chngd3 ) =
                                modifyStatus id modifier reblog
                        in
                        ( { oldStatus
                            | reblog =
                                Just <| WrappedStatus reblog3
                          }
                        , chngd3
                        )
        in
        case oldStat2.quote of
            Nothing ->
                ( oldStat2, chngd2 )

            Just (WrappedStatus quote) ->
                let
                    ( quote4, chngd4 ) =
                        modifyStatus id modifier quote
                in
                ( { oldStat2
                    | quote =
                        Just <| WrappedStatus quote4
                  }
                , chngd4
                )


modifyStatuses : String -> (Status -> Status) -> List Status -> ( List Status, Bool )
modifyStatuses id modifier statuses =
    let
        folder : Status -> ( List Status, Bool ) -> ( List Status, Bool )
        folder stat ( result, chngd ) =
            let
                ( stat2, chngd2 ) =
                    modifyStatus id modifier stat
            in
            ( stat2 :: result, chngd || chngd2 )

        ( stats, changed ) =
            List.foldr folder ( [], False ) statuses
    in
    if changed then
        ( stats, True )

    else
        ( statuses, False )


modifyNotifications : String -> (Status -> Status) -> List Notification -> ( List Notification, Bool )
modifyNotifications id modifier notifications =
    let
        folder : Notification -> ( List Notification, Bool ) -> ( List Notification, Bool )
        folder notification ( nots, chngd ) =
            case notification.status of
                Nothing ->
                    ( notification :: nots, chngd )

                Just stat ->
                    let
                        ( stat2, chngd2 ) =
                            modifyStatus id modifier stat
                    in
                    if chngd2 then
                        ( { notification | status = Just stat2 } :: nots
                        , True
                        )

                    else
                        ( notification :: nots, chngd )
    in
    List.foldr folder ( [], False ) notifications


splitMediaIds : String -> List String
splitMediaIds string =
    let
        s =
            String.trim string
    in
    if s == "" then
        []

    else
        String.split "," s
            |> List.map String.trim


quoteFieldDecoder : Decoder Value
quoteFieldDecoder =
    JD.field "quote" JD.value


quoteFieldExists : Status -> Bool
quoteFieldExists status =
    case JD.decodeValue quoteFieldDecoder status.v of
        Ok _ ->
            True

        _ ->
            False


{-| This relies on the fact that if the quote field is supported,
it is always passed, with a null value if there is no quoted post.
-}
updateQuoteFeature : List Status -> Model -> Model
updateQuoteFeature statuses model =
    let
        maybeServer =
            model.renderEnv.loginServer
    in
    case statuses of
        status :: _ ->
            case serverKnowsFeature maybeServer featureNames.quote model of
                Just _ ->
                    model

                Nothing ->
                    setServerHasFeature maybeServer
                        featureNames.quote
                        (quoteFieldExists status)
                        model

        _ ->
            model


statusSmartPaging : Entity -> Maybe Paging -> Model -> Model
statusSmartPaging entity paging model =
    case entity of
        StatusListEntity statuses ->
            let
                mdl =
                    updateQuoteFeature statuses model
            in
            smartPaging statuses .id paging mdl

        _ ->
            model


notificationsSmartPaging : Entity -> Maybe Paging -> Model -> Model
notificationsSmartPaging entity paging model =
    case entity of
        NotificationListEntity notifications ->
            smartPaging notifications .id paging model

        _ ->
            model


smartPaging : List a -> (a -> String) -> Maybe Paging -> Model -> Model
smartPaging entities getid paging model =
    let
        ( limit, ( max_id, min_id, since_id ) ) =
            case paging of
                Nothing ->
                    -- use the API default here?
                    ( 1, ( "", "", "" ) )

                Just pag ->
                    ( Maybe.withDefault 1 pag.limit
                    , ( Maybe.withDefault "" pag.max_id
                      , Maybe.withDefault "" pag.min_id
                      , Maybe.withDefault "" pag.since_id
                      )
                    )

        pagingInput =
            model.pagingInput
    in
    if not model.smartPaging then
        model

    else if since_id /= "" then
        model

    else if min_id /= "" then
        if max_id /= "" then
            model

        else
            case List.head entities of
                Nothing ->
                    model

                Just e ->
                    { model | pagingInput = { pagingInput | min_id = getid e } }

    else if limit <= List.length entities then
        case LE.last entities of
            Nothing ->
                model

            Just e ->
                { model | pagingInput = { pagingInput | max_id = getid e } }

    else
        model


sendRequest : Request -> Model -> ( Model, Cmd Msg )
sendRequest request =
    sendGeneralRequest (ExplorerSendMsg << ReceiveResponse request) request


sendGeneralRequest : (Result Error Response -> Msg) -> Request -> Model -> ( Model, Cmd Msg )
sendGeneralRequest tagger request model =
    case model.renderEnv.loginServer of
        Nothing ->
            model |> withNoCmd

        Just server ->
            let
                rawRequest =
                    Request.requestToRawRequest []
                        { server = server
                        , token = model.token
                        }
                        request
            in
            { model
                | request = Just rawRequest
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
                , msg = Nothing
            }
                |> withCmd
                    (Request.rawRequestToCmd tagger rawRequest)


saveAuthorization : String -> Authorization -> Model -> ( Model, Cmd Msg )
saveAuthorization server authorization model =
    let
        tokens =
            model.tokens

        mdl =
            { model
                | tokens =
                    Dict.insert server
                        authorization.token
                        tokens
            }
    in
    mdl
        |> withCmds
            [ putToken server <| Just authorization.token
            , fetchFeatures server mdl
            ]


serverOption : String -> String -> Html Msg
serverOption currentServer server =
    option
        [ value server
        , selected <| server == currentServer
        ]
        [ text server ]


serverSelect : Model -> Html Msg
serverSelect model =
    let
        currentServer =
            case model.renderEnv.loginServer of
                Nothing ->
                    ""

                Just server ->
                    server
    in
    select [ onInput (GlobalMsg << SetServer) ]
        (option [ value "" ]
            [ text "-- select a server --" ]
            :: (List.map (serverOption currentServer) <| Dict.keys model.tokens)
        )


whichGroupsSelect : Model -> Html Msg
whichGroupsSelect model =
    let
        whichGroups =
            model.whichGroups
    in
    select [ onInput (ExplorerUIMsg << SetWhichGroups) ]
        [ option
            [ value "Member"
            , selected <| Request.MemberGroups == whichGroups
            ]
            [ text "Member" ]
        , option
            [ value "Featured"
            , selected <| Request.FeaturedGroups == whichGroups
            ]
            [ text "Featured" ]
        , option
            [ value "Admin"
            , selected <| Request.AdminGroups == whichGroups
            ]
            [ text "Admin" ]
        ]


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


type alias StyleProperties =
    { backgroundColor : String
    , inputBackground : String
    , color : String
    , popupChoiceClass : String
    , highlightStatusColor : String
    , repliedToStatusColor : String
    , visitedStatusColor : String
    }


type Style
    = DarkStyle
    | LightStyle


styles :
    { dark : StyleProperties
    , light : StyleProperties
    }
styles =
    { dark =
        { backgroundColor = "#222"
        , inputBackground = "#333"
        , color = "#eee"
        , popupChoiceClass = "popup-choice-dark"
        , highlightStatusColor = "darkblue"
        , repliedToStatusColor = "darkslategray"
        , visitedStatusColor = "#444"
        }
    , light =
        { backgroundColor = "white"
        , inputBackground = "white"
        , color = "black"
        , popupChoiceClass = "popup-choice-light"
        , highlightStatusColor = "#fed8b1"
        , repliedToStatusColor = "gainsboro"
        , visitedStatusColor = "#ececec"
        }
    }


getStyle : Style -> StyleProperties
getStyle style =
    case style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light


{-| Choose the visible section of the user interface.

`MediaAttachmentsRequest` and `PollsRequest` are done as part of `StatusesSelected`.

`TrendsRequest` is part of `InstanceSelected`.

-}
type SelectedRequest
    = LoginSelected
    | InstanceSelected
    | AccountsSelected
    | BlocksSelected
    | CustomEmojisSelected
    | EndorsementsSelected
    | FavouritesSelected
    | FiltersSelected
    | FollowRequestsSelected
    | FollowSuggestionsSelected
    | GroupsSelected
    | ListsSelected
    | MutesSelected
    | NotificationsSelected
    | ReportsSelected
    | SearchSelected
    | ScheduledStatusesSelected
    | StatusesSelected
    | TimelinesSelected
    | TrendsSelected


encodeSelectedRequest : SelectedRequest -> Value
encodeSelectedRequest selectedRequest =
    JE.string <| selectedRequestToString selectedRequest


selectedRequestToString : SelectedRequest -> String
selectedRequestToString selectedRequest =
    case selectedRequest of
        LoginSelected ->
            "login"

        InstanceSelected ->
            "Instance Information"

        AccountsSelected ->
            "AccountsRequest"

        BlocksSelected ->
            "BlocksRequest"

        CustomEmojisSelected ->
            "CustomEmojisRequest"

        EndorsementsSelected ->
            "EndorsementsRequest"

        FavouritesSelected ->
            "FavouritesRequest"

        FiltersSelected ->
            "FiltersRequest"

        FollowRequestsSelected ->
            "FollowRequestsRequest"

        FollowSuggestionsSelected ->
            "FollowSuggestionsRequest"

        GroupsSelected ->
            "GroupsRequest"

        ListsSelected ->
            "ListsRequest"

        MutesSelected ->
            "MutesRequest"

        SearchSelected ->
            "SearchRequest"

        ScheduledStatusesSelected ->
            "ScheduledStatusRequest"

        NotificationsSelected ->
            "NotificationsRequest"

        ReportsSelected ->
            "ReportsRequest"

        StatusesSelected ->
            "StatusesRequest"

        TimelinesSelected ->
            "TimelinesRequest"

        TrendsSelected ->
            "TrendsRequest"


selectedRequestDecoder : Decoder SelectedRequest
selectedRequestDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                JD.succeed <|
                    selectedRequestFromString s
            )


selectedRequestUrlDictPairs : List ( String, SelectedRequest )
selectedRequestUrlDictPairs =
    [ ( "instance", InstanceSelected )
    , ( "accounts", AccountsSelected )
    , ( "blocks", BlocksSelected )
    , ( "customemojis", CustomEmojisSelected )
    , ( "endorsements", EndorsementsSelected )
    , ( "favourites", FavouritesSelected )
    , ( "filters", FiltersSelected )
    , ( "followrequests", FollowRequestsSelected )
    , ( "followsuggestions", FollowSuggestionsSelected )
    , ( "groups", GroupsSelected )
    , ( "lists", ListsSelected )
    , ( "mutes", MutesSelected )
    , ( "search", SearchSelected )
    , ( "scheduledstatus", ScheduledStatusesSelected )
    , ( "notifications", NotificationsSelected )
    , ( "reports", ReportsSelected )
    , ( "statuses", StatusesSelected )
    , ( "timelines", TimelinesSelected )
    , ( "trends", TrendsSelected )
    , ( "login", LoginSelected )
    ]


selectedRequestToUrlValue : SelectedRequest -> String
selectedRequestToUrlValue request =
    case LE.find (\( _, req ) -> req == request) selectedRequestUrlDictPairs of
        Nothing ->
            -- Can't happen
            "login"

        Just ( str, _ ) ->
            str


selectedRequestFromUrlDict : Dict String SelectedRequest
selectedRequestFromUrlDict =
    Dict.fromList selectedRequestUrlDictPairs


selectedRequestFromStringDict : Dict String SelectedRequest
selectedRequestFromStringDict =
    Dict.fromList
        [ ( "Instance Information", InstanceSelected )
        , ( "AccountsRequest", AccountsSelected )
        , ( "BlocksRequest", BlocksSelected )
        , ( "CustomEmojisRequest", CustomEmojisSelected )
        , ( "EndorsementsRequest", EndorsementsSelected )
        , ( "FavouritesRequest", FavouritesSelected )
        , ( "FiltersRequest", FiltersSelected )
        , ( "FollowRequestsRequest", FollowRequestsSelected )
        , ( "FollowSuggestionsRequest", FollowSuggestionsSelected )
        , ( "GroupsRequest", GroupsSelected )
        , ( "ListsRequest", ListsSelected )
        , ( "MutesRequest", MutesSelected )
        , ( "SearchRequest", SearchSelected )
        , ( "ScheduledStatusRequest", ScheduledStatusesSelected )
        , ( "NotificationsRequest", NotificationsSelected )
        , ( "ReportsRequest", ReportsSelected )
        , ( "StatusesRequest", StatusesSelected )
        , ( "TimelinesRequest", TimelinesSelected )
        , ( "TrendsRequest", TrendsSelected )
        , ( "Login", LoginSelected )
        ]


selectedRequestFromString : String -> SelectedRequest
selectedRequestFromString s =
    case Dict.get s selectedRequestFromStringDict of
        Nothing ->
            LoginSelected

        Just request ->
            request


radioNames =
    { selectedRequest = "selectedRequest"
    , privacy = "privacy"
    , visibility = "visibility"
    }


radioButton : { buttonValue : a, radioValue : a, radioName : String, setter : Msg, label : String } -> Html Msg
radioButton { buttonValue, radioValue, radioName, setter, label } =
    span []
        [ input
            [ type_ "radio"
            , name radioName
            , value label
            , checked <| buttonValue == radioValue
            , onCheck <|
                \checked ->
                    if checked then
                        setter

                    else
                        Noop
            ]
            []
        , span
            [ onClick setter
            , style "cursor" "default"
            ]
            [ text label ]
        ]


selectedRequestRadio : SelectedRequest -> Model -> Html Msg
selectedRequestRadio selectedRequest model =
    let
        string =
            selectedRequestToString selectedRequest
    in
    radioButton
        { buttonValue = selectedRequest
        , radioValue = model.selectedRequest
        , radioName = string
        , setter = ExplorerUIMsg <| SetSelectedRequest selectedRequest
        , label = string
        }


selectedRequestHtml : SelectedRequest -> String -> Model -> (Model -> Html Msg) -> Html Msg
selectedRequestHtml selectedRequest url model body =
    span []
        [ selectedRequestRadio selectedRequest model
        , if url == "" then
            text ""

          else
            span []
                [ text " ("
                , link "docs" url
                , text ")"
                ]
        , br
        , if selectedRequest /= model.selectedRequest then
            text ""

          else
            body model
        ]


privacyRadio : Privacy -> String -> Model -> Html Msg
privacyRadio privacy label model =
    radioButton
        { buttonValue = privacy
        , radioValue = model.privacy
        , radioName = radioNames.privacy
        , setter = ExplorerUIMsg <| SetPrivacy privacy
        , label = label
        }


type alias ImageSpec =
    { imageUrl : String
    , linkUrl : String
    , altText : String
    , borderColor : Maybe String
    , h : String
    }


imageLink : ImageSpec -> Html Msg
imageLink spec =
    a
        [ href spec.linkUrl
        , blankTarget
        ]
        [ imageFromSpec spec ]


imageFromSpec : ImageSpec -> Html Msg
imageFromSpec { imageUrl, altText, borderColor, h } =
    img
        [ src imageUrl
        , alt altText
        , style "height" h
        , title altText
        , style "border" <|
            case borderColor of
                Just color ->
                    "3px solid " ++ color

                Nothing ->
                    "none"
        ]
        []


link : String -> String -> Html Msg
link label url =
    a
        [ href url
        , blankTarget
        ]
        [ text label ]


blankTarget : Attribute msg
blankTarget =
    target "_blank"


{-| The Material Design CSS puts paragraph spacing BELOW the paragraph.

Use this to make a paragraph worth of vertical white space.

-}
pspace : Html msg
pspace =
    p [] [ text "" ]


titledButton : String -> Bool -> Msg -> String -> Html Msg
titledButton theTitle enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        , title theTitle
        ]
        [ b label ]


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton =
    titledButton ""


button : Msg -> String -> Html Msg
button =
    enabledButton True


view : Model -> Document Msg
view model =
    let
        { backgroundColor, color } =
            getStyle model.renderEnv.style
    in
    { title = "Mammudeck"
    , body =
        [ BodyColors.bodyColors
            [ BodyColors.color color
            , BodyColors.backgroundColor backgroundColor
            ]
            []
        , renderPopupExplorer model
        , renderDialog model
        , renderPopup model
        , let
            pos =
                model.postInputPosition

            cnt =
                model.postInputCount
          in
          TextAreaTracker.textAreaTracker
            [ TextAreaTracker.textAreaId nodeIds.postDialogText
            , TextAreaTracker.triggerCoordinates model.postTriggerCoordinatesCount
            , TextAreaTracker.onCoordinates (ColumnsUIMsg << ReceiveCoordinates)
            , TextAreaTracker.setSelection pos pos cnt
            ]
            []
        , div [ id "body" ]
            [ case model.page of
                HomePage ->
                    renderHome model

                ColumnsPage ->
                    renderColumns model

                ExplorerPage ->
                    renderExplorer model
            ]
        ]
    }


pageSelector : Bool -> Bool -> Page -> Html Msg
pageSelector showLabel showColumns page =
    span []
        [ if showLabel then
            b "Page: "

          else
            text ""
        , select [ onInput (GlobalMsg << SetPage) ]
            [ if showColumns then
                option
                    [ value "ColumnsPage"
                    , selected <| page == ColumnsPage
                    ]
                    [ text "Columns" ]

              else
                text ""
            , option
                [ value "HomePage"
                , selected <| page == HomePage
                ]
                [ text "Home" ]
            , option
                [ value "ExplorerPage"
                , selected <| page == ExplorerPage
                ]
                [ text "API Explorer" ]
            ]
        ]


renderCenteredScreen : Model -> String -> List (Html msg) -> Html msg
renderCenteredScreen model width body =
    let
        { backgroundColor, color } =
            getStyle model.renderEnv.style
    in
    div
        [ style "background-color" backgroundColor
        , style "padding" "0 0 0 0"
        , style "margin" "0"
        , style "width" "auto"
        ]
        [ div
            [ style "color" color
            , style "background-color" backgroundColor
            , style "padding" "0 5px 0 5px"
            , style "max-width" "fill-available"
            , style "width" width
            , style "margin" "auto"
            ]
            body
        ]


renderHome : Model -> Html Msg
renderHome model =
    let
        renderEnv =
            model.renderEnv

        screenWidth =
            renderEnv.windowSize |> Tuple.first

        divWidth =
            min screenWidth 500

        imgWidth =
            8 * divWidth // 10
    in
    renderCenteredScreen model
        (px divWidth)
        [ h2 [ style "text-align" "center" ]
            [ text "Mammudeck" ]
        , pageSelector True (model.renderEnv.loginServer /= Nothing) model.page
        , if model.renderEnv.loginServer == Nothing then
            p []
                [ text "Enter a 'server' name and click 'Login'."
                ]

          else
            primaryServerLine model
        , loginSelectedUI False model
        , Markdown.toHtml []
            """
Mammudeck is a TweetDeck-like columnar interface to Mastodon/Pleroma. It is a work in progress. Keep an eye on the "Columns" page for new features. Use the "API Explorer" page to do low-level API hacking.

[Wikipedia says](https://en.wikipedia.org/wiki/Mastodon) that "Mastodons... are any species of extinct proboscideans in the genus Mammut (family Mammutidae), distantly related to elephants..." I removed the ending "t" from "Mammut" and added "deck" to get "Mammudeck".

    """
        , p []
            [ img
                [ src "images/mammoth-500x360.png"
                , width imgWidth
                , alt "Mammoth"
                , style "text-align" "center"
                ]
                []
            ]
        , Markdown.toHtml []
            """
To get started, enter the domain of a Fediverse server on which you have an account, e.g. "mastodon.online", in the text entry field below "Server:" above, and click the "Login" button. You will be redirected to that server to login, and to authorize Mammudeck to access your account. You can login to multiple sites, and Mammudeck will remember them, and their access tokens, in your browser's local storage database, on your computer. NO information about you is EVER stored on Mammudeck.com, though the server you log in to will create one or more cookies to remember your login there.

You may choose amongst known sites in the pop-up below "Server:", then click the "Login" button, (or click the "Server" button in the left column of the "Columns" page, which is where you'll spend most of your time).

There's a huge list of servers at [fediverse.network](https://fediverse.network/). This webapp doesn't know how to register a new account (yet), so you'll have to do that on the server's web site, then come back here to log in.

Mammudeck is a labor of love, but I wouldn't at all mind it becoming a full-time job. That can only happen if you, my customers, support me. If you use it, and like it, please donate at [paypal.me/billstclair](https://www.paypal.me/billstclair).
            """
        , p [ style "text-align" "center" ]
            [ checkBox (ColumnsUIMsg ToggleStyle)
                (model.renderEnv.style == DarkStyle)
                "Dark Mode"
            , br
            , link "@billstclair@impeccable.social"
                "https://impeccable.social/billstclair"
            , br
            , link "@billstclair@gab.com"
                "https://gab.com/billstclair"
            , br
            , text <| "Copyright " ++ special.copyright ++ " 2019-2020, Bill St. Clair"
            , br
            , imageLink
                { imageUrl = "images/elm-logo-125x125.png"
                , linkUrl = "https://elm-lang.org/"
                , altText = "Elm Inside"
                , borderColor = Nothing
                , h = "32px"
                }
            , text " "
            , imageLink
                { imageUrl =
                    if model.renderEnv.style == DarkStyle then
                        "images/GitHub-Mark-Light-32px.png"

                    else
                        "images/GitHub-Mark-32px.png"
                , linkUrl = "https://github.com/billstclair/mammudeck"
                , altText = "GitHub"
                , borderColor = Nothing
                , h = "32px"
                }
            ]
        ]


leftColumnWidth : Int
leftColumnWidth =
    120


scaledLeftColumnWidth : RenderEnv -> Int
scaledLeftColumnWidth renderEnv =
    let
        pct =
            case String.toInt renderEnv.fontSize of
                Nothing ->
                    100.0

                Just i ->
                    i |> toFloat
    in
    (leftColumnWidth |> toFloat) * pct / 100 |> round


fspct : RenderEnv -> String
fspct renderEnv =
    renderEnv.fontSize ++ "%"


fsStyle : RenderEnv -> Attribute msg
fsStyle renderEnv =
    style "font-size" <| fspct renderEnv


renderLeftColumn : RenderEnv -> Html Msg
renderLeftColumn renderEnv =
    let
        { color } =
            getStyle renderEnv.style
    in
    div
        [ style "color" color
        , style "width" <| px (scaledLeftColumnWidth renderEnv)
        , style "padding-top" "5px"
        ]
        [ p []
            [ titledButton "Show server information. Switch servers."
                True
                (ColumnsUIMsg <| ShowServerDialog)
                "server"
            ]
        , p [] [ pageSelector False (renderEnv.loginServer /= Nothing) ColumnsPage ]
        , p []
            [ checkBox (ColumnsUIMsg ToggleStyle)
                (renderEnv.style == DarkStyle)
                "dark"
            ]
        , p []
            [ text "font"
            , text " "
            , titledButton "Restore the font size to the default."
                True
                (ColumnsUIMsg <| ResetFontSize)
                labels.x
            , br
            , titledButton "Increase the font size by 5% of the default."
                True
                (ColumnsUIMsg <| FontSize Up)
                labels.up
            , text special.nbsp
            , titledButton "Decrease the font size by 5% of the default."
                True
                (ColumnsUIMsg <| FontSize Down)
                labels.down
            ]
        , p
            []
            [ text "width"
            , br
            , titledButton "Increase the column width."
                True
                (ColumnsUIMsg <| ColumnWidth Up)
                labels.up
            , text special.nbsp
            , titledButton "Decrease the column width."
                True
                (ColumnsUIMsg <| ColumnWidth Down)
                labels.down
            ]

        --, p []
        --  [ button (ColumnsUIMsg <| ClearFeatures) "clear" ]
        , p []
            [ button (ColumnsUIMsg ShowEditColumnsDialog) "edit" ]
        , p []
            [ button (ColumnsUIMsg ShowSettingsDialog) "settings" ]
        , p []
            [ button (ColumnsUIMsg ShowSaveRestoreDialog) "save" ]
        , p []
            [ button (ColumnsUIMsg ShowKeyboardShortcutsDialog) "keyboard" ]
        , p []
            [ button (ColumnsUIMsg ReloadAllColumns) "reload" ]
        , p []
            [ button (ColumnsUIMsg <| ShowPostDialog Nothing) "post" ]
        ]


labels =
    { up = special.nbsp ++ "^" ++ special.nbsp
    , down = special.nbsp ++ "v" ++ special.nbsp
    , x = special.nbsp ++ "X" ++ special.nbsp
    }


{-| This is here to be close to the `renderLeftColumn` code, which it subsumes.
-}
settingsDialog : Model -> Html Msg
settingsDialog model =
    let
        renderEnv =
            model.renderEnv
    in
    dialogRender
        renderEnv
        { styles =
            [ ( "width", "40em" )
            , ( "max-width", "95%" )
            , ( "max-height", "90%" )
            , ( "overflow", "auto" )
            , ( "font-size", fspct model.renderEnv )
            ]
        , title = "Settings"
        , content =
            settingsDialogContent model
        , actionBar =
            [ button (ColumnsUIMsg DismissDialog) "OK"
            ]
        }
        True


settingsDialogContent : Model -> List (Html Msg)
settingsDialogContent model =
    let
        renderEnv =
            model.renderEnv

        { inputBackground, color } =
            getStyle renderEnv.style
    in
    [ p []
        [ primaryServerLine model ]
    , p []
        [ loginSelectedUI False model ]
    , p [] [ pageSelector True (renderEnv.loginServer /= Nothing) ColumnsPage ]
    , p [] [ b "Actions:" ]
    , p []
        [ button (ColumnsUIMsg ShowEditColumnsDialog)
            "Edit Columns Dialog"
        , br
        , button (ColumnsUIMsg ShowSaveRestoreDialog)
            "Save/Restore Dialog"
        , br
        , button (ColumnsUIMsg ShowKeyboardShortcutsDialog)
            "Keyboard Shortcuts Dialog"
        , br
        , button (ColumnsUIMsg ReloadAllColumns)
            "Reload All Columns"
        , br
        , button (ColumnsUIMsg <| ShowPostDialog Nothing)
            "Post Dialog"
        ]
    , p [] [ b "Appearance:" ]
    , table []
        [ tr []
            [ td [ style "padding-right" "0.5em" ]
                [ text "font"
                , text " "
                , titledButton "Restore the font size to the default."
                    True
                    (ColumnsUIMsg <| ResetFontSize)
                    labels.x
                , br
                , titledButton "Increase the font size by 5% of the default."
                    True
                    (ColumnsUIMsg <| FontSize Up)
                    labels.up
                , text special.nbsp
                , titledButton "Decrease the font size by 5% of the default."
                    True
                    (ColumnsUIMsg <| FontSize Down)
                    labels.down
                ]
            , td [ style "padding-right" "0.5em" ]
                [ text "column width"
                , br
                , titledButton "Increase the column width."
                    True
                    (ColumnsUIMsg <| ColumnWidth Up)
                    labels.up
                , text special.nbsp
                , titledButton "Decrease the column width."
                    True
                    (ColumnsUIMsg <| ColumnWidth Down)
                    labels.down
                ]
            , td []
                [ checkBox (ColumnsUIMsg ToggleStyle)
                    (renderEnv.style == DarkStyle)
                    "dark"
                ]
            ]
        ]
    , br
    , p []
        [ text "If you hide both the left column and the scroll pill, "
        , text "you will need to remember keyboard shortcuts. "
        , text "? shows them. "
        , text "On mobile, the Post Dialog will save you. "
        , text "Reply to a post. "
        ]
    , let
        columnText =
            if model.showLeftColumn then
                "Hide Left Column"

            else
                "Show Left Column"

        pillText =
            if model.scrollPillState.showScrollPill then
                "Hide Scroll Pill"

            else
                "Show Scroll Pill"

        serverText =
            if model.scrollPillState.showServer then
                "Hide Server under Scroll Pill"

            else
                "Show Server under Scroll Pill"
      in
      p []
        [ button (ColumnsUIMsg ToggleShowLeftColumn) columnText
        , text " "
        , button (ColumnsUIMsg ToggleShowScrollPill) pillText
        , br
        , button (ColumnsUIMsg ToggleShowScrollPillServer) serverText
        ]
    , p [] [ b "Advanced:" ]
    , p []
        [ text "Reload the page after doing this:"
        , br
        , button (ColumnsUIMsg ClearFeatures) "Clear saved server features"
        , br
        , br
        , text "Will ask you to confirm before clearing EVERYTHING:"
        , br
        , button (GlobalMsg ClearAllDialog) "Clear all persistent state!"
        ]
    ]


postToFeedMsg : FeedType -> RenderEnv -> Maybe Msg
postToFeedMsg feedType renderEnv =
    case feedType of
        UserFeed { username, server } ->
            Just
                (ColumnsUIMsg
                    (PostWithMention <|
                        usernameAtServer username server renderEnv
                    )
                )

        GroupFeed group_id ->
            Just (ColumnsUIMsg <| PostWithGroupId group_id)

        _ ->
            Nothing


headerFeedId : String -> String
headerFeedId feedId =
    feedId ++ " [header]"


renderFeed : Bool -> Int -> RenderEnv -> FeedEnv -> Feed -> Html Msg
renderFeed isFeedLoading newPostCount renderEnv feedEnv feed =
    let
        feedType =
            feed.feedType

        newElements =
            feed.newElements

        { color } =
            getStyle renderEnv.style

        ( _, h ) =
            renderEnv.windowSize

        innerHeight =
            --Debug.log "  innerHeight" <|
            case feedEnv.headerHeight of
                Nothing ->
                    "calc(100% - 1.4em)"

                Just headerHeight ->
                    String.fromFloat (toFloat h - 20 - headerHeight) ++ "px"

        feedId =
            Types.feedID <| Debug.log "renderFeed" feedType

        footer theElements =
            let
                notEmpty =
                    case theElements of
                        StatusElements statuses ->
                            statuses /= []

                        NotificationElements notifications ->
                            notifications /= []

                        _ ->
                            False
            in
            if isFeedLoading && notEmpty then
                renderFeedLoadingEmojiFooter renderEnv

            else
                text ""

        title =
            case feedType of
                GroupFeed _ ->
                    case feedEnv.group of
                        Just group ->
                            b group.title

                        _ ->
                            feedTitle feedType

                ListFeed _ ->
                    case feedEnv.list of
                        Just list ->
                            b list.title

                        _ ->
                            feedTitle feedType

                _ ->
                    feedTitle feedType
    in
    div
        [ style "height" <| px (h - 20)
        , style "width" <| px renderEnv.columnWidth
        , style "border" <| "1px solid " ++ color
        ]
        [ div
            [ style "border" <| "1px solid " ++ color
            , style "text-align" "center"
            , style "color" color
            , id <| headerFeedId feedId
            ]
            [ if isFeedLoading then
                feedLoadingEmojiSpan True True

              else if newPostCount > 0 then
                a
                    [ href "#"
                    , onClick (ColumnsUIMsg <| ShowNewFeedStatuses feedType)
                    , Html.Attributes.title "Show new"

                    --, style "border" <| "2px solid " ++ color
                    ]
                    [ text <|
                        special.nbsp
                            ++ String.fromInt newPostCount
                            ++ special.nbsp
                    ]

              else
                Html.i
                    [ onClick <| ColumnsUIMsg (RefreshFeed feedType)
                    , style "cursor" "pointer"
                    , style "font-size" smallTextFontSize
                    , class "icon-spin3"
                    ]
                    []
            , text " "
            , title
            , case postToFeedMsg feedType renderEnv of
                Nothing ->
                    text ""

                Just msg ->
                    span []
                        [ text " "
                        , Html.i
                            [ onClick msg
                            , style "cursor" "pointer"
                            , style "font-size" smallTextFontSize
                            , class "icon-pencil"
                            ]
                            []
                        ]
            , case feedType of
                GroupFeed _ ->
                    case feedEnv.group of
                        Just group ->
                            let
                                memberLine =
                                    text <| "(" ++ String.fromInt group.member_count ++ ") members"

                                memberLink =
                                    case groupUrl renderEnv group of
                                        Nothing ->
                                            memberLine

                                        Just url ->
                                            a [ href url ]
                                                [ memberLine ]
                            in
                            span []
                                [ br
                                , memberLink
                                ]

                        _ ->
                            text ""

                _ ->
                    text ""
            , if newElements <= 0 then
                text ""

              else
                span
                    [ style "text-align" "center"
                    , style "color" "red"
                    , style "cursor" "pointer"
                    , Html.Attributes.title "Click to mark read."
                    , onClick (ColumnsUIMsg <| MarkFeedRead feedType)
                    ]
                    [ br
                    , text <| String.fromInt newElements ++ " new"
                    ]
            ]
        , div
            [ style "overflow-y" "auto"
            , style "overflow-x" "hidden"
            , id feedId
            , style "height" innerHeight
            ]
          <|
            [ Lazy.lazy5 renderFeedElements
                newElements
                feedType
                renderEnv
                feedEnv
                feed.elements
            , footer feed.elements
            ]

        -- This turns scroll tracking back on, after the virtual DOM is synced.
        , RenderNotify.renderNotify
            [ RenderNotify.notifyValue <| JE.string feedId
            , RenderNotify.onRender (ColumnsUIMsg << FeedRendered)
            ]
            []
        ]


renderFeedElements : Int -> FeedType -> RenderEnv -> FeedEnv -> FeedElements -> Html Msg
renderFeedElements newElements feedType renderEnv feedEnv elements =
    case elements of
        StatusElements statuses ->
            div [] <|
                List.indexedMap
                    (renderStatusWithNewMarker feedType renderEnv feedEnv newElements)
                    statuses

        NotificationElements notifications ->
            let
                ( gangedNotifications, newElements2 ) =
                    gangNotifications newElements notifications
            in
            div [] <|
                List.indexedMap
                    (renderGangedNotificationWithNewMarker feedType
                        renderEnv
                        newElements
                    )
                    gangedNotifications

        _ ->
            text ""


renderGangedNotificationWithNewMarker : FeedType -> RenderEnv -> Int -> Int -> GangedNotification -> Html Msg
renderGangedNotificationWithNewMarker feedType renderEnv newElements index gangedNotification =
    if newElements > 0 && newElements == index then
        div []
            [ renderNewMarker feedType renderEnv
            , renderGangedNotification renderEnv gangedNotification
            ]

    else
        renderGangedNotification renderEnv gangedNotification


renderGangedNotification : RenderEnv -> GangedNotification -> Html Msg
renderGangedNotification renderEnv gangedNotification =
    let
        notification =
            gangedNotification.notification
    in
    case gangedNotification.accounts of
        account :: others ->
            if others == [] then
                renderNotification renderEnv notification

            else
                renderMultiNotification renderEnv
                    account
                    others
                    notification

        _ ->
            renderNotification renderEnv notification


renderMultiNotification : RenderEnv -> Account -> List Account -> Notification -> Html Msg
renderMultiNotification renderEnv account others notification =
    let
        { color } =
            getStyle renderEnv.style

        othersCount =
            List.length others

        display_name =
            account.display_name ++ " and " ++ String.fromInt othersCount ++ " others "

        description =
            notificationDescriptionWithDisplayName display_name notification renderEnv

        timeString =
            formatIso8601 renderEnv.here notification.created_at
    in
    div
        [ style "border" <| "1px solid " ++ color
        , style "color" color
        , style "padding" "0 3px"
        ]
        [ div [ headerFontSizeStyle ]
            [ description
            , br
            , text timeString
            , br
            , List.map
                (\other ->
                    imageLink
                        { imageUrl = other.avatar
                        , linkUrl = other.url
                        , altText = other.display_name
                        , borderColor =
                            if other.is_pro then
                                Just "gold"

                            else
                                Nothing
                        , h = "1.5em"
                        }
                )
                (account :: others)
                |> List.intersperse (text " ")
                |> span []
            ]
        , renderNotificationBody renderEnv notification
        ]


notificationStatusId : Notification -> String
notificationStatusId notification =
    case notification.status of
        Just { id } ->
            id

        Nothing ->
            ""


gangNotifications : Int -> List Notification -> ( List GangedNotification, Int )
gangNotifications newElements notifications =
    let
        loop : Int -> Int -> List Notification -> List GangedNotification -> ( List GangedNotification, Int )
        loop idx new tail res =
            case tail of
                [] ->
                    ( List.reverse res, new )

                car :: cdr ->
                    let
                        id =
                            notificationStatusId car
                    in
                    case
                        LE.find
                            (\gn ->
                                (id == gn.id)
                                    && (car.type_ == gn.notification.type_)
                            )
                            res
                    of
                        Nothing ->
                            loop (idx + 1) new cdr <|
                                { id = id
                                , notification = car
                                , accounts = [ car.account ]
                                }
                                    :: res

                        Just gn ->
                            let
                                newgn =
                                    { gn
                                        | accounts =
                                            List.append gn.accounts [ car.account ]
                                    }

                                newnew =
                                    if idx >= newElements then
                                        new

                                    else
                                        new - 1
                            in
                            loop (idx + 1) newnew cdr <|
                                LE.setIf ((==) gn) newgn res
    in
    loop 0 newElements notifications []


notificationDescription : Notification -> RenderEnv -> Html Msg
notificationDescription notification renderEnv =
    notificationDescriptionWithDisplayName notification.account.display_name
        notification
        renderEnv


renderDisplayName : String -> RenderEnv -> Html Msg
renderDisplayName display_name renderEnv =
    case parseEmojiString display_name of
        [ TextString _ ] ->
            b display_name

        emojisAndStrings ->
            let
                size =
                    18 * renderEnv.fontSizePct // 100

                sizeStr =
                    String.fromInt size ++ "px"
            in
            List.map (emojiStringToImg sizeStr renderEnv.emojis) emojisAndStrings
                |> Util.toVirtualDom
                |> Html.b []


notificationDescriptionWithDisplayName : String -> Notification -> RenderEnv -> Html Msg
notificationDescriptionWithDisplayName display_name notification renderEnv =
    let
        postName =
            if notification.type_ == PollNotification then
                text "poll"

            else
                text "your post"

        displayHtml =
            renderDisplayName display_name renderEnv
    in
    case notification.type_ of
        FollowNotification ->
            span [] [ displayHtml, text " followed you" ]

        MentionNotification ->
            span [] [ displayHtml, text " mentioned you" ]

        ReblogNotification ->
            span [] [ displayHtml, text " reblogged ", postName ]

        FavouriteNotification ->
            span [] [ displayHtml, text " favorited ", postName ]

        PollNotification ->
            span [] [ displayHtml, text "'s ", postName, text " is closed" ]


headerFontSize : String
headerFontSize =
    "90%"


headerFontSizeStyle : Attribute msg
headerFontSizeStyle =
    style "font-size" headerFontSize


renderNotification : RenderEnv -> Notification -> Html Msg
renderNotification renderEnv notification =
    let
        description =
            notificationDescription notification renderEnv

        { color } =
            getStyle renderEnv.style
    in
    div [ style "border" <| "1px solid " ++ color ]
        [ div []
            [ div [ headerFontSizeStyle ]
                [ renderAccount renderEnv.fontSizePct
                    color
                    renderEnv.here
                    notification.account
                    description
                    notification.created_at
                    Nothing
                ]
            , renderNotificationBody renderEnv notification
            ]
        ]


type EmojiOrTextString
    = EmojiString String
    | TextString String


validEmojiChar : Char -> Bool
validEmojiChar c =
    Char.isAlpha c || Char.isDigit c || c == '-' || c == '_'


validEmojiName : String -> Bool
validEmojiName string =
    -- Temporary
    let
        n =
            EmojiChar.findEmojiChars string
    in
    (String.length string > 0)
        && (String.toList string
                |> LE.find (\c -> not (validEmojiChar c))
                |> (==) Nothing
           )


parseEmojiString : String -> List EmojiOrTextString
parseEmojiString string =
    let
        substrs =
            String.split ":" string

        len =
            List.length substrs

        loop : List String -> List EmojiOrTextString -> List EmojiOrTextString
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

                [ last ] ->
                    List.reverse <|
                        case res of
                            (TextString x) :: more ->
                                TextString (x ++ last) :: more

                            _ ->
                                TextString last :: res

                first :: rest ->
                    if validEmojiName first then
                        case rest of
                            car :: cdr ->
                                loop rest <|
                                    TextString car
                                        :: EmojiString first
                                        :: res

                            _ ->
                                loop rest <| EmojiString first :: res

                    else
                        let
                            cF =
                                ":" ++ first
                        in
                        case res of
                            (TextString x) :: more ->
                                loop rest <| TextString (x ++ cF) :: more

                            _ ->
                                loop rest <| TextString cF :: res
    in
    case substrs of
        first :: rest ->
            loop rest [ TextString first ]

        _ ->
            [ TextString string ]


{-| <img draggable="false" style="height:16px;width:16px;margin:-3px 0 0;font-family:'object-fit:contain',inherit;vertical-align:middle;-o-object-fit:contain;object-fit:contain;" alt=":vomit:" title=":vomit:" src="https://gab.com/system/custom_emojis/images/000/008/280/original/bf13580b84754702.png?1563335294">
-}
emojiStringToImg : String -> Dict String Emoji -> EmojiOrTextString -> Html.Parser.Node
emojiStringToImg size dict emojiOrText =
    case emojiOrText of
        TextString s ->
            Html.Parser.Text s

        EmojiString s ->
            let
                tit =
                    ":" ++ s ++ ":"
            in
            case Dict.get s dict of
                Nothing ->
                    Html.Parser.Text <| ":" ++ s ++ ":"

                Just emoji ->
                    Html.Parser.Element "img"
                        [ ( "src", emoji.url )
                        , ( "draggable", "false" )
                        , ( "style", "height:" ++ size ++ ";" )
                        , ( "title", tit )
                        , ( "alt", tit )
                        ]
                        []


replaceEmojiReferences : RenderEnv -> List Html.Parser.Node -> List Html.Parser.Node
replaceEmojiReferences renderEnv nodes =
    let
        emojis =
            renderEnv.emojis

        size =
            18 * renderEnv.fontSizePct // 100

        sizeStr =
            String.fromInt size ++ "px"

        updater : Html.Parser.Node -> Html.Parser.Node
        updater node =
            case node of
                Html.Parser.Text string ->
                    case parseEmojiString string of
                        [] ->
                            node

                        [ TextString _ ] ->
                            node

                        emojisAndStrings ->
                            Html.Parser.Element "span"
                                []
                            <|
                                List.map (emojiStringToImg sizeStr renderEnv.emojis)
                                    emojisAndStrings

                Html.Parser.Element tag attrs subnodes ->
                    Html.Parser.Element tag attrs <|
                        replaceEmojiReferences renderEnv subnodes

                _ ->
                    node
    in
    List.map updater nodes


statusBody : RenderEnv -> Status -> List (Html Msg)
statusBody renderEnv status =
    case Html.Parser.run status.content of
        Ok nodes ->
            replaceEmojiReferences renderEnv nodes
                |> Util.toVirtualDom

        Err _ ->
            [ case status.plain_markdown of
                Just markdown ->
                    text markdown

                Nothing ->
                    text status.content
            ]


smallTextFontSize : String
smallTextFontSize =
    "80%"


renderNotificationBody : RenderEnv -> Notification -> Html Msg
renderNotificationBody renderEnv notification =
    let
        { color } =
            getStyle renderEnv.style
    in
    case notification.status of
        Nothing ->
            text ""

        Just status ->
            let
                body =
                    statusBody renderEnv status

                timeString =
                    formatIso8601 renderEnv.here status.created_at

                postLink =
                    renderStatusUrl timeString status
            in
            div []
                [ hr
                , div
                    [ class "content"
                    , style "color" color
                    ]
                  <|
                    List.concat
                        [ [ p [ style "font-size" smallTextFontSize ] [ postLink ] ]
                        , body
                        ]
                , renderMediaAttachments renderEnv status
                , renderStatusActions renderEnv status
                ]


feedTitle : FeedType -> Html Msg
feedTitle feedType =
    case feedType of
        HomeFeed ->
            b "Home"

        UserFeed { username, server } ->
            let
                serverString =
                    if server == "" then
                        ""

                    else
                        "@" ++ server
            in
            b <| username ++ serverString

        PublicFeed _ ->
            b "Public"

        ProFeed _ ->
            b "Pro"

        NotificationFeed _ ->
            b "Notifications"

        GroupFeed group_id ->
            b <| "Group ID " ++ group_id

        ListFeed list_id ->
            b <| "List ID " ++ list_id

        HashtagFeed tag ->
            b <| "#" ++ tag

        _ ->
            text ""


autocapitalize : String -> Attribute msg
autocapitalize =
    Html.Attributes.attribute "autocapitalize"


ariaLabel : String -> Attribute msg
ariaLabel =
    Html.Attributes.attribute "aria-label"


{-| Stolen from gab.com.

    <svg style="margin-left: 5px;"
         version="1.1"
         xmlns="http://www.w3.org/2000/svg"
         x="0px"
         y="0px"
         width="19px"
         height="19px"
         viewBox="0 0 32 32"
         xml:space="preserve"
         aria-label="Verified Account">
      <g>
        <path fill="#3E99ED"
              d="M 27.31 4.69 C 24.29 1.66 20.27 0 16 0 C 11.73 0 7.71 1.66 4.69 4.69 C 1.66 7.71 0 11.73 0 16 C 0 20.27 1.66 24.29 4.69 27.31 C 7.71 30.34 11.73 32 16 32 C 20.27 32 24.29 30.34 27.31 27.31 C 30.34 24.29 32 20.27 32 16 C 32 11.73 30.34 7.71 27.31 4.69 Z M 23.64 12.19 L 14.7 21.13 C 14.52 21.32 14.28 21.41 14.04 21.41 C 13.8 21.41 13.56 21.32 13.38 21.13 L 8.36 16.11 C 7.99 15.75 7.99 15.15 8.36 14.79 C 8.72 14.42 9.32 14.42 9.68 14.79 L 14.04 19.14 L 22.32 10.87 C 22.68 10.5 23.28 10.5 23.64 10.87 C 24.01 11.23 24.01 11.82 23.64 12.19 Z M 23.64 12.19">
        </path>
      </g>
    </svg>

-}
blueCheck : Int -> Html msg
blueCheck scalePct =
    let
        sizepx =
            (17 * scalePct // 100 |> String.fromInt) ++ "px"
    in
    svg
        (List.append
            [ Svga.width sizepx
            , Svga.height sizepx
            ]
            blueCheckAttr
        )
        blueCheckBody


blueCheckAttr : List (Svg.Attribute msg)
blueCheckAttr =
    [ Svga.style "margin-left: 5px;"
    , Svga.version "1.1"
    , Svga.x "0px"
    , Svga.y "0px"
    , Svga.viewBox "0 0 32 32"
    , Svga.xmlSpace "preserve"
    , ariaLabel "Verified Account"
    ]


blueCheckBody : List (Svg msg)
blueCheckBody =
    [ Svg.g []
        [ Svg.path
            [ Svga.fill "#3E99ED"
            , Svga.d "M 27.31 4.69 C 24.29 1.66 20.27 0 16 0 C 11.73 0 7.71 1.66 4.69 4.69 C 1.66 7.71 0 11.73 0 16 C 0 20.27 1.66 24.29 4.69 27.31 C 7.71 30.34 11.73 32 16 32 C 20.27 32 24.29 30.34 27.31 27.31 C 30.34 24.29 32 20.27 32 16 C 32 11.73 30.34 7.71 27.31 4.69 Z M 23.64 12.19 L 14.7 21.13 C 14.52 21.32 14.28 21.41 14.04 21.41 C 13.8 21.41 13.56 21.32 13.38 21.13 L 8.36 16.11 C 7.99 15.75 7.99 15.15 8.36 14.79 C 8.72 14.42 9.32 14.42 9.68 14.79 L 14.04 19.14 L 22.32 10.87 C 22.68 10.5 23.28 10.5 23.64 10.87 C 24.01 11.23 24.01 11.82 23.64 12.19 Z M 23.64 12.19"
            ]
            []
        ]
    ]


renderAccount : Int -> String -> Zone -> Account -> Html Msg -> Datetime -> Maybe Status -> Html Msg
renderAccount fontSizePct color zone account description datetime maybeStatus =
    table []
        [ tr []
            [ td []
                [ imageLink
                    { imageUrl = account.avatar
                    , linkUrl = account.url
                    , altText = ""
                    , borderColor =
                        if account.is_pro then
                            Just "gold"

                        else
                            Nothing
                    , h = "3em"
                    }
                ]
            , td [ style "color" color ]
                [ description
                , br
                , link ("@" ++ account.username) account.url
                , if account.is_verified && fontSizePct > 0 then
                    blueCheck fontSizePct

                  else
                    text ""
                , br
                , let
                    timeString =
                        formatIso8601 zone datetime
                  in
                  case maybeStatus of
                    Nothing ->
                        text timeString

                    Just status ->
                        renderStatusUrl timeString status
                ]
            ]
        ]


needsStatusExplorerButton : Status -> Bool
needsStatusExplorerButton status =
    status.in_reply_to_id /= Nothing || status.replies_count > 0


renderStatusExplorerButton : String -> Status -> Html Msg
renderStatusExplorerButton fontSize status =
    a
        [ href "#"
        , onClick <|
            (ColumnsUIMsg <| OpenThreadExplorer status)
        , title "Open Thread Explorer"
        ]
        [ Html.i
            [ style "font-size" fontSize
            , class openThreadExplorerIconClass
            ]
            []
        ]


renderStatusUrl : String -> Status -> Html Msg
renderStatusUrl timeString status =
    span []
        [ a
            [ href "#"
            , onClick <|
                (ColumnsUIMsg <| OpenThreadExplorer status)
            , title "Open Thread Explorer"
            ]
            [ text timeString ]
        , case status.url of
            Nothing ->
                text ""

            Just url ->
                span []
                    [ text " "
                    , a
                        [ href url
                        , blankTarget
                        , title "Open page on server web site."
                        ]
                        [ Html.i [ class "icon-link-ext" ] [] ]
                    ]
        , if needsStatusExplorerButton status then
            span []
                [ text " "
                , a
                    [ href "#"
                    , onClick <|
                        (ColumnsUIMsg <| OpenThreadExplorer status)
                    , title "Open Thread Explorer"
                    ]
                    [ Html.i [ class openThreadExplorerIconClass ] [] ]
                ]

          else
            text ""
        ]


openThreadExplorerIconClass : String
openThreadExplorerIconClass =
    "icon-down-open"


feedLoadingEmojiSpan : Bool -> Bool -> Html msg
feedLoadingEmojiSpan addSpace reduceSize =
    span
        (if reduceSize then
            [ style "font-size" "70%" ]

         else
            []
        )
        [ text feedLoadingEmoji
        , if addSpace then
            text " "

          else
            text ""
        ]


renderFeedLoadingEmojiFooter : RenderEnv -> Html Msg
renderFeedLoadingEmojiFooter renderEnv =
    let
        { color } =
            getStyle renderEnv.style
    in
    div
        [ style "border" <| "1px solid " ++ color ]
        [ div []
            [ div
                [ class "content"
                , style "color" color
                , style "text-align" "center"
                ]
                [ feedLoadingEmojiSpan False False ]
            ]
        ]


renderNewMarker : FeedType -> RenderEnv -> Html Msg
renderNewMarker feedType renderEnv =
    let
        { color } =
            getStyle renderEnv.style

        border =
            "1px solid " ++ color
    in
    div
        [ style "border-right" border
        , style "border-left" border
        , style "border-bottom" border
        ]
        [ Html.hr
            [ style "color" "red"
            , style "background-color" "red"
            , style "height" "10px"
            , style "margin" "0"
            , style "cursor" "pointer"
            , title "Click to mark read."
            , onClick (ColumnsUIMsg <| MarkFeedRead feedType)
            ]
            []
        ]


renderStatusWithNewMarker : FeedType -> RenderEnv -> FeedEnv -> Int -> Int -> Status -> Html Msg
renderStatusWithNewMarker feedType renderEnv feedEnv newElements index status =
    if newElements > 0 && newElements == index then
        div []
            [ renderNewMarker feedType renderEnv
            , renderStatus renderEnv feedEnv status
            ]

    else
        renderStatus renderEnv feedEnv status


renderStatus : RenderEnv -> FeedEnv -> Status -> Html Msg
renderStatus =
    renderStatusWithId Nothing


renderStatusWithId : Maybe String -> RenderEnv -> FeedEnv -> Status -> Html Msg
renderStatusWithId maybeNodeid renderEnv feedEnv statusIn =
    let
        ( status, account, reblogAccount ) =
            case statusIn.reblog of
                Nothing ->
                    ( statusIn, statusIn.account, Nothing )

                Just (WrappedStatus reblog) ->
                    ( reblog, reblog.account, Just statusIn.account )

        replyToInfo =
            case reblogAccount of
                Just _ ->
                    Nothing

                Nothing ->
                    case status.in_reply_to_account_id of
                        Nothing ->
                            Nothing

                        Just account_id ->
                            case Dict.get account_id feedEnv.references of
                                Nothing ->
                                    Just ( account_id, "" )

                                Just reference ->
                                    case reference of
                                        ReferencedAccount acct ->
                                            Just ( acct.acct, acct.url )

                                        ReferencedMention mention ->
                                            Just ( mention.acct, mention.url )

        { color } =
            getStyle renderEnv.style

        displayNameHtml =
            renderDisplayName account.display_name renderEnv

        body =
            statusBody renderEnv status
    in
    div
        (List.append
            [ style "border" <| "1px solid " ++ color ]
            (case maybeNodeid of
                Nothing ->
                    []

                Just nodeid ->
                    [ id nodeid ]
            )
        )
        [ div []
            [ div
                [ class "content"
                , style "color" color
                , headerFontSizeStyle
                ]
                [ case reblogAccount of
                    Nothing ->
                        text ""

                    Just acct ->
                        span []
                            [ a [ href acct.url ]
                                [ renderDisplayName acct.display_name
                                    renderEnv
                                ]
                            , text " reblogged:"
                            ]
                , case replyToInfo of
                    Nothing ->
                        text ""

                    Just ( acct, url ) ->
                        div
                            [ class "status-el media-body"
                            , class "reply-to-and-account-name"
                            , style "margin-top" "0.2em"
                            ]
                            [ Html.i [ class "icon-reply" ]
                                []
                            , text " Reply to "
                            , if url == "" then
                                text acct

                              else
                                a
                                    [ href url
                                    , style "margin-left" "0.4em"
                                    ]
                                    [ text <| "@" ++ acct ]
                            ]
                , renderAccount renderEnv.fontSizePct
                    color
                    renderEnv.here
                    account
                    displayNameHtml
                    status.created_at
                    (Just status)
                ]
            , case status.group of
                Nothing ->
                    text ""

                Just group ->
                    case feedEnv.group of
                        Just _ ->
                            text ""

                        _ ->
                            div
                                [ class "content"
                                , style "color" color
                                ]
                                [ case groupUrl renderEnv group of
                                    Nothing ->
                                        text group.title

                                    Just url ->
                                        a [ href url ]
                                            [ text group.title ]
                                , text <| " (" ++ String.fromInt group.member_count ++ " members)"
                                ]
            , hr
            , div
                [ class "content"
                , style "color" color
                ]
                body
            , renderMediaAttachments renderEnv status
            , renderStatusQuote renderEnv feedEnv status
            , renderStatusCard renderEnv status
            , renderStatusActions renderEnv status
            ]
        ]


renderStatusQuote : RenderEnv -> FeedEnv -> Status -> Html Msg
renderStatusQuote renderEnv feedEnv status =
    case status.quote of
        Nothing ->
            text ""

        Just quote ->
            case quote of
                WrappedStatus wrappedStatus ->
                    div [ style "margin" "4px" ]
                        [ div
                            [ style "padding" "4px"
                            ]
                            [ span [ style "font-size" smallTextFontSize ]
                                [ text "[quote]" ]
                            , br
                            , renderStatus
                                { renderEnv
                                    | columnWidth =
                                        renderEnv.columnWidth - 16
                                }
                                feedEnv
                                wrappedStatus
                            ]
                        ]


renderStatusCard : RenderEnv -> Status -> Html Msg
renderStatusCard renderEnv status =
    case status.card of
        Nothing ->
            text ""

        Just card ->
            div
                [ style "margin" "4px" ]
                [ div
                    [ style "border" "1px solid"
                    , style "padding" "4px"
                    ]
                    [ case card.image of
                        Nothing ->
                            text ""

                        Just image ->
                            p
                                [ style "width" "100%"
                                , style "text-align" "center"
                                ]
                                [ a [ href card.url ]
                                    [ img
                                        [ src image
                                        , style "max-height" "4em"
                                        ]
                                        []
                                    ]
                                ]
                    , p []
                        [ a [ href card.url ]
                            [ b card.title ]
                        , case nothingIfMaybeBlank card.author_name of
                            Nothing ->
                                text ""

                            Just author_name ->
                                span [ style "font-size" smallTextFontSize ]
                                    [ br
                                    , text "by "
                                    , maybeLink author_name card.author_url
                                    , case nothingIfMaybeBlank card.provider_name of
                                        Nothing ->
                                            text ""

                                        Just provider_name ->
                                            span []
                                                [ text " "
                                                , maybeLink provider_name
                                                    card.provider_url
                                                ]
                                    ]
                        ]
                    , p []
                        [ text card.description ]
                    ]
                ]


nothingIfMaybeBlank : Maybe String -> Maybe String
nothingIfMaybeBlank maybeString =
    case maybeString of
        Nothing ->
            Nothing

        Just string ->
            if string == "" then
                Nothing

            else
                Just string


maybeLink : String -> Maybe String -> Html msg
maybeLink string maybeUrl =
    case nothingIfMaybeBlank maybeUrl of
        Nothing ->
            text string

        Just url ->
            a [ href url ] [ text string ]


groupUrl : RenderEnv -> Group -> Maybe String
groupUrl renderEnv group =
    case renderEnv.loginServer of
        Nothing ->
            Nothing

        Just loginServer ->
            Just <| "https://" ++ loginServer ++ "/groups/" ++ group.id


renderMediaAttachments : RenderEnv -> Status -> Html Msg
renderMediaAttachments renderEnv status =
    if status.sensitive && status.media_attachments /= [] then
        p []
            [ button (ColumnsUIMsg <| ShowStatusImages status.id)
                "Show sensitive images"
            ]

    else
        p [] <|
            List.map (renderAttachment renderEnv)
                status.media_attachments


statusButton : List (Attribute Msg) -> String -> Msg -> Html Msg
statusButton attributes theText msg =
    div [ onClick msg ]
        [ Html.i attributes []
        , text theText
        ]


renderStatusActions : RenderEnv -> Status -> Html Msg
renderStatusActions renderEnv status =
    let
        { color } =
            getStyle renderEnv.style

        id =
            status.id

        replies_count =
            status.replies_count

        reblogged =
            status.reblogged

        reblogs_count =
            status.reblogs_count

        favourites_count =
            status.favourites_count

        favourited =
            status.favourited

        repliesString =
            if replies_count > 0 then
                String.fromInt replies_count

            else
                ""

        rebloggedTitle =
            if reblogged then
                "unRepeat"

            else
                "Repeat"

        retweetedClass =
            if reblogged then
                " retweeted"

            else
                " retweeted-empty"

        reblogsString =
            if reblogs_count > 0 then
                String.fromInt reblogs_count

            else
                ""

        favoriteTitle =
            if favourited then
                "unFavorite"

            else
                "Favorite"

        favoriteClass =
            if favourited then
                " icon-star"

            else
                " icon-star-empty"

        favoritesString =
            if favourites_count > 0 then
                String.fromInt favourites_count

            else
                ""
    in
    div
        [ class "status-el status-actions media-body"
        , style "color" color
        ]
        [ statusButton
            [ title "Reply"

            -- It would be lovely to add button-icon-active
            -- here, if YOU have replied, but the status
            -- doesn't tell us that. Pleroma adds it
            -- while you're in the process of typing a reply.
            -- Mammudeck will do that with a pop-up, so it won't apply.
            , class "button-icon icon-reply"
            ]
            repliesString
            (ColumnsUIMsg <| ShowPostDialog (Just status))
        , statusButton
            [ title rebloggedTitle
            , class <|
                "button-icon retweet-button icon-retweet rt-active"
                    ++ retweetedClass
            ]
            reblogsString
            (ColumnsUIMsg <| ToggleStatusRepeat status)
        , statusButton
            [ title favoriteTitle
            , class <|
                "button-icon favorite-button fav-active"
                    ++ favoriteClass
            ]
            favoritesString
            (ColumnsUIMsg <| ToggleStatusFavorite status)
        , if needsStatusExplorerButton status then
            statusButton
                [ title "Open Thread Explorer"
                , class openThreadExplorerIconClass
                ]
                ""
                (ColumnsUIMsg <| OpenThreadExplorer status)

          else
            text ""
        , statusButton
            [ class "icon-ellipsis" ]
            ""
            (ColumnsUIMsg <| StatusEllipsisDialog status)
        ]


hrpct : Int -> Html msg
hrpct pct =
    Html.hr
        [ style "width" <| String.fromInt pct ++ "%"
        , style "margin" "auto"
        ]
        []


hr : Html msg
hr =
    hrpct 95


iso8601ToMonthYear : Zone -> String -> String
iso8601ToMonthYear zone iso8601 =
    case Iso8601.toTime iso8601 of
        Err _ ->
            iso8601

        Ok posix ->
            Format.format (Configs.getConfig "en_us")
                "%b %Y"
                zone
                posix


formatIso8601 : Zone -> String -> String
formatIso8601 zone iso8601 =
    case Iso8601.toTime iso8601 of
        Err _ ->
            iso8601

        Ok posix ->
            Format.format (Configs.getConfig "en_us")
                "%y%m%d %-H:%M:%S"
                zone
                posix


renderAttachment : RenderEnv -> Attachment -> Html Msg
renderAttachment renderEnv attachment =
    case attachment.type_ of
        ImageAttachment ->
            let
                preview_url =
                    case attachment.preview_url of
                        Just url ->
                            url

                        Nothing ->
                            attachment.url
            in
            a [ href attachment.url ]
                [ img
                    [ src preview_url
                    , alt "image"
                    , style "width" "100%"
                    ]
                    []
                ]

        VideoAttachment ->
            div []
                [ span [ style "font-size" smallTextFontSize ]
                    [ text "["
                    , a [ href attachment.url ]
                        [ text "video" ]
                    , text "]"
                    ]
                , case attachment.preview_url of
                    Nothing ->
                        text ""

                    Just preview_url ->
                        span []
                            [ br
                            , a [ href attachment.url ]
                                [ img
                                    [ src preview_url
                                    , alt "image"
                                    , style "width" "100%"
                                    ]
                                    []
                                ]
                            ]
                ]

        _ ->
            text ""


px : Int -> String
px int =
    String.fromInt int ++ "px"


pxBang : Int -> String
pxBang int =
    px int ++ " !important"


columnsBorderSpacing : Int
columnsBorderSpacing =
    2


triangleHeight : Float -> Float
triangleHeight width =
    width * sqrt 3 * 0.5


scrollPillBackground : String
scrollPillBackground =
    "lightBlue"


scrollPillSelectedBackground : String
scrollPillSelectedBackground =
    "gray"


scrollPillColors : Button.Colors
scrollPillColors =
    let
        colors =
            Button.defaultColors
    in
    { colors
        | background = scrollPillBackground
        , text = "black"
    }


renderScrollPill : Model -> Html Msg
renderScrollPill model =
    let
        ( _, sh ) =
            model.renderEnv.windowSize

        tw =
            toFloat sh / 7

        -- w + w*sqrt(3) = tw
        w =
            tw / (1 + sqrt 3)

        l =
            w * sqrt 3 / 2

        squareButton location iClass cuiMsg title =
            let
                content =
                    if iClass == "" then
                        Button.NoContent

                    else
                        let
                            fontSize =
                                0.6 * w

                            xDelta =
                                fontSize / 4 * 0.8

                            yDelta =
                                fontSize / 8

                            nominal =
                                (w - fontSize) / 2

                            x =
                                nominal - xDelta

                            y =
                                nominal - yDelta

                            fontSizeS =
                                String.fromFloat fontSize

                            xS =
                                String.fromFloat x

                            yS =
                                String.fromFloat y

                            wid =
                                fontSize + 2 * xDelta

                            widS =
                                String.fromFloat wid

                            hei =
                                fontSize + 2 * yDelta

                            heiS =
                                String.fromFloat hei

                            -- This stops icon-spin3 from getting cut off
                            paddingS =
                                String.fromFloat yDelta ++ "px"
                        in
                        Button.SvgContent <|
                            Svg.foreignObject
                                [ Svga.x xS
                                , Svga.y yS
                                , Svga.width widS
                                , Svga.height heiS
                                ]
                                [ div
                                    [ style "color" "black"
                                    , style "padding-top" paddingS
                                    ]
                                    [ Html.i
                                        [ class <| iClass
                                        , style "font-size" <| fontSizeS ++ "px"
                                        ]
                                        []
                                    ]
                                ]
            in
            Button.render
                location
                content
                (\m -> ColumnsUIMsg (SimpleButtonMsg m cuiMsg))
                (Button.simpleButton ( w, w ) ()
                    |> Button.setTouchAware model.isTouchAware
                    |> Button.setColors scrollPillColors
                    |> addButtonTitle title
                )

        triangleButton direction location cuiMsg title =
            Button.render
                location
                Button.NoContent
                (\m -> ColumnsUIMsg (SimpleButtonMsg m cuiMsg))
                (Button.simpleButton ( l, w ) ()
                    |> Button.setTouchAware model.isTouchAware
                    |> Button.setColors scrollPillColors
                    |> Button.setTriangularButtonRenderers direction
                    --rectangular hit region
                    |> Button.setRenderOverlay Button.renderOverlay
                    |> addButtonTitle title
                )

        th =
            if model.showFullScrollPill then
                3 * w - 2

            else
                w
    in
    svg
        [ Svga.width <| String.fromFloat tw
        , Svga.height <| String.fromFloat th
        ]
    <|
        if model.showFullScrollPill then
            [ squareButton ( l, 0 )
                "icon-cog"
                ShowSettingsDialog
                "Show Settings Dialog [,]"
            , squareButton ( l, w - 1 )
                "icon-spin3"
                ReloadAllColumns
                "Reload All Columns [r]"
            , squareButton ( l, 2 * w - 2 )
                "icon-pencil"
                (ShowPostDialog Nothing)
                "Show Post Dialog [p]"
            , triangleButton LeftButton
                ( 1, 2 * w - 2 )
                (ScrollPage ScrollLeft)
                "Scroll One Page Left"
            , triangleButton RightButton
                ( l + w - 2, 2 * w - 2 )
                (ScrollPage ScrollRight)
                "Scroll One Page Right"
            ]

        else
            [ squareButton ( l, 0 )
                ""
                ShowFullScrollPill
                "Expand Buttons, Click or Escape to Contract"
            , triangleButton LeftButton
                ( 1, 0 )
                (ScrollPage ScrollLeft)
                "Scroll One Page Left"
            , triangleButton RightButton
                ( l + w - 2, 0 )
                (ScrollPage ScrollRight)
                "Scroll One Page Right"
            ]


type ScrollDirection
    = ScrollRight
    | ScrollLeft


addButtonTitle : String -> Button state msg -> Button state msg
addButtonTitle title svgButton =
    let
        renderer =
            Button.getRenderOverlay svgButton

        newRenderer wrapper but =
            Svg.g []
                [ Svg.title [] [ Svg.text title ]
                , renderer wrapper but
                ]
    in
    Button.setRenderOverlay newRenderer svgButton


{-| Never rendered. Just used for updating.
-}
simpleButton : Button () ColumnsUIMsg
simpleButton =
    Button.simpleButton ( 10, 10 ) ()


renderColumns : Model -> Html Msg
renderColumns model =
    let
        renderEnv =
            model.renderEnv

        { feeds } =
            model.feedSet

        ( _, h ) =
            renderEnv.windowSize

        ( windowWidth, windowHeight ) =
            renderEnv.windowSize
    in
    renderCenteredScreen model
        ""
        [ case model.msg of
            Nothing ->
                text ""

            Just msg ->
                p [ style "color" "red" ]
                    [ text msg ]
        , if model.dialog /= NoDialog || not model.scrollPillState.showScrollPill then
            text ""

          else
            div
                [ style "position" "fixed"
                , style "bottom" <| (10 |> String.fromInt) ++ "px"
                , style "right" <| (10 |> String.fromInt) ++ "px"
                , PopupPicker.zIndex zIndices.scrollPill
                ]
                [ renderScrollPill model
                , if not model.scrollPillState.showServer then
                    text ""

                  else
                    case renderEnv.loginServer of
                        Nothing ->
                            text ""

                        Just server ->
                            div
                                [ style "text-align" "center"
                                , style "padding" "5px"
                                , style "border" "2px solid"
                                , style "border-radius" "25px"
                                , style "background" scrollPillBackground
                                , style "margin-top" "-1px"
                                ]
                                [ a
                                    [ href <| "https://" ++ server
                                    , class "scroll-pill-server"
                                    ]
                                    [ text server ]
                                ]

                --, br
                --, text (model.bodyScroll.scrollLeft |> String.fromFloat)
                ]
        , table
            [ style "border-spacing" <| String.fromInt columnsBorderSpacing
            , fsStyle renderEnv
            ]
            [ tr [] <|
                List.concat
                    [ if model.showLeftColumn then
                        [ td
                            [ style "vertical-align" "top"
                            , style "padding" "0"
                            ]
                            [ Lazy.lazy renderLeftColumn renderEnv ]
                        ]

                      else
                        []
                    , List.map
                        (\feed ->
                            let
                                ignore =
                                    feedDescription feed

                                id =
                                    Types.feedID feed.feedType

                                isFeedLoading =
                                    Set.member id model.loadingFeeds

                                newCount =
                                    case Dict.get id model.newElements of
                                        Nothing ->
                                            0

                                        Just elements ->
                                            case elements of
                                                StatusElements statuses ->
                                                    List.length statuses

                                                NotificationElements notifications ->
                                                    List.length notifications

                                                _ ->
                                                    0

                                feedEnv =
                                    getFeedEnv feed.feedType model
                            in
                            td
                                [ style "vertical-align" "top"
                                , style "padding" "0"
                                ]
                                [ Lazy.lazy5 renderFeed
                                    isFeedLoading
                                    newCount
                                    renderEnv
                                    feedEnv
                                    feed
                                ]
                        )
                        feeds
                    ]
            ]
        ]


feedDescription : Feed -> ( String, Int )
feedDescription feed =
    case feed.elements of
        StatusElements list ->
            ( "Status", List.length list )

        NotificationElements list ->
            ( "Notification", List.length list )

        AccountElements list ->
            ( "Account", List.length list )

        ConversationsElements list ->
            ( "Conversations", List.length list )

        ResultsElements list ->
            ( "Results", List.length list )


primaryServerLine : Model -> Html Msg
primaryServerLine model =
    case model.renderEnv.loginServer of
        Nothing ->
            text ""

        Just server ->
            p []
                [ case model.account of
                    Nothing ->
                        b "Using server: "

                    Just account ->
                        span []
                            [ b "Logged in as: "
                            , link ("@" ++ account.username) account.url
                            , text "@"
                            ]
                , link server <| "https://" ++ server
                , text " "
                , button (GlobalMsg Logout) "Logout"
                ]


renderExplorer : Model -> Html Msg
renderExplorer model =
    let
        { backgroundColor, color } =
            getStyle model.renderEnv.style
    in
    renderCenteredScreen model
        "40em"
        [ div []
            [ h2 [] [ text "Mastodon API Explorer" ]
            , pageSelector True (model.renderEnv.loginServer /= Nothing) model.page
            , primaryServerLine model
            , p []
                [ selectedRequestHtml LoginSelected
                    "https://docs.joinmastodon.org/methods/apps/oauth/"
                    model
                    (loginSelectedUI True)
                , selectedRequestHtml InstanceSelected
                    "https://docs.joinmastodon.org/methods/instance/"
                    model
                    instanceSelectedUI
                , selectedRequestHtml AccountsSelected
                    "https://docs.joinmastodon.org/methods/accounts/"
                    model
                    accountsSelectedUI
                , selectedRequestHtml BlocksSelected
                    "https://docs.joinmastodon.org/methods/accounts/blocks/"
                    model
                    blocksSelectedUI
                , selectedRequestHtml CustomEmojisSelected
                    "https://docs.joinmastodon.org/methods/instance/custom_emojis/"
                    model
                    customEmojisSelectedUI
                , selectedRequestHtml EndorsementsSelected
                    "https://docs.joinmastodon.org/methods/accounts/endorsements/"
                    model
                    endorsementsSelectedUI
                , selectedRequestHtml FavouritesSelected
                    "https://docs.joinmastodon.org/methods/accounts/favourites/"
                    model
                    favouritesSelectedUI
                , selectedRequestHtml FiltersSelected
                    "https://docs.joinmastodon.org/methods/accounts/filters/"
                    model
                    filtersSelectedUI
                , selectedRequestHtml FollowRequestsSelected
                    "https://docs.joinmastodon.org/methods/accounts/follow_requests/"
                    model
                    followRequestsSelectedUI
                , selectedRequestHtml FollowSuggestionsSelected
                    "https://docs.joinmastodon.org/methods/accounts/suggestions/"
                    model
                    followSuggestionsSelectedUI
                , selectedRequestHtml GroupsSelected
                    ""
                    model
                    groupsSelectedUI
                , selectedRequestHtml ListsSelected
                    "https://docs.joinmastodon.org/methods/timelines/lists/"
                    model
                    listsSelectedUI
                , selectedRequestHtml MutesSelected
                    "https://docs.joinmastodon.org/methods/accounts/mutes/"
                    model
                    mutesSelectedUI
                , selectedRequestHtml NotificationsSelected
                    "https://docs.joinmastodon.org/methods/notifications/"
                    model
                    notificationsSelectedUI
                , selectedRequestHtml ReportsSelected
                    "https://docs.joinmastodon.org/methods/accounts/reports/"
                    model
                    reportsSelectedUI
                , selectedRequestHtml ScheduledStatusesSelected
                    "https://docs.joinmastodon.org/methods/statuses/scheduled_statuses/"
                    model
                    scheduledStatusesSelectedUI
                , selectedRequestHtml SearchSelected
                    "https://docs.joinmastodon.org/methods/search/"
                    model
                    searchSelectedUI
                , selectedRequestHtml StatusesSelected
                    "https://docs.joinmastodon.org/methods/statuses/"
                    model
                    statusesSelectedUI
                , selectedRequestHtml TimelinesSelected
                    "https://docs.joinmastodon.org/methods/timelines/"
                    model
                    timelinesSelectedUI
                , selectedRequestHtml TrendsSelected
                    "https://docs.joinmastodon.org/methods/instance/trends/"
                    model
                    trendsSelectedUI
                ]
            , p [ style "color" "red" ]
                [ Maybe.withDefault "" model.msg |> text ]
            , p []
                [ span [ hidden <| model.selectedKeyValue == "" ]
                    [ b "selected path: "
                    , text model.selectedKeyPath
                    , case Url.fromString model.selectedKeyValue of
                        Nothing ->
                            text ""

                        Just _ ->
                            span []
                                [ br
                                , a
                                    [ href model.selectedKeyValue
                                    , blankTarget
                                    ]
                                    [ text "open URL in new tab" ]
                                ]
                    , br
                    , textarea
                        [ id "selectedKeyValue"
                        , rows 4
                        , cols 80
                        , readonly True
                        , value model.selectedKeyValue
                        ]
                        []
                    , br
                    ]
                , WriteClipboard.writeClipboard
                    [ WriteClipboard.write
                        { id =
                            if isKeyDown keyboard.alt model then
                                ""

                            else
                                "selectedKeyValue"
                        , text = model.clipboardValue
                        , count = model.clipboardCount
                        }
                    ]
                    []
                ]
            , checkBox (ExplorerUIMsg ToggleShowJsonTree)
                model.showJsonTree
                "show tree"
            , if model.showJsonTree then
                text ""

              else
                span []
                    [ text " "
                    , checkBox (ExplorerUIMsg TogglePrettify)
                        model.prettify
                        "prettify"
                    ]
            , text " "
            , checkBox (ExplorerUIMsg ToggleUseElmButtonNames)
                model.useElmButtonNames
                "elm button names"
            , text " "
            , button (ExplorerUIMsg ClearSentReceived) "Clear"
            , p [] [ b "Sent:" ]
            , pre []
                [ case model.request of
                    Nothing ->
                        text ""

                    Just request ->
                        span []
                            [ text request.method
                            , text " "
                            , text request.url
                            , case request.jsonBody of
                                Nothing ->
                                    text ""

                                Just value ->
                                    pre []
                                        [ text <|
                                            encodeWrap model.prettify value
                                        ]
                            ]
                ]
            , p [] <|
                if model.showMetadata then
                    [ b "Headers: "
                    , button (ExplorerUIMsg ToggleShowMetadata) "Hide"
                    ]

                else
                    [ b "Headers "
                    , button (ExplorerUIMsg ToggleShowMetadata) "Show"
                    ]
            , if not model.showMetadata then
                text ""

              else
                case model.metadata of
                    Nothing ->
                        text ""

                    Just metadata ->
                        p []
                            [ renderHeaders model.prettify color metadata ]
            , p [] <|
                if model.showReceived then
                    [ b "Received:"
                    , button (ExplorerUIMsg ToggleShowReceived) "Hide"
                    ]

                else
                    [ b "Received "
                    , button (ExplorerUIMsg ToggleShowReceived) "Show"
                    ]
            , renderJson ResponseJson
                model
                model.showReceived
                model.response
                Nothing
            , p [] <|
                if model.showEntity then
                    [ b "Decoded:"
                    , button (ExplorerUIMsg ToggleShowEntity) "Hide"
                    ]

                else
                    [ b "Decoded "
                    , button (ExplorerUIMsg ToggleShowEntity) "Show"
                    ]
            , renderJson DecodedJson
                model
                model.showEntity
                Nothing
                model.entity
            , div []
                [ explorerHelp model ]
            , br
            , p []
                [ button (GlobalMsg ClearAllDialog)
                    "Clear All Persistent State"
                ]
            , br
            , p
                [ onClick (ColumnsUIMsg ToggleStyle)
                , style "cursor" "default"
                ]
                [ input
                    [ type_ "checkbox"
                    , checked <| model.renderEnv.style == DarkStyle
                    ]
                    []
                , b "Dark Mode"
                ]
            , p []
                [ text <| "Copyright " ++ special.copyright ++ " 2019-2020, Bill St. Clair"
                , br
                , link "@billstclair@impeccable.social"
                    "https://impeccable.social/billstclair"
                , br
                , link "@billstclair@gab.com"
                    "https://gab.com/billstclair"
                , br
                , text "API Docs: "
                , link "docs.joinmastodon.org"
                    "https://docs.joinmastodon.org/client/guidelines"
                , br
                , text "Source code: "
                , link "GitHub"
                    "https://github.com/billstclair/mammudeck"
                ]
            ]
        ]


renderJson : WhichJson -> Model -> Bool -> Maybe Value -> Maybe Entity -> Html Msg
renderJson whichJson model enabled value entity =
    if not enabled then
        text ""

    else
        let
            mv =
                case value of
                    Just val ->
                        Just val

                    Nothing ->
                        case entity of
                            Nothing ->
                                Nothing

                            Just e ->
                                Just <| ED.encodeEntity e
        in
        case mv of
            Nothing ->
                text ""

            Just v ->
                if model.showJsonTree then
                    renderJsonTree whichJson model v

                else
                    pre []
                        [ text <| encodeWrap model.prettify v ]


type WhichJson
    = ResponseJson
    | DecodedJson


jsonTreeColors =
    let
        default =
            JsonTree.defaultColors
    in
    { light = default
    , dark = { default | number = "turquoise" }
    }


renderJsonTree : WhichJson -> Model -> Value -> Html Msg
renderJsonTree whichJson model value =
    let
        ( setter, treeResponse, state ) =
            case whichJson of
                ResponseJson ->
                    ( GlobalMsg << SetResponseState, model.responseTree, model.responseState )

                DecodedJson ->
                    ( GlobalMsg << SetEntityState, model.entityTree, model.entityState )

        config =
            { colors =
                if model.renderEnv.style == DarkStyle then
                    jsonTreeColors.dark

                else
                    jsonTreeColors.light

            -- Should copy the text to a <TextArea> on select.
            , onSelect = Just <| (GlobalMsg << SelectTreeNode whichJson)
            , toMsg = setter
            }
    in
    case treeResponse of
        Err _ ->
            text ""

        Ok root ->
            span []
                [ button ((GlobalMsg << ExpandAll) whichJson) "Expand All"
                , text " "
                , button ((GlobalMsg << CollapseAll) whichJson) "Collapse All"
                , case root.value of
                    TList nodes ->
                        span []
                            [ br
                            , b "length: "
                            , text (String.fromInt <| List.length nodes)
                            ]

                    _ ->
                        text ""
                , br
                , JsonTree.view root config state
                ]


blocksSelectedUI : Model -> Html Msg
blocksSelectedUI model =
    p []
        [ pspace
        , textInput "limit: " 10 (ExplorerUIMsg << SetLimit) model.pagingInput.limit
        , br
        , sendButton SendGetBlocks model
        , br
        , text "-- writes below here --"
        , br
        , textInput "account id: " 25 (ExplorerUIMsg << SetAccountId) model.accountId
        , br
        , sendButton SendPostBlock model
        , text " "
        , sendButton SendPostUnblock model
        ]


maybeLabeledTextInput : Maybe String -> Int -> (String -> Msg) -> String -> Html Msg
maybeLabeledTextInput label inputSize wrapper string =
    let
        inputBox =
            input
                [ size inputSize
                , onInput wrapper
                , value string
                ]
                []
    in
    case label of
        Nothing ->
            inputBox

        Just lab ->
            span []
                [ b lab
                , inputBox
                ]


textInput : String -> Int -> (String -> Msg) -> String -> Html Msg
textInput label =
    maybeLabeledTextInput (Just label)


unlabeledTextInput : Int -> (String -> Msg) -> String -> Html Msg
unlabeledTextInput =
    maybeLabeledTextInput Nothing


excludedNotificationTypeCheckbox : String -> NotificationType -> Model -> Html Msg
excludedNotificationTypeCheckbox label notificationType model =
    checkBox ((ExplorerUIMsg << ToggleExcludedNotificationType) notificationType)
        (List.member notificationType model.excludedNotificationTypes)
        label


titledCheckBox : String -> Msg -> Bool -> String -> Html Msg
titledCheckBox theTitle msg isChecked label =
    span
        [ onClick msg
        , style "cursor" "default"
        , title theTitle
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            ]
            []
        , b label
        ]


checkBox : Msg -> Bool -> String -> Html Msg
checkBox =
    titledCheckBox ""


customEmojisSelectedUI : Model -> Html Msg
customEmojisSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetCustomEmojis model
        ]


endorsementsSelectedUI : Model -> Html Msg
endorsementsSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetEndorsements model
        , br
        , text "-- writes below here --"
        , br
        , textInput "account id: "
            25
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , br
        , sendButton SendPostPinAccount model
        , sendButton SendPostUnpinAccount model
        ]


favouritesSelectedUI : Model -> Html Msg
favouritesSelectedUI model =
    p []
        [ pspace
        , textInput "limit: " 10 (ExplorerUIMsg << SetLimit) model.pagingInput.limit
        , text " "
        , sendButton SendGetFavourites model
        , br
        , text "-- writes below here --"
        , br
        , textInput "status id: " 25 (ExplorerUIMsg << SetStatusId) model.statusId
        , br
        , sendButton SendPostFavourite model
        , text " "
        , sendButton SendPostUnfavourite model
        ]


filtersSelectedUI : Model -> Html Msg
filtersSelectedUI model =
    let
        { phrase, context, irreversible, whole_word, expires_in } =
            model.filterInput

        canPost =
            isFilterInputValid model.filterInput
    in
    p []
        [ pspace
        , sendButton SendGetFilters model
        , br
        , textInput "filter id: " 25 (ExplorerUIMsg << SetFilterId) model.filterId
        , text " "
        , sendButton SendGetFilter model
        , br
        , text "-- writes below here --"
        , br
        , textInput "phrase: " 50 (ExplorerUIMsg << SetFilterPhrase) phrase
        , br
        , b "context (check at least one): "
        , filterContextCheckBoxes context
        , br
        , checkBox (ExplorerUIMsg ToggleFilterIrreversible) irreversible "irreversible"
        , text " "
        , checkBox (ExplorerUIMsg ToggleFilterWholeWord) whole_word "whole word"
        , br
        , textInput "expires in (seconds): " 10 (ExplorerUIMsg << SetFilterExpiresIn) expires_in
        , br
        , enabledSendButton canPost SendPostFilter model
        , br
        , textInput "filter id: " 25 (ExplorerUIMsg << SetFilterId) model.filterId
        , text " "
        , enabledSendButton canPost SendPutFilter model
        , text " "
        , enabledSendButton (String.trim model.filterId /= "")
            SendDeleteFilter
            model
        ]


filterContextCheckBoxes : List FilterContext -> Html Msg
filterContextCheckBoxes context =
    span [] <|
        List.map
            (\c ->
                span []
                    [ filterContextCheckbox c context
                    , text " "
                    ]
            )
            [ HomeContext, NotificationsContext, PublicContext, ThreadContext ]


filterContextCheckbox : FilterContext -> List FilterContext -> Html Msg
filterContextCheckbox context contexts =
    checkBox ((ExplorerUIMsg << ToggleFilterInputContext) context)
        (List.member context contexts)
        (ED.filterContextToString context |> String.toLower)


followRequestsSelectedUI : Model -> Html Msg
followRequestsSelectedUI model =
    p []
        [ pspace
        , textInput "limit: " 10 (ExplorerUIMsg << SetLimit) model.pagingInput.limit
        , text " "
        , sendButton SendGetFollowRequests model
        , br
        , text "-- writes below here --"
        , br
        , textInput "account id: " 25 (ExplorerUIMsg << SetAccountId) model.accountId
        , br
        , sendButton SendPostAuthorizeFollow model
        , text " "
        , sendButton SendPostRejectFollow model
        ]


followSuggestionsSelectedUI : Model -> Html Msg
followSuggestionsSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetFollowSuggestions model
        , br
        , textInput "account id: "
            25
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , text " "
        , sendButton SendDeleteFollowSuggestions model
        ]


groupsSelectedUI : Model -> Html Msg
groupsSelectedUI model =
    p []
        [ pspace
        , whichGroupsSelect model
        , text " "
        , sendButton SendGetGroups model
        , br
        , textInput "group id: "
            20
            (ExplorerUIMsg << SetGroupId)
            model.groupId
        , br
        , sendButton SendGetGroup model
        , text " "
        , sendButton SendGetGroupAccounts model
        , br
        , sendButton SendGetGroupRemovedAccounts model
        , br
        , textInput "group ids (a,b,...): "
            50
            (ExplorerUIMsg << SetGroupIds)
            model.groupIds
        , br
        , sendButton SendGetGroupRelationships model
        , br
        , text "-- writes below here --"
        , br
        , textInput "group id: "
            20
            (ExplorerUIMsg << SetGroupId)
            model.groupId
        , br
        , sendButton SendPostGroupJoin model
        , text " "
        , sendButton SendDeleteGroupJoin model
        , br
        , textInput "acccount id: "
            25
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , br
        , sendButton SendPostGroupRemovedAccounts model
        , text " "
        , sendButton SendDeleteGroupRemovedAccounts model
        , br
        , sendButton SendPatchGroupAddAdministrator model
        , br
        , textInput "status id: "
            25
            (ExplorerUIMsg << SetStatusId)
            model.statusId
        , text " "
        , sendButton SendDeleteGroupStatus model
        , br
        , textInput "title: "
            60
            (ExplorerUIMsg << SetGroupTitle)
            model.groupTitle
        , br
        , b "description:"
        , br
        , textarea
            [ onInput (ExplorerUIMsg << SetGroupDescription)
            , value model.groupDescription
            , rows 4
            , cols 80
            ]
            []
        , br
        , renderChooseFile "cover image (19x7): "
            model.groupCoverImage
            (ExplorerUIMsg << GetGroupCoverImage)
        , br
        , sendButton SendPostGroup model
        , br
        , textInput "group id: "
            20
            (ExplorerUIMsg << SetGroupId)
            model.groupId
        , text " "
        , sendButton SendPutGroup model
        ]


listsSelectedUI : Model -> Html Msg
listsSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetLists model
        , br
        , textInput "list id: "
            25
            (ExplorerUIMsg << SetListId)
            model.listId
        , text " "
        , sendButton SendGetList model
        , text " "
        , sendButton SendGetListAccounts model
        , br
        , textInput "account id: "
            25
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , text " "
        , sendButton SendGetAccountLists model
        , br
        , text "-- writes below here --"
        , br
        , textInput "title: "
            40
            (ExplorerUIMsg << SetListTitle)
            model.listTitle
        , text " "
        , sendButton SendPostList model
        , br
        , textInput "list id: "
            25
            (ExplorerUIMsg << SetListId)
            model.listId
        , text " "
        , sendButton SendPutList model
        , text " "
        , sendButton SendDeleteList model
        , br
        , textInput "account ids (a,b,...): "
            40
            (ExplorerUIMsg << SetAccountIds)
            model.accountIds
        , br
        , sendButton SendPostListAccounts model
        , text " "
        , sendButton SendDeleteListAccounts model
        ]


mutesSelectedUI : Model -> Html Msg
mutesSelectedUI model =
    p []
        [ pspace
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , br
        , sendButton SendGetAccountMutes model
        , br
        , textInput "account id: "
            25
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , br
        , text "-- writes below here --"
        , br
        , checkBox (ExplorerUIMsg ToggleMuteNotifications)
            model.muteNotifications
            "mute notifications"
        , text " "
        , sendButton SendPostAccountMute model
        , text " "
        , sendButton SendPostAccountUnmute model
        , br
        , textInput "status id: "
            25
            (ExplorerUIMsg << SetStatusId)
            model.statusId
        , br
        , sendButton SendPostStatusMute model
        , text " "
        , sendButton SendPostStatusUnmute model
        ]


notificationsSelectedUI : Model -> Html Msg
notificationsSelectedUI model =
    p []
        [ pspace
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , text " "
        , checkBox (ExplorerUIMsg ToggleSmartPaging)
            model.smartPaging
            "smart paging "
        , br
        , textInput "max id: "
            25
            (ExplorerUIMsg << SetMaxId)
            model.pagingInput.max_id
        , text " "
        , textInput "min id: "
            25
            (ExplorerUIMsg << SetMinId)
            model.pagingInput.min_id
        , br
        , textInput "since id: "
            25
            (ExplorerUIMsg << SetSinceId)
            model.pagingInput.since_id
        , br
        , b "excluded notifications: "
        , excludedNotificationTypeCheckbox "follow" FollowNotification model
        , excludedNotificationTypeCheckbox "mention" MentionNotification model
        , excludedNotificationTypeCheckbox "reblog" ReblogNotification model
        , excludedNotificationTypeCheckbox "favourite " FavouriteNotification model
        , excludedNotificationTypeCheckbox "poll" PollNotification model
        , br
        , button (ExplorerUIMsg IncludeAllNotifications) "include all"
        , text " "
        , button (ExplorerUIMsg IncludeOnlyMentionNotifications) "mentions only"
        , br
        , textInput "from account id only: "
            25
            (ExplorerUIMsg << SetNotificationsAccountId)
            model.notificationsAccountId
        , br
        , sendButton SendGetNotifications model
        , br
        , textInput "notification id: "
            25
            (ExplorerUIMsg << SetNotificationId)
            model.notificationId
        , br
        , enabledSendButton (model.notificationId /= "")
            SendGetNotification
            model
        , br
        , text "-- writes below here --"
        , br
        , enabledSendButton (model.notificationId /= "")
            SendPostDismissNotification
            model
        , br
        , button
            ((GlobalMsg << SetDialog) <|
                ConfirmDialog "Clear all notifications? This cannot be undone."
                    "OK"
                    (ExplorerSendMsg SendPostClearNotifications)
            )
          <|
            sendButtonName model.useElmButtonNames SendPostClearNotifications
        , text " (after confirmation)"
        ]


scheduledStatusesSelectedUI : Model -> Html Msg
scheduledStatusesSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetScheduledStatuses model
        , br
        , textInput "scheduled status id: "
            25
            (ExplorerUIMsg << SetScheduledStatusId)
            model.scheduledStatusId
        , br
        , sendButton SendGetScheduledStatus model
        , text " "
        , sendButton SendDeleteScheduledStatus model
        , br
        , textInput "scheduled at: "
            40
            (ExplorerUIMsg << SetScheduledAt)
            model.scheduled_at
        , text " "
        , sendButton SendPutScheduledStatus model
        ]


searchSelectedUI : Model -> Html Msg
searchSelectedUI model =
    p []
        [ pspace
        , textInput "q: "
            40
            (ExplorerUIMsg << SetQ)
            model.q
        , br
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , text " "
        , textInput "offset: "
            10
            (ExplorerUIMsg << SetOffset)
            model.offset
        , br
        , checkBox (ExplorerUIMsg ToggleResolve) model.resolve "resolve "
        , checkBox (ExplorerUIMsg ToggleFollowing) model.following "following "
        , br
        , sendButton SendGetSearch model
        ]


reportsSelectedUI : Model -> Html Msg
reportsSelectedUI model =
    p []
        [ pspace
        , textInput "account id: "
            25
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , br
        , textInput "status ids (a,b,...): "
            60
            (ExplorerUIMsg << SetStatusIds)
            model.statusIds
        , br
        , b "comment:"
        , br
        , textarea
            [ onInput (ExplorerUIMsg << SetReportComment)
            , value model.reportComment
            , rows 4
            , cols 80
            ]
            []
        , br
        , checkBox (ExplorerUIMsg ToggleForwardReport)
            model.forwardReport
            "forward"
        , text " "
        , sendButton SendPostReports model
        ]


statusesSelectedUI : Model -> Html Msg
statusesSelectedUI model =
    p []
        [ pspace
        , textInput "status id: "
            25
            (ExplorerUIMsg << SetStatusId)
            model.statusId
        , br
        , sendButton SendGetStatus model
        , text " "
        , sendButton SendGetStatusContext model
        , text " "
        , sendButton SendGetStatusCard model
        , br
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , br
        , sendButton SendGetStatusRebloggedBy model
        , text " "
        , sendButton SendGetStatusFavouritedBy model
        , br
        , text "-- writes below here --"
        , br
        , textInput "status id: "
            25
            (ExplorerUIMsg << SetStatusId)
            model.statusId
        , br
        , button
            ((GlobalMsg << SetDialog) <|
                ConfirmDialog "Delete Status?"
                    "OK"
                    (ExplorerSendMsg SendDeleteStatus)
            )
          <|
            sendButtonName model.useElmButtonNames SendDeleteStatus
        , text " (after confirmation)"
        , br
        , sendButton SendPostReblogStatus model
        , text " "
        , sendButton SendPostUnreblogStatus model
        , br
        , sendButton SendPostPinStatus model
        , text " "
        , sendButton SendPostUnpinStatus model
        , br
        , if not model.showPostStatus then
            let
                buttonName =
                    sendButtonName model.useElmButtonNames
                        SendPostStatus
            in
            button (ExplorerUIMsg ToggleShowPostStatus) <| "Show '" ++ buttonName ++ "'"

          else
            span []
                [ b "Status:"
                , br
                , textarea
                    [ onInput (ExplorerUIMsg << SetStatus)
                    , value model.status
                    , rows 4
                    , cols 80
                    ]
                    []
                , br
                , textInput "in reply to id: "
                    25
                    (ExplorerUIMsg << SetInReplyToId)
                    model.in_reply_to_id
                , br
                , textInput "group id: "
                    20
                    (ExplorerUIMsg << SetGroupId)
                    model.groupId
                , br
                , textInput "quote of id: "
                    25
                    (ExplorerUIMsg << SetInQuoteOfId)
                    model.quote_of_id
                , br
                , textInput "spoiler text: "
                    40
                    (ExplorerUIMsg << SetSpoilerText)
                    model.spoiler_text
                , text " "
                , checkBox (ExplorerUIMsg ToggleMediaSensitive)
                    model.media_sensitive
                    "sensitive"
                , br
                , b "visibility: "
                , visibilityRadio Nothing model
                , visibilityRadio (Just PublicVisibility) model
                , visibilityRadio (Just UnlistedVisibility) model
                , visibilityRadio (Just PrivateVisibility) model
                , visibilityRadio (Just DirectVisibility) model
                , br
                , textInput "scheduled at: "
                    40
                    (ExplorerUIMsg << SetScheduledAt)
                    model.scheduled_at
                , br
                , textInput "language: "
                    2
                    (ExplorerUIMsg << SetLanguage)
                    model.language
                , br
                , textInput "idempotency key: "
                    20
                    (ExplorerUIMsg << SetIdempotencyKey)
                    model.idempotencyKey
                , br
                , textInput "media ids (a,b,...): "
                    50
                    (ExplorerUIMsg << SetMediaIds)
                    model.media_ids
                , br
                , button (ExplorerUIMsg ToggleShowPostStatus) "Hide"
                , text " "
                , sendButton SendPostStatus model
                , br
                , text "-- new media --"
                , span []
                    [ text " ("
                    , link "docs" "https://docs.joinmastodon.org/methods/statuses/media/"
                    , text ")"
                    ]
                , br
                , renderChooseFile "media: "
                    model.mediaFile
                    (ExplorerUIMsg << GetMediaFile)
                , br
                , textInput "description: "
                    50
                    (ExplorerUIMsg << SetMediaDescription)
                    model.mediaDescription
                , br
                , textInput "focus x: "
                    5
                    (ExplorerUIMsg << SetMediaFocusX)
                    model.mediaFocus.x
                , textInput " y: "
                    5
                    (ExplorerUIMsg << SetMediaFocusY)
                    model.mediaFocus.y
                , text " "
                , enabledSendButton (isEnabledPostMedia model)
                    SendPostMedia
                    model
                , br
                , textInput "media id: "
                    20
                    (ExplorerUIMsg << SetMediaId)
                    model.media_id
                , text " "
                , enabledSendButton (isEnabledPutMedia model)
                    SendPutMedia
                    model
                , br
                , text "-- poll --"
                , br
                , case pollInvalidReason model of
                    Nothing ->
                        text " "

                    Just reason ->
                        span [ style "color" "red" ]
                            [ text reason
                            , br
                            ]
                , textInput "expires in: "
                    10
                    (ExplorerUIMsg << SetExpiresIn)
                    model.expires_in
                , text " "
                , checkBox (ExplorerUIMsg ToggleMultiple) model.multiple "multiple"
                , text " "
                , checkBox (ExplorerUIMsg ToggleHideTotals) model.hide_totals "hide totals"
                , br
                , b "options: "
                , let
                    len =
                        List.length model.pollOptions
                  in
                  span []
                    [ enabledButton (len > 2) (ExplorerUIMsg RemovePollOption) "-"
                    , text " "
                    , enabledButton (len < 4) (ExplorerUIMsg AddPollOption) "+"
                    ]
                , span [] <|
                    List.concat <|
                        List.indexedMap
                            (\idx option ->
                                [ br
                                , unlabeledTextInput 25
                                    (ExplorerUIMsg << SetPollOption idx)
                                    option
                                ]
                            )
                            model.pollOptions
                ]
        ]


parseFocus : FocusInput -> Maybe (Maybe Focus)
parseFocus { x, y } =
    if x == "" && y == "" then
        Just Nothing

    else
        case ( String.toFloat x, String.toFloat y ) of
            ( Just fx, Just fy ) ->
                if fx >= 0 && fx <= 1 && fy >= 0 && fy <= 1 then
                    Just <| Just { x = fx, y = fy }

                else
                    Nothing

            _ ->
                Nothing


pollInvalidReason : Model -> Maybe String
pollInvalidReason model =
    if LE.find ((/=) "") model.pollOptions == Nothing then
        Nothing

    else
        case LE.find ((==) "") model.pollOptions of
            Just _ ->
                Just "No poll option may be blank"

            Nothing ->
                case String.toInt model.expires_in of
                    Nothing ->
                        Just "\"expires in\" must be integer seconds"

                    Just seconds ->
                        if seconds <= 0 then
                            Just "\"expires in\" must be positive"

                        else
                            Nothing


pollDefinition : Model -> Maybe PollDefinition
pollDefinition model =
    if LE.find ((/=) "") model.pollOptions == Nothing then
        Nothing

    else if pollInvalidReason model /= Nothing then
        Nothing

    else
        Just
            { options = model.pollOptions
            , expires_in = Maybe.withDefault 0 <| String.toInt model.expires_in
            , multiple = model.multiple
            , hide_totals = model.hide_totals
            }


isEnabledPostMedia : Model -> Bool
isEnabledPostMedia model =
    case parseFocus model.mediaFocus of
        Nothing ->
            False

        Just _ ->
            model.mediaFile /= Nothing


isEnabledPutMedia : Model -> Bool
isEnabledPutMedia model =
    case parseFocus model.mediaFocus of
        Nothing ->
            False

        Just _ ->
            model.media_id /= ""


visibilityToString : Maybe Visibility -> String
visibilityToString visibility =
    case visibility of
        Nothing ->
            "none "

        Just vis ->
            case vis of
                PublicVisibility ->
                    "public "

                UnlistedVisibility ->
                    "unlisted "

                PrivateVisibility ->
                    "private "

                DirectVisibility ->
                    "direct "


visibilityRadio : Maybe Visibility -> Model -> Html Msg
visibilityRadio visibility model =
    let
        label =
            visibilityToString visibility
    in
    radioButton
        { buttonValue = visibility
        , radioValue = model.visibility
        , radioName = radioNames.privacy
        , setter = ExplorerUIMsg <| SetVisibility visibility
        , label = label
        }


timelinesSelectedUI : Model -> Html Msg
timelinesSelectedUI model =
    p []
        [ pspace
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , text " "
        , checkBox (ExplorerUIMsg ToggleSmartPaging)
            model.smartPaging
            "smart paging "
        , br
        , textInput "max id: "
            25
            (ExplorerUIMsg << SetMaxId)
            model.pagingInput.max_id
        , text " "
        , textInput "min id: "
            25
            (ExplorerUIMsg << SetMinId)
            model.pagingInput.min_id
        , br
        , textInput "since id: "
            25
            (ExplorerUIMsg << SetSinceId)
            model.pagingInput.since_id
        , br
        , sendButton SendGetHomeTimeline model
        , text " "
        , sendButton SendGetConversations model
        , text " "
        , sendButton SendGetProTimeline model
        , br
        , checkBox (ExplorerUIMsg ToggleLocal) model.local "local "
        , checkBox (ExplorerUIMsg ToggleOnlyMedia) model.onlyMedia "media only "
        , sendButton SendGetPublicTimeline model
        , br
        , textInput "hashtag: "
            30
            (ExplorerUIMsg << SetHashtag)
            model.hashtag
        , text " "
        , sendButton SendGetTagTimeline model
        , br
        , textInput "list id: "
            20
            (ExplorerUIMsg << SetListId)
            model.listId
        , text " "
        , sendButton SendGetListTimeline model
        , br
        , textInput "group id: "
            20
            (ExplorerUIMsg << SetGroupId)
            model.groupId
        , text " "
        , sendButton SendGetGroupTimeline model
        ]


trendsSelectedUI : Model -> Html Msg
trendsSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetTrends model
        ]


renderChooseFile : String -> Maybe File -> (Bool -> Msg) -> Html Msg
renderChooseFile label maybeFile getter =
    span []
        [ b label
        , case maybeFile of
            Nothing ->
                text "No file selected "

            Just file ->
                let
                    name =
                        File.name file

                    size =
                        File.size file
                in
                span []
                    [ text <| String.fromInt size
                    , text " "
                    , text name
                    , text " "
                    , button (getter True) "Clear"
                    , text " "
                    ]
        , button (getter False) "Choose File"
        ]


accountIsVerified : Model -> Bool
accountIsVerified model =
    case model.account of
        Nothing ->
            False

        Just account ->
            account.is_verified


instanceSelectedUI : Model -> Html Msg
instanceSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetInstance model
        , text " "
        , sendButton SendGetActivity model
        , text " "
        , sendButton SendGetPeers model
        ]


accountsSelectedUI : Model -> Html Msg
accountsSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetVerifyCredentials model
        , br
        , textInput "username: "
            30
            (ExplorerUIMsg << SetUsername)
            model.username
        , text " "
        , sendButton SendGetAccountByUsername model
        , br
        , textInput "account id: "
            20
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , text " "
        , sendButton SendGetAccount model
        , br
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , text " "
        , sendButton SendGetFollowers model
        , text " "
        , sendButton SendGetFollowing model
        , br
        , textInput "max id: "
            25
            (ExplorerUIMsg << SetMaxId)
            model.pagingInput.max_id
        , text " "
        , textInput "min id: "
            25
            (ExplorerUIMsg << SetMinId)
            model.pagingInput.min_id
        , br
        , textInput "since id: "
            25
            (ExplorerUIMsg << SetSinceId)
            model.pagingInput.since_id
        , text " "
        , checkBox (ExplorerUIMsg ToggleSmartPaging)
            model.smartPaging
            "smart paging "
        , br
        , checkBox (ExplorerUIMsg ToggleOnlyMedia)
            model.onlyMedia
            "media only"
        , text " "
        , checkBox (ExplorerUIMsg TogglePinned) model.pinned "pinned"
        , text " "
        , checkBox (ExplorerUIMsg ToggleExcludeReplies)
            (not model.excludeReplies)
            "replies"
        , text " "
        , checkBox (ExplorerUIMsg ToggleExcludeReblogs)
            (not model.excludeReblogs)
            "reblogs"
        , text " "
        , sendButton SendGetStatuses model
        , br
        , textInput "ids (1,2,...): "
            40
            (ExplorerUIMsg << SetAccountIds)
            model.accountIds
        , text " "
        , sendButton SendGetRelationships model
        , br
        , textInput "q: " 40 (ExplorerUIMsg << SetQ) model.q
        , br
        , textInput "limit: "
            10
            (ExplorerUIMsg << SetLimit)
            model.pagingInput.limit
        , text " "
        , checkBox
            (ExplorerUIMsg ToggleResolve)
            model.resolve
            " resolve "
        , checkBox (ExplorerUIMsg ToggleFollowing) model.following " following "
        , sendButton SendGetSearchAccounts model
        , br
        , text "-- writes below here --"
        , br
        , textInput "account id: "
            20
            (ExplorerUIMsg << SetAccountId)
            model.accountId
        , text " "
        , checkBox (ExplorerUIMsg ToggleFollowReblogs)
            model.followReblogs
            "reblogs "
        , if model.isAccountFollowed then
            sendButton SendPostUnfollow model

          else
            sendButton SendPostFollow model
        , br
        , if not <| accountIsVerified model then
            text ""

          else
            span []
                [ text "[account is verified. Changing display name will error]"
                , br
                ]
        , if not model.showUpdateCredentials then
            let
                buttonName =
                    sendButtonName model.useElmButtonNames
                        SendPatchUpdateCredentials
            in
            button (ExplorerUIMsg ToggleShowUpdateCredentials) <|
                "Show '"
                    ++ buttonName
                    ++ "'"

          else
            span []
                [ textInput "display name: "
                    40
                    (ExplorerUIMsg << SetDisplayName)
                    model.displayName
                , br
                , b "note:"
                , br
                , textarea
                    [ rows 4
                    , cols 50
                    , onInput (ExplorerUIMsg << SetNote)
                    , value model.note
                    ]
                    []
                , br
                , renderChooseFile "avatar: "
                    model.avatarFile
                    (ExplorerUIMsg << GetAvatarFile)
                , br
                , renderChooseFile "header: "
                    model.headerFile
                    (ExplorerUIMsg << GetHeaderFile)
                , br
                , b "privacy: "
                , privacyRadio PublicPrivacy "public " model
                , privacyRadio UnlistedPrivacy "unlisted " model
                , privacyRadio PrivatePrivacy "private " model
                , br
                , checkBox (ExplorerUIMsg ToggleLocked) model.locked "blocked"
                , checkBox (ExplorerUIMsg ToggleSensitive) model.sensitive "sensitive "
                , br
                , textInput "Language: " 2 (ExplorerUIMsg << SetLanguage) model.language
                , br
                , b "Profile metadata:"
                , br
                , List.map2 fieldEditorUI
                    model.fields
                    (List.range 0 <| List.length model.fields)
                    |> List.intersperse [ br ]
                    |> List.concat
                    |> span []
                , br
                , button (ExplorerUIMsg ToggleShowUpdateCredentials) "Hide"
                , text " "
                , sendButton SendPatchUpdateCredentials model
                ]
        ]


fieldEditorUI : Field -> Int -> List (Html Msg)
fieldEditorUI field idx =
    [ input
        [ size 20
        , onInput <| (ExplorerUIMsg << SetField idx False)
        , value field.name
        , placeholder "name"
        ]
        []
    , text " "
    , input
        [ size 30
        , onInput <| (ExplorerUIMsg << SetField idx True)
        , value field.value
        , placeholder "value"
        ]
        []
    ]


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (JD.map tagger keyCode)


loginSelectedUI : Bool -> Model -> Html Msg
loginSelectedUI showSetServerButton model =
    p []
        [ pspace
        , b "Server: "
        , br
        , serverSelect model
        , text " "
        , input
            [ id nodeIds.loginServer
            , size 30
            , onInput (GlobalMsg << SetServer)
            , value model.server
            , placeholder "mastodon.online"
            , onKeyUp
                (\code ->
                    if code == 13 then
                        GlobalMsg Login

                    else
                        Noop
                )
            ]
            []
        , br
        , renderLoginButton model
        , if showSetServerButton then
            span []
                [ text " "
                , button (GlobalMsg SetLoginServer) "Set Server"
                ]

          else
            text ""
        ]


renderLoginButton : Model -> Html Msg
renderLoginButton model =
    Html.button
        [ onClick (GlobalMsg Login)
        , disabled <| model.server == ""
        ]
        [ b "Login" ]


renderHeaders : Bool -> String -> Http.Metadata -> Html Msg
renderHeaders prettify color metadata =
    let
        fold k v res =
            let
                vline =
                    if not prettify then
                        v

                    else
                        wrapJsonLine wrapColumns v
                            |> String.join "\n"
            in
            tr []
                [ td [] [ pre [] [ text k ] ]
                , td [] [ pre [] [ text vline ] ]
                ]
                :: res
    in
    span []
        [ b "status code: "
        , text <| String.fromInt metadata.statusCode
        , br
        , b "status text: "
        , text metadata.statusText
        , Dict.foldr fold [] metadata.headers
            |> table
                [ style "color" color
                ]
        ]


type NodeId
    = CancelButtonId
    | PostDialogTextId
    | LoginServerId


nodeIdAlist : List ( NodeId, String )
nodeIdAlist =
    [ ( CancelButtonId, nodeIds.cancelButton )
    , ( PostDialogTextId, nodeIds.postDialogText )
    , ( LoginServerId, nodeIds.loginServer )
    ]


focusId : NodeId -> Cmd Msg
focusId nodeId =
    case LE.find (\( id, _ ) -> id == nodeId) nodeIdAlist of
        Nothing ->
            Cmd.none

        Just ( _, id ) ->
            Task.attempt (\_ -> Noop) <|
                Dom.focus id


nodeIds =
    { cancelButton = "cancelButton"
    , postDialogText = "postDialogText"
    , loginServer = "loginServer"
    , userNameInput = "userNameInput"
    , groupNameInput = "groupNameInput"
    , hashtagInput = "hashtagInput"
    , showListsButton = "showListsButton"
    , postGroupInput = "postGroupInput"
    , threadExplorerStatusDiv = "threadExplorerStatusDiv"
    , threadExplorerHeader = "threadExplorerHeader"
    , threadExplorerStatus = "threadExplorerStatus"
    }


threadExplorerStatusId : Int -> String
threadExplorerStatusId idx =
    nodeIds.threadExplorerStatus ++ "-" ++ String.fromInt idx


popupPicker : RenderEnv -> PopupPicker PopupChoice Msg
popupPicker renderEnv =
    let
        picker =
            PopupPicker.makePopupPicker (renderPopupChoice renderEnv)
                (ColumnsUIMsg << PopupChoose)

        { backgroundColor, color } =
            getStyle renderEnv.style
    in
    { picker
        | divAttributes =
            [ style "border" <| "2px solid " ++ scrollPillBackground
            , style "color" color
            , style "background-color" backgroundColor
            , style "min-width" "5em"
            , PopupPicker.zIndex zIndices.popup
            , style "overflow" "auto"
            ]
        , choiceAttributes =
            [ style "width" "100%"
            , style "padding" "0.25em 0.5em"
            ]
    }


popupPositionAttributes : Int -> RenderEnv -> Dom.Element -> List (Attribute msg)
popupPositionAttributes choiceWidth renderEnv element =
    let
        el =
            element.element

        ( w, h ) =
            renderEnv.windowSize

        x =
            max 5 (min (toFloat <| w - choiceWidth) (el.x - 20))

        xs =
            String.fromFloat x ++ "px"

        maxw =
            w - ceiling x - 10

        ( maxh, attrs ) =
            if el.y > toFloat h / 2 then
                let
                    yy =
                        el.y - 10

                    ys =
                        String.fromFloat (toFloat h - yy) ++ "px"
                in
                ( floor yy
                , [ PopupPicker.left xs
                  , PopupPicker.bottom ys
                  ]
                )

            else
                let
                    yy =
                        el.y + el.height + 10

                    ys =
                        String.fromFloat yy ++ "px"
                in
                ( h - ceiling yy - 20
                , PopupPicker.position ( xs, ys )
                )
    in
    List.concat
        [ [ style "max-width" <| px maxw
          , style "max-height" <| px maxh
          ]
        , attrs
        ]


renderPopupChoice : RenderEnv -> PopupChoice -> Html Msg
renderPopupChoice renderEnv choice =
    case choice of
        AccountChoice account ->
            span
                [ title <| "@" ++ account.acct
                , class (getStyle renderEnv.style |> .popupChoiceClass)
                ]
                [ imageFromSpec
                    { imageUrl = account.avatar
                    , linkUrl = ""
                    , altText = account.acct
                    , borderColor =
                        if account.is_pro then
                            Just "gold"

                        else
                            Nothing
                    , h = "1.5em"
                    }
                , text " "
                , renderDisplayName account.display_name renderEnv
                , text " @"
                , text account.acct
                , if not account.is_verified then
                    text ""

                  else
                    span []
                        -- style "padding-left" "0.25em" ]
                        [ blueCheck 100 ]
                , text <| special.nbsp ++ special.nbsp
                , text <| iso8601ToMonthYear renderEnv.here account.created_at
                , text " ("
                , text <| String.fromInt account.following_count
                , text " / "
                , text <| String.fromInt account.followers_count
                , text ")"
                ]

        GroupChoice group ->
            span
                [ title group.description
                , class (getStyle renderEnv.style |> .popupChoiceClass)
                ]
                [ imageFromSpec
                    { imageUrl = group.cover_image_url
                    , linkUrl = ""
                    , altText = group.title
                    , borderColor = Nothing
                    , h = "1.5em"
                    }
                , text " "
                , text group.title
                , text " ("
                , text <| String.fromInt group.member_count
                , text " members)"
                ]

        HashtagChoice hashtag ->
            span
                [ title <| "#" ++ hashtag
                , class (getStyle renderEnv.style |> .popupChoiceClass)
                ]
                [ text <| "#" ++ hashtag ]

        PostGroupChoice group ->
            renderPopupChoice renderEnv (GroupChoice group)

        PostEmojiChoice emoji ->
            -- TODO, add image.
            let
                size =
                    18 * renderEnv.fontSizePct // 100
            in
            span []
                [ img
                    [ src emoji.url
                    , draggable "false"
                    , style "height" <| px size
                    ]
                    []
                , text <| " " ++ emoji.shortcode
                ]

        PostEmojiCharChoice emojiChar ->
            text <| emojiChar.char ++ " " ++ emojiChar.name


renderPopup : Model -> Html Msg
renderPopup model =
    case model.popupChoices of
        [] ->
            text ""

        choices ->
            case model.popupElement of
                Nothing ->
                    text ""

                Just element ->
                    let
                        choiceWidth =
                            case model.popup of
                                UserNamePopup ->
                                    1024

                                GroupNamePopup ->
                                    1024

                                PostGroupPopup ->
                                    1024

                                HashtagPopup ->
                                    300

                                PostTextPopup { popupType } ->
                                    case popupType of
                                        PostPopupAtsign ->
                                            1024

                                        _ ->
                                            300

                                NoPopup ->
                                    300

                        positionAttributes =
                            popupPositionAttributes choiceWidth
                                model.renderEnv
                                element

                        picker =
                            popupPicker model.renderEnv

                        picker2 =
                            { picker | positionAttributes = positionAttributes }
                    in
                    PopupPicker.view Nothing choices picker2


{-| Higher numbers are on top.
-}
zIndices =
    { explorer = 5
    , scrollPill = 7
    , dialog = 10 --wired in to Dialog
    , popup = 20
    }


renderPopupExplorer : Model -> Html Msg
renderPopupExplorer model =
    case model.popupExplorer of
        NoPopupExplorer ->
            text ""

        ThreadPopupExplorer state ->
            renderThreadExplorer state model


renderThreadExplorer : ThreadExplorerState -> Model -> Html Msg
renderThreadExplorer state model =
    let
        renderEnv =
            model.renderEnv

        feedEnv =
            { emptyFeedEnv | references = model.references }

        { backgroundColor, color, highlightStatusColor, repliedToStatusColor, visitedStatusColor } =
            getStyle renderEnv.style

        ( maxWidth, maxHeight ) =
            renderEnv.windowSize

        columnWidth =
            min maxWidth <| 2 * renderEnv.columnWidth

        renderEnv2 =
            { renderEnv | columnWidth = columnWidth }

        wpx =
            px columnWidth

        margin =
            10

        left =
            (maxWidth - columnWidth) // 2

        lpx =
            px left

        headerHeight =
            case Debug.log "render headerHeight" state.headerHeight of
                Nothing ->
                    25

                Just h ->
                    round h

        hpx =
            px (maxHeight - 20 - headerHeight - (2 * margin))
    in
    case state.ribbon of
        [] ->
            text ""

        { status, displayed, visited } :: _ ->
            let
                renderAStatus s idx replyChain =
                    let
                        nodeid =
                            threadExplorerStatusId idx
                    in
                    if s.id == status.id then
                        div [ style "background-color" highlightStatusColor ]
                            [ renderStatusWithId (Just nodeid) renderEnv2 feedEnv s ]

                    else if not replyChain && s.replies_count > 0 then
                        div
                            [ style "background-color" <|
                                if Set.member s.id visited then
                                    visitedStatusColor

                                else
                                    repliedToStatusColor
                            ]
                            [ renderStatusWithId (Just nodeid) renderEnv2 feedEnv s ]

                    else
                        renderStatusWithId (Just nodeid) renderEnv2 feedEnv s

                loop : Int -> Bool -> List Status -> List (Html Msg) -> List (Html Msg)
                loop idx replyChain statuses res =
                    case statuses of
                        [] ->
                            List.reverse res

                        s :: tail ->
                            let
                                rc =
                                    if replyChain then
                                        True

                                    else
                                        isReplyChain tail

                                -- Highlight is different for the last one
                                sReplyChain =
                                    if tail == [] then
                                        s.replies_count == 0

                                    else
                                        replyChain

                                html =
                                    renderAStatus s idx sReplyChain
                            in
                            loop (idx + 1) rc tail <| html :: res
            in
            div
                [ style "width" wpx
                , style "overflow-x" "default"
                , style "border" <| "2px solid " ++ color
                , style "color" color
                , style "background-color" backgroundColor
                , style "position" "absolute"
                , PopupPicker.top <| px margin
                , PopupPicker.left lpx
                , PopupPicker.zIndex zIndices.explorer
                ]
                [ div
                    [ id nodeIds.threadExplorerHeader
                    , style "width" "100%"
                    , style "border" <| "1px solid " ++ color
                    ]
                    [ p
                        [ style "padding" "0 2px"
                        , class "content"
                        , style "margin-bottom" "0"
                        ]
                        [ Html.span
                            [ onClick <| ColumnsUIMsg ClosePopupExplorer
                            , title "Close"
                            , style "float" "right"
                            ]
                            [ Html.i
                                [ style "font-size" "120%"
                                , class "icon-cancel"
                                ]
                                []
                            ]
                        , renderThreadExplorerRibbon state.ribbon renderEnv2
                        ]
                    ]
                , div
                    [ style "width" "100%"
                    , style "max-height" hpx
                    , style "overflow-y" "auto"
                    , style "overflow-x" "hidden"
                    , id nodeIds.threadExplorerStatusDiv
                    ]
                  <|
                    loop 1 (isReplyChain displayed) displayed []
                ]


renderThreadExplorerRibbon : List ScrolledStatus -> RenderEnv -> Html Msg
renderThreadExplorerRibbon ribbon renderEnv =
    if ribbon == [] then
        text "No ribbon. Shouldn't happen."

    else
        let
            here =
                renderEnv.here

            ribbonEntry { status } =
                let
                    timestring =
                        formatIso8601 here status.created_at

                    username =
                        status.account.username
                in
                a
                    [ href "#"
                    , onClick (ColumnsUIMsg <| SetThreadExplorerStatus status)
                    ]
                    [ text <| "@" ++ username ++ " " ++ timestring ]
        in
        span []
            (List.reverse ribbon
                |> List.map ribbonEntry
                |> List.intersperse (text " > ")
            )


renderDialog : Model -> Html Msg
renderDialog model =
    case model.dialog of
        NoDialog ->
            text ""

        EditColumnsDialog ->
            editColumnsDialog model

        ServerDialog ->
            serverDialog model

        PostDialog ->
            postDialog model

        SettingsDialog ->
            settingsDialog model

        KeyboardShortcutsDialog ->
            keyboardShortcutsDialog model

        SaveRestoreDialog ->
            saveRestoreDialog model

        AlertDialog content ->
            dialogRender
                model.renderEnv
                { styles =
                    [ ( "width", "40%" )
                    , ( "font-size", fspct model.renderEnv )
                    ]
                , title = "Alert"
                , content = [ text content ]
                , actionBar =
                    [ Html.button
                        [ onClick (ColumnsUIMsg DismissDialog)
                        , id nodeIds.cancelButton
                        ]
                        [ b "OK" ]
                    ]
                }
                (model.dialog /= NoDialog)

        ConfirmDialog content okButtonText msg ->
            dialogRender
                model.renderEnv
                { styles = [ ( "width", "40%" ) ]
                , title = "Confirm"
                , content = [ text content ]
                , actionBar =
                    [ Html.button
                        [ onClick <| (GlobalMsg << SetDialog) NoDialog
                        , id nodeIds.cancelButton
                        ]
                        [ b "Cancel" ]
                    , text <| String.repeat 4 special.nbsp
                    , button msg okButtonText
                    ]
                }
                (model.dialog /= NoDialog)


dialogRender : RenderEnv -> Dialog.Config msg -> Dialog.Visible -> Html msg
dialogRender renderEnv config visible =
    let
        { backgroundColor, color } =
            getStyle renderEnv.style
    in
    Dialog.render
        { config
            | styles =
                List.append
                    [ ( "color", color )
                    , ( "background-color", backgroundColor )
                    ]
                    config.styles
        }
        visible


serverDialog : Model -> Html Msg
serverDialog model =
    dialogRender
        model.renderEnv
        { styles =
            [ ( "font-size", fspct model.renderEnv )
            ]
        , title = "Server"
        , content = serverDialogContent model
        , actionBar =
            [ renderLoginButton model
            , button (ColumnsUIMsg DismissDialog) "Cancel"
            ]
        }
        True


serverDialogContent : Model -> List (Html Msg)
serverDialogContent model =
    let
        renderEnv =
            model.renderEnv
    in
    [ div []
        [ p []
            [ primaryServerLine model ]
        , p []
            [ loginSelectedUI False model ]
        ]
    ]


editColumnsDialog : Model -> Html Msg
editColumnsDialog model =
    let
        ( _, windowHeight ) =
            model.renderEnv.windowSize

        maxHeight =
            (90 * windowHeight // 100 |> String.fromInt) ++ "px"
    in
    dialogRender
        model.renderEnv
        { styles =
            [ ( "max-width", "95%" )
            , ( "max-height", "90%" )
            , ( "overflow", "auto" )
            , ( "font-size", fspct model.renderEnv )
            ]
        , title = "Edit Columns"
        , content = editColumnsDialogRows model
        , actionBar =
            [ button (ColumnsUIMsg DismissDialog) "OK" ]
        }
        True


plus : String
plus =
    "+"


setServerHasFeature : Maybe String -> String -> Bool -> Model -> Model
setServerHasFeature maybeServer featureName hasFeature model =
    case maybeServer of
        Nothing ->
            model

        Just server ->
            let
                features =
                    model.features
            in
            case Dict.get server features of
                Nothing ->
                    { model
                        | features =
                            Dict.insert server
                                (Dict.fromList [ ( featureName, hasFeature ) ])
                                features
                    }

                Just dict ->
                    { model
                        | features =
                            Dict.insert server
                                (Dict.insert featureName hasFeature dict)
                                features
                    }


serverHasFeature : Maybe String -> String -> Model -> Bool
serverHasFeature maybeServer featureName model =
    case serverKnowsFeature maybeServer featureName model of
        Nothing ->
            False

        Just has ->
            has


serverKnowsFeature : Maybe String -> String -> Model -> Maybe Bool
serverKnowsFeature maybeServer featureName model =
    case maybeServer of
        Nothing ->
            Nothing

        Just server ->
            case Dict.get server model.features of
                Nothing ->
                    Nothing

                Just dict ->
                    case Dict.get featureName dict of
                        Nothing ->
                            Nothing

                        Just hasFeature ->
                            Just hasFeature


computeListSelector : Model -> Maybe (Html Msg)
computeListSelector model =
    case model.lists of
        [] ->
            Nothing

        lists ->
            let
                isListFeed feed =
                    case feed of
                        ListFeed _ ->
                            True

                        _ ->
                            False

                listFeedId feed =
                    case feed of
                        ListFeed id ->
                            id

                        _ ->
                            ""
            in
            let
                listIds =
                    List.filter isListFeed model.feedSetDefinition.feedTypes
                        |> List.map listFeedId

                availableLists =
                    List.filter (\list -> not <| List.member list.id listIds) lists
                        |> List.sortBy (\list -> list.title)
            in
            if availableLists == [] then
                Just <| text "All have columns."

            else
                let
                    currentList =
                        case model.selectedList of
                            Nothing ->
                                ""

                            Just { id } ->
                                id

                    listOption list =
                        option
                            [ value list.id
                            , selected <| currentList == list.id
                            ]
                            [ text list.title ]
                in
                select [ onInput (ColumnsUIMsg << SetSelectedList) ]
                    (option [ value "" ]
                        [ text "-- select a list --" ]
                        :: List.map listOption availableLists
                    )
                    |> Just


editColumnsDialogRows : Model -> List (Html Msg)
editColumnsDialogRows model =
    let
        feedTypes =
            model.feedSetDefinition.feedTypes

        row : List (Html Msg) -> Msg -> Html Msg
        row =
            conditionalRow (\_ -> True)

        conditionalRow : (Model -> Bool) -> List (Html Msg) -> Msg -> Html Msg
        conditionalRow pred td1 msg =
            tr []
                [ td [] td1
                , td [] [ enabledButton (pred model) msg "+" ]
                ]

        renderEnv =
            model.renderEnv
    in
    -- This will change quite a bit when I add multiple-server support
    [ table [] <|
        List.concat
            [ case model.editColumnsMessage of
                Nothing ->
                    []

                Just message ->
                    [ span [ style "color" "red" ]
                        [ text message ]
                    ]
            , if List.member HomeFeed feedTypes then
                []

              else
                [ row [ b "Home" ] (ColumnsUIMsg <| AddFeedColumn HomeFeed) ]
            , case
                LE.find
                    (\feedType ->
                        case feedType of
                            NotificationFeed _ ->
                                True

                            _ ->
                                False
                    )
                    feedTypes
              of
                Just _ ->
                    []

                Nothing ->
                    [ row [ b "Notifications" ]
                        (ColumnsUIMsg <|
                            AddFeedColumn
                                (NotificationFeed
                                    { accountId = Nothing
                                    , exclusions = []
                                    }
                                )
                        )
                    ]
            , case
                LE.find
                    (\feedType ->
                        case feedType of
                            PublicFeed _ ->
                                True

                            ProFeed _ ->
                                True

                            _ ->
                                False
                    )
                    feedTypes
              of
                Just _ ->
                    []

                Nothing ->
                    let
                        ( feedType, name ) =
                            if
                                serverHasFeature renderEnv.loginServer
                                    featureNames.proFeed
                                    model
                            then
                                ( ProFeed { flags = Nothing }, "Pro" )

                            else
                                ( PublicFeed { flags = Nothing }, "Public" )
                    in
                    [ row [ b name ]
                        (ColumnsUIMsg <|
                            AddFeedColumn feedType
                        )
                    ]
            , [ row
                    [ b "User: "
                    , input
                        [ id nodeIds.userNameInput
                        , autocapitalize "off"
                        , autocomplete False
                        , size 30
                        , onInput (ColumnsUIMsg << UserNameInput)
                        , value model.userNameInput
                        , placeholder <|
                            "username@"
                                ++ Maybe.withDefault "server" renderEnv.loginServer
                        ]
                        []
                    ]
                    (ColumnsUIMsg <| AddFeedColumn Types.defaultUserFeedType)
              ]
            , [ if not <| serverHasFeature renderEnv.loginServer featureNames.groups model then
                    text ""

                else
                    row
                        [ b "Group: "
                        , input
                            [ id nodeIds.groupNameInput
                            , size 30
                            , autocapitalize "off"
                            , autocomplete False
                            , onInput (ColumnsUIMsg << GroupNameInput)
                            , value model.groupNameInput
                            , placeholder "Group name"
                            ]
                            []
                        ]
                        (ColumnsUIMsg <| AddFeedColumn Types.defaultGroupFeedType)
              ]
            , [ row
                    [ b "Hashtag: "
                    , input
                        [ id nodeIds.hashtagInput
                        , size 30
                        , autocapitalize "off"
                        , autocomplete False
                        , onInput (ColumnsUIMsg << HashtagInput)
                        , value model.hashtagInput
                        , placeholder "hashtag"
                        ]
                        []
                    ]
                    (ColumnsUIMsg <| AddFeedColumn Types.defaultHashtagFeedType)
              ]
            , case computeListSelector model of
                Nothing ->
                    []

                Just selector ->
                    [ conditionalRow
                        (\mdl -> mdl.selectedList /= Nothing)
                        [ b "List: "
                        , selector
                        ]
                        (ColumnsUIMsg <| AddFeedColumn Types.defaultListFeedType)
                    ]
            ]
    , if [] == feedTypes then
        text ""

      else
        div
            [ style "display" "flex"
            , style "flex" "1"
            , style "margin-bottom" "0.5em"
            ]
            [ text "Click a "
            , fontelloChar [] "icon-menu" [] model
            , text " to move that column."
            ]
    , hrpct 100
    , let
        feedRow feedType =
            let
                title =
                    smartFeedTitle feedType model
            in
            tr []
                [ if Just feedType == model.movingColumn then
                    text ""

                  else
                    td []
                        [ div
                            [ style "display" "flex"
                            , style "flex" "1"
                            ]
                            [ if Just feedType == model.movingColumn then
                                text ""

                              else
                                fontelloChar
                                    [ onClick (ColumnsUIMsg <| MoveFeedColumn feedType)
                                    , Html.Attributes.title <|
                                        if model.movingColumn == Nothing then
                                            "Click to move this column"

                                        else
                                            "Click to move selected column to here."
                                    ]
                                    "icon-menu"
                                    []
                                    model
                            ]
                        ]
                , td
                    [ colspan <|
                        if Just feedType == model.movingColumn then
                            2

                        else
                            1
                    ]
                    [ span
                        (case model.movingColumn of
                            Nothing ->
                                []

                            Just clickedFeedType ->
                                [ Html.Attributes.title <|
                                    if feedType == clickedFeedType then
                                        "Click to cancel move."

                                    else
                                        "Click to move selected column here"
                                , onClick
                                    (ColumnsUIMsg <| MoveFeedColumn feedType)
                                ]
                        )
                        [ title
                        , text special.nbsp
                        ]
                    ]
                , td []
                    [ titledButton "Remove this column"
                        True
                        (ColumnsUIMsg <| DeleteFeedColumn feedType)
                        "x"
                    ]
                ]

        ( _, windowHeight ) =
            renderEnv.windowSize

        maxHeight =
            (String.fromInt <| 70 * windowHeight // 100) ++ "px"
      in
      div
        [--style "max-height" "70%"
         --, style "overflow" "auto"
        ]
        [ table [] <|
            List.map feedRow feedTypes
        ]
    ]


keyboardShortcutsDialog : Model -> Html Msg
keyboardShortcutsDialog model =
    dialogRender
        model.renderEnv
        { styles =
            [ ( "font-size", fspct model.renderEnv )
            ]
        , title = "Keyboard Shortcuts"
        , content = keyboardShortcutsDialogRows model
        , actionBar =
            [ button (ColumnsUIMsg DismissDialog) "OK" ]
        }
        True


keyboardShortcutsDialogRows : Model -> List (Html Msg)
keyboardShortcutsDialogRows model =
    let
        row key line1 line2 =
            tr []
                [ td [ style "padding-right" "1em" ]
                    [ text key ]
                , td [] <|
                    text line1
                        :: (case line2 of
                                Nothing ->
                                    []

                                Just l2 ->
                                    [ br, text l2 ]
                           )
                ]
    in
    [ p [] [ text "Active when no dialog is up (except <esc>)." ]
    , table []
        [ row "p" "Show Post dialog" Nothing
        , row "r" "Reload all columns" Nothing
        , row "," "Show Settings dialog" Nothing
        , row "t" "Toggle Dark Mode" Nothing
        , row "o" "Save/Restore Dialog" Nothing
        , row "?" "Show Keyboard Shortcuts dialog" Nothing
        , row "j / l" "Scroll one page left/right" <|
            Just "(all the way on two keystrokes quickly)"
        , row "a / d" "Scroll one page left/right (likewise)" Nothing
        , row "esc" "Dismiss dialog, collapse scroll pill" Nothing
        ]
    ]


saveRestoreDialog : Model -> Html Msg
saveRestoreDialog model =
    dialogRender
        model.renderEnv
        { styles =
            [ ( "width", "40em" )
            , ( "max-width", "95%" )
            , ( "max-height", "90%" )
            , ( "overflow", "auto" )
            , ( "font-size", fspct model.renderEnv )
            ]
        , title = "Save / Restore"
        , content = saveRestoreDialogRows model
        , actionBar =
            [ button (ColumnsUIMsg DismissDialog) "OK" ]
        }
        True


saveRestoreDialogRows : Model -> List (Html Msg)
saveRestoreDialogRows model =
    let
        { inputBackground, color } =
            getStyle model.renderEnv.style

        feedsText =
            case model.feedsText of
                Just txt ->
                    txt

                Nothing ->
                    model.feedSetDefinition.feedTypes
                        |> JE.list MED.encodeFeedType
                        |> JE.encode 2

        modelText =
            case model.modelText of
                Just txt ->
                    txt

                Nothing ->
                    let
                        mdl =
                            { model
                                | feedSetDefinition =
                                    { name = "", feedTypes = [] }
                                , token = Nothing
                                , postState = initialPostState
                            }
                    in
                    modelToSavedModel mdl
                        |> encodeSavedModel
                        |> JE.encode 2
    in
    [ b "Feeds:"
    , br
    , textarea
        [ rows 10
        , style "width" "100%"
        , style "color" color
        , style "background-color" inputBackground
        , onInput (ColumnsUIMsg << SetFeedsText)
        , value feedsText
        ]
        []
    , br
    , enabledButton (model.feedsText /= Nothing)
        (ColumnsUIMsg ClearFeedsText)
        "Resample"
    , br
    , br
    , b "Model:"
    , br
    , textarea
        [ rows 10
        , style "width" "100%"
        , style "color" color
        , style "background-color" inputBackground
        , onInput (ColumnsUIMsg << SetModelText)
        , value modelText
        ]
        []
    , br
    , enabledButton (model.modelText /= Nothing)
        (ColumnsUIMsg ClearModelText)
        "Resample"
    , br
    , br
    , b "Storage:"
    , br
    , textarea
        [ rows 10
        , style "width" "100%"
        , style "color" color
        , style "background-color" inputBackground
        , disabled True
        , value <| storageReadsText 2 model
        ]
        []
    ]


storageReadsText : Int -> Model -> String
storageReadsText indent model =
    JE.dict identity (\x -> ED.encodeMaybe identity x) model.storageReads
        |> JE.encode indent


fontelloChar : List (Attribute Msg) -> String -> List (Attribute Msg) -> Model -> Html Msg
fontelloChar divAttributes iClass iAttributes model =
    div
        (class "status-el" :: divAttributes)
        [ Html.i (class iClass :: iAttributes)
            []
        ]


smartFeedTitle : FeedType -> Model -> Html Msg
smartFeedTitle feedType model =
    case feedType of
        GroupFeed group_id ->
            case getGroup group_id model of
                Just group ->
                    b group.title

                _ ->
                    feedTitle feedType

        ListFeed list_id ->
            case LE.find (\list -> list_id == list.id) model.lists of
                Just list ->
                    b list.title

                _ ->
                    feedTitle feedType

        _ ->
            feedTitle feedType


type ReplyType
    = ReplyToPost
    | QuotePost
    | NoReply


type alias PostState =
    { replyTo : Maybe Status
    , replyType : ReplyType
    , text : String
    , mentionsString : String
    , sensitive : Bool
    , media_ids : List String
    , fileNames : List String
    , fileUrls : List String
    , groupName : String
    , group_id : Maybe String
    }


initialPostState : PostState
initialPostState =
    { replyTo = Nothing
    , replyType = NoReply
    , text = ""
    , mentionsString = ""
    , sensitive = False
    , media_ids = []
    , fileNames = []
    , fileUrls = []
    , groupName = ""
    , group_id = Nothing
    }


postDialog : Model -> Html Msg
postDialog model =
    let
        postState =
            model.postState

        renderEnv =
            model.renderEnv

        hasQuoteFeature =
            serverHasFeature renderEnv.loginServer featureNames.quote model

        hasGroupsFeature =
            serverHasFeature renderEnv.loginServer featureNames.groups model
    in
    dialogRender
        renderEnv
        { styles =
            [ ( "width", "50em" )
            , ( "font-size", fspct model.renderEnv )
            ]
        , title =
            case postState.replyType of
                ReplyToPost ->
                    "Reply"

                QuotePost ->
                    "Quote"

                _ ->
                    "Post"
        , content =
            postDialogContent ( hasQuoteFeature, hasGroupsFeature )
                model.renderEnv
                model.dropZone
                postState
        , actionBar =
            [ enabledButton
                ((postState.text /= "" || postState.media_ids /= [])
                    && (List.length postState.fileNames == List.length postState.media_ids)
                )
                (ColumnsUIMsg Post)
                "Post"
            , if model.showLeftColumn || model.scrollPillState.showScrollPill then
                text ""

              else
                button (ColumnsUIMsg ToggleShowScrollPill) "Show Scroll Pill"
            , button (ColumnsUIMsg DismissDialog) "Cancel"
            ]
        }
        True


statusMentionsString : String -> Status -> String
statusMentionsString me status =
    let
        addMention acct res =
            if acct == me then
                res

            else
                "@" ++ acct ++ " " ++ res
    in
    List.map .acct status.mentions
        |> (::) status.account.acct
        |> LE.unique
        |> List.foldr addMention ""


addPostStateMentions : String -> PostState -> PostState
addPostStateMentions me postState =
    let
        postText =
            postState.text
    in
    case postState.replyTo of
        Nothing ->
            if postText == postState.mentionsString then
                { postState | text = "" }

            else
                postState

        Just replyTo ->
            if postText /= "" && postText /= postState.mentionsString then
                postState

            else
                let
                    mentionsString =
                        statusMentionsString me replyTo
                in
                { postState
                    | text = mentionsString
                    , mentionsString = mentionsString
                }


maximumPostAttachments : Int
maximumPostAttachments =
    4


postDialogContent : ( Bool, Bool ) -> RenderEnv -> DropZone.Model -> PostState -> List (Html Msg)
postDialogContent ( hasQuoteFeature, hasGroupsFeature ) renderEnv dropZone postState =
    let
        { inputBackground, color } =
            getStyle renderEnv.style
    in
    [ case postState.replyTo of
        Nothing ->
            text ""

        Just replyTo ->
            let
                timeString =
                    formatIso8601 renderEnv.here replyTo.created_at

                replyType =
                    postState.replyType

                preposition =
                    case replyType of
                        ReplyToPost ->
                            "to "

                        QuotePost ->
                            "of "

                        NoReply ->
                            "by "

                replyRadio val lab =
                    radioButton
                        { buttonValue = val
                        , radioValue = replyType
                        , radioName = "replyType"
                        , setter = ColumnsUIMsg <| SetPostReplyType val
                        , label = lab
                        }
            in
            p []
                [ text preposition
                , renderDisplayName replyTo.account.display_name renderEnv
                , br
                , case replyTo.url of
                    Nothing ->
                        text timeString

                    Just url ->
                        link timeString url
                , br
                , replyRadio ReplyToPost "Reply"
                , if hasQuoteFeature then
                    span []
                        [ text " "
                        , replyRadio QuotePost "Quote"
                        , text " "
                        ]

                  else
                    text " "
                , replyRadio NoReply <|
                    if hasQuoteFeature then
                        "Neither"

                    else
                        "No Reply"
                , text " "
                , button (ColumnsUIMsg ClearPostStateReplyTo) "Clear Reply"
                ]
    , if not hasGroupsFeature then
        text ""

      else
        p []
            [ b "Group: "
            , input
                [ id nodeIds.postGroupInput
                , size 30
                , autocapitalize "off"
                , autocomplete False
                , onInput (ColumnsUIMsg << PostGroupNameInput)
                , value postState.groupName
                , placeholder "Group name"
                ]
                []
            , text " "
            , input
                [ type_ "checkbox"
                , checked <| postState.group_id /= Nothing
                , onClick <| ColumnsUIMsg TogglePostGroup
                ]
                []
            , br
            ]
    , p []
        [ textarea
            [ id nodeIds.postDialogText
            , rows 20
            , style "width" "100%"
            , style "color" color
            , style "background-color" inputBackground
            , onInput (ColumnsUIMsg << SetPostText)
            , value postState.text
            ]
            []
        ]
    , p []
        [ let
            fileCount =
                List.length postState.fileNames

            enabled =
                (fileCount < maximumPostAttachments)
                    && (fileCount == List.length postState.media_ids)
          in
          enabledButton enabled
            (ColumnsUIMsg ChoosePostAttachment)
            "Choose File"
        , text " "
        , renderDropZone dropZone
        ]
    , p []
        [ let
            urls =
                postState.fileUrls

            fileNames =
                postState.fileNames

            images =
                List.map3 postImage
                    fileNames
                    urls
                    (List.range 0 <| List.length urls - 1)
          in
          if images == [] then
            text ""

          else
            p []
                [ span [] <| List.intersperse (text " ") images
                , br
                , checkBox (ColumnsUIMsg TogglePostSensitive)
                    postState.sensitive
                    "Mark attachments as sensitive"
                , br
                , span [ style "font-size" smallTextFontSize ]
                    [ text "Click on image to remove it." ]
                ]
        ]
    ]


{-| TODO: hide this on mobile.
-}
renderDropZone : DropZone.Model -> Html Msg
renderDropZone dropZone =
    Html.map (ColumnsUIMsg << PostDrop) <|
        div
            ([ style "display" "inline-block"
             , style "vertical-align" "middle"
             , style "height" "2em"
             , style "width" "10em"
             , style "border-radius" "10px"
             ]
                ++ renderZoneAttributes dropZone
            )
            []


renderZoneAttributes : DropZone.Model -> List (Html.Attribute (DropZone.DropZoneMessage (List File)))
renderZoneAttributes dropZone =
    List.concat
        [ if DropZone.isHovering dropZone then
            dropZoneHover

          else
            dropZoneDefault
        , DropZone.dropZoneEventHandlers decodeFiles
        ]


dropZoneDefault : List (Html.Attribute a)
dropZoneDefault =
    [ style "border" "1px dashed steelblue"
    ]


dropZoneHover : List (Html.Attribute a)
dropZoneHover =
    [ style "border" "1px dashed red"
    , style "background-color" "#efefef"
    ]


decodeFiles : JD.Decoder (List File)
decodeFiles =
    JD.field "dataTransfer" (JD.field "files" (JD.list File.decoder))



{- For testing

   type alias DzFile =
       { name : String, size : Int, lastModified : Int, fileType : String }


   toFileList : DzFile -> List DzFile -> List DzFile
   toFileList f lf =
       f :: lf


   decodeFileList : JD.Decoder DzFile
   decodeFileList =
       JD.map4 DzFile
           (JD.field "name" JD.string)
           (JD.field "size" JD.int)
           (JD.field "lastModified" JD.int)
           (JD.field "type" JD.string)
-}


postImage : String -> String -> Int -> Html Msg
postImage fileName url index =
    img
        [ src url
        , alt <| fileName
        , style "height" "4em"
        , onClick <| (ColumnsUIMsg <| DeletePostAttachment index)
        ]
        []


dollarButtonNameToSendName : Bool -> String -> String
dollarButtonNameToSendName useElmButtonNames dollarButtonName =
    let
        name =
            dollarButtonNameToMsg dollarButtonName
                |> sendButtonName useElmButtonNames
    in
    if name == unknownButtonName then
        dollarButtonName

    else
        name


replaceSendButtonNames : Bool -> String -> String
replaceSendButtonNames useElmButtonNames string =
    case Regex.fromString "\\$[A-Za-z]+" of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex
                (.match >> dollarButtonNameToSendName useElmButtonNames)
                string


{-| `$Foo` will be replaced in strings below by `replaceSendButtonNames`.
-}
explorerHelp : Model -> Html Msg
explorerHelp model =
    Markdown.toHtml [] <|
        replaceSendButtonNames model.useElmButtonNames <|
            case model.selectedRequest of
                InstanceSelected ->
                    """
**Instance Information Help**

The "$GetInstance" button fetches the `Instance` entity for the "Use API for" instance.

The "$GetActivity" button fetches a list of `Activity` entities.

The "$GetPeers" button fetches a list of peer domain names.
                """

                AccountsSelected ->
                    """
**AccountsRequest Help**

The "$GetVerifyCredentials" button fetches the `Account` entity for the logged-in user.

The "$GetAccountByUsername" button fetches the `Account` entity for the user with the given "username". If the "username" is blank, it uses the username of the logged in user, or sends blank, which will result in an error, if not logged in. If successful, it fills in the "account id" with that user's account ID. This is a Gab-only feature.

The "$GetAccount" button fetches the `Account` entity for the user with the given "account id". If "account id" is blank, uses the id of the logged in user, or sends blank, which will result in an error, if not logged in.

The "$GetFollowers" and "$GetFollowing" buttons fetch lists of the associated `Account` entities. If "limit" is non-blank, it is the maximum number of entities to return.

The "$GetRelationships" button returns a list of `Relationship` entities, one for each of the (comma-separated) "ids".

The "$GetSearchAccounts" button returns a list of `Account` entities that match "q", "resolve", and "following". If "limit" is non-blank, it is the maximum number of entities to return.

The "$PostFollow" / "$PostUnfollow" button either follows or unfollows the account with the given "account id". If following, will show reblogs if and only if "reblogs" is checked. In order to decide whether to follow or unfollow when you click the button, every change to the "account id" causes A `GetRelationships` request to be sent.

Since the parameters to "$PatchUpdateCredentials" take a lot of screen space, that section is initially invisible. Click the "Show '$PatchUpdateCredentials'" button to reveal it.

The "$PatchUpdateCredentials" button changes account profile information from "Display name", "Note", "Avatar", "Header", "Privacy", "Locked", "Sensitive", "Language", and "Profile MetaData". Only the changed fields are sent to the server. The displayed field values are updated whenever a request returns the logged-in user's `Account` entity, e.g. at login or when you press the "$GetVerifyCredentials" button.

The "Hide" button to the left of "$PatchUpdateCredentials" hides that section of the user interface again.
              """

                BlocksSelected ->
                    """
**BlocksRequest Help**

The "$GetBlocks" button gets a list of blocked accounts, limited in number by "limit".

The "$PostBlock" button blocks the "account id", and returns a `Relationship` entity.

The "$PostUnblock" button unblocks the "account id", and returns a `Relationship` entity.
              """

                CustomEmojisSelected ->
                    """
**CustomEmojisRequest Help**

The "$GetCustomEmojis" button gets a list of `Emoji` entities.
                   """

                EndorsementsSelected ->
                    """
**EndorsementsRequest Help**

The "$GetEndorsements" button gets a list of endorsed "Account" entities.

The "$PostPinAccount" button adds "account id" to the list of endorsed accounts.

The "$PostUnpinAccount" button removes "account id" from the list of endorsed accounts.
                   """

                FavouritesSelected ->
                    """
**FavouritesRequest Help**

The "$GetFavourites" button gets a maximum of "limit" `Favourites` entities.

The "$PostFavourite" button add "status id" to your list of favourites.

The "$PostUnfavourite" button removes "status id" from your list of favourites.
                   """

                FiltersSelected ->
                    """
**FiltersRequest Help**

The "$GetFilters" button gets your list of `Filter` entities.

The "$GetFilter" button gets the `Filter` entity for "filter id".

The "$PostFilter" button creates a new filter from "phrase", "context", "irreversible", "whole word", and "expires in".

The "$PutFilter" button updates the filter parameters for "filter id".

The "$DeleteFilter" button deletes "filter id".
                    """

                FollowRequestsSelected ->
                    """
**FollowRequestsRequest Help**

The "$GetFollowRequests" button gets a maximum of "limit" `FollowRequest` entities.

The "$PostAuthorizeFollow" button authorizes the follow request from "account id".

The "$PostRejectFollow" button rejects the follow request from "account id".

                   """

                FollowSuggestionsSelected ->
                    """
**FollowSuggestionsRequest Help**

The "$GetFollowSuggestions" button requests a list of suggested Account entities.

The "$DeleteFollowSuggestions" button deletes "account id" from the suggestion list.
                   """

                GroupsSelected ->
                    """
**GroupsRequest Help**

Groups are a Gab-only feature.

Click "$GetGroups" to get a list of `Group` entities in the "Member", "Featured", or "Admin" selector.

Click "$GetGroup" to get the `Group` entity for "group id".

Click "$GetGroupAccounts" to get a list of `Account` entities for the group members.

Click "$GetGroupRemovedAccounts" to get a list of `Account` entities for accounts that have been removed from the group (via "$PostGroupRemovedAccounts").

Click "$GetGroupRelationships" to get the list of `GroupRelationship` entities for the comma-separated "group ids".

Click "$PostGroupJoin" to join "group id".

Click "$DeleteGroupJoin" to leave "group id".

Click "$PostGroupRemovedAccounts" to remove "account id" from "group id". It will be returned by "$GetGroupRemovedAccounts".

Click "$DeleteGroupRemovedAccounts" to remove "account id" from the list of removed accounts for "group id". This will allow that person to join again.

Click "$PatchGroupAddAdministrator" to make "account id" an administrator for "group id". There is currently no way to remove an administrator except by removing her from the group with "$PostGroupRemovedAccounts". (This doesn't currently work for me. Don't know why yet)

Click "$DeleteGroupStatus" to remove "status id" from "group id". The status will still exist, but will no longer be part of the group's timeline.

Click "$PostGroup" to create a new group from "title", "description", and "cover image". The cover image will be cropped to a 19x7 aspect ratio (1900x700).

Click $PutGroup to change the "title", "description", and/or "cover image" for "group id". Leave a field blank or the image unspecified to not change it.
            """

                StatusesSelected ->
                    """
**StatusesRequest Help**

Click "$GetStatus" to get the `Status` entity for "status id".

Click "$GetStatusContext" to get the `Context` entity for "status id".

Click "$GetStatusCard" to get the `Card` entity for "status id". A card is generated in the background from the first link in a status. Returns are variously `HTTP 404 status`, `{}`, or `null` when the card doesn't exist.

Click "$GetStatusRebloggedBy" to get a list of `Account` entities that reblogged "status id". "limit" controls the maximum number of results.

Click "$GetStatusFavouritedBy" to get a list of `Account` entities that favorited "status id". "limit" controls the maximum number of results.

Click "$DeleteStatus" to delete "status id", after confirmation.

Click "$PostReblogStatus" to reblog "status id".

Click "$PostUnreblogStatus" to unreblog "status id".

Click "$PostPinStatus" to pin "status id".

Click "$PostUnpinStatus" to unpin "status id".

Click "$PostStatus" to create a new status, using "status", "in reply to id", "group id", "quote of id", "sensitive", "spoiler text", "visibility", "scheduled at", "language", "idempotency key", and "media ids".

If you want to add media to a status, you can do that in the "-- new media --" section. Click "Choose File" to read a media file. Optionally fill in a "description" and ""focus x" and "y" (each between 0.0 and 1.0). Click "$PostMedia" to send it to the server. The "id" from the received `Attachment` entity will set the "media id" and be added to the comma-separated list of "media ids".

To edit the description or focus of a "media id", fill in one or both of those, and click "$PutMedia".
                """

                ListsSelected ->
                    """
**ListsSelected Help**

The "$GetLists" button gets your list of `List` entities.

The "$GetList" button gets the `List` entity for "list id".

The "$GetListAccounts" button gets the list of `Account` entities that are in "list id".

The "$GetAccountLists" button gets the list of `List` entities containing "account id".

The "$PostList" button creates a new list with the given "title".

The "$PutList" button changes the title for "list id" to "title".

The "$DeleteList" button deletes "list id".

The "$PostListAccounts" button adds the (comma-separated) "account ids" to "list id". You must follow each of them, or you'll get an error.

The "$DeleteListAccounts" button removes the "account ids" from "list id".
                    """

                MutesSelected ->
                    """
**MutesRequest Help**

The "$GetAccountMutes" button gets a list of muted accounts, limited in number by "limit".

The "$PostAccountMute" button mutes the "account id", and notifications from that account as well, if "mute notifications" is checked.

The "$PostAccountUnmute" button unmutes the "account id".

The "$PostStatusMute" button mutes the "status id".

The "$PostStatusUnmute" button unmutes the "status id".
                    """

                NotificationsSelected ->
                    """
**NotificationsRequest Help**

The "include all" and "mentions only" buttons change the "excluded notifications" checkboxes to the two most common configurations.

The "$GetNotifications" button returns a list of notifications meeting the paging requirements in "limit", "max id", "min id", and "since id", excluding the checked "excluded notifications", and, if "from account id only" is non-blank, including only notifications from that account ID.

The "$GetNotification" button returns the notification with the given "notification id". It will be disabled if the id field is empty.

The "$PostDismissNotification" button deletes the notification with the given "notification id". It will be disabled if the id field is empty. It does NOT request confirmation.

The "$PostClearNotifications" button deletes all notifications for your account, after confirmation.
                   """

                ReportsSelected ->
                    """
**ReportsRequest Help**

The "$PostReports" button sends "comment" to the instance administrator about the comma-separated list of "status ids" from "account id".
                    """

                ScheduledStatusesSelected ->
                    """
**ScheduledStatusesRequest Help**

The "$GetScheduledStatuses" button returns a list of `ScheduledStatus` entitities.

The "$GetScheduledStatus" button fetches "scheduled status id".

The "$DeleteScheduledStatus" button deletes "scheduled status id".

The "$PutScheduledStatus" button changes the "scheduled at" timestamp for "scheduled status id".
                    """

                SearchSelected ->
                    """
**SearchRequest Help**

The "$GetSearch" button searches for the query string, "q" in accounts, hashtags, and statuses, and returns a `Results` entity. "limit" is the maximum number of results. "offset" is the offset in the results. "resolve" attempts a WebFinger lookup if true. "following" includes only accounts the user is following.
                    """

                TimelinesSelected ->
                    """
**TimelinesRequest Help**

The paging parameters, "limit", "max id", "min id", and "since id" are used for all the timelines requests. The ids are `Status` ids for `GetXXXTimeline` and `Conversation` ids for `GetConversations`.

If "smart paging" is checked, does its best to be smart about changing the paging parameters after a request. If fewer entities are returned than the "limit", and "since id" & "min id" are blank, will set "max id" to the id of the last entity returned. If "min id" is non-blank, and "max id" and "since id" are blank, will set "min id" to the id of the first entity returned.

The "$GetHomeTimeline" button returns the statuses for those you follow.

The "$GetConversations" button returns a list of conversations. I don't know what a conversation is, possibly the PM feature.

The "$GetProTimeline" button returns the pro timeline (Gab only), with local posts only if "local" is checked, and with only posts containing media if "media only" is checked.

The "$GetPublicTimeline" button returns the public timeline, with local posts only if "local" is checked, and with only posts containing media if "media only" is checked.

The "$GetTagTimeline" button returns posts containing the given "hashtag", with local posts only if "local" is checked, and with only posts containing media if "media only" is checked.

The "$GetListTimeline" button returns posts for the list with the given "list id".

The "$GetGroupTimeline" button returns posts for the given "group id".
              """

                TrendsSelected ->
                    """
** TrendsRequest Help**

The "$GetTrends" button fetches a list of `Tag` entities, containing information about trending hashtags. Some servers always return an empty list for this.
                    """

                LoginSelected ->
                    """
**General and Login Help**

Click a radio button to choose the user interface for that section of the API. The names (mostly) match the variant names in the [`Mastodon.Request`](https://github.com/billstclair/elm-mastodon/blob/master/src/Mastodon/Request.elm)`.Request` type.

The "docs" links open, in a new tab, the relevant section of the documentation at docs.joinmastodon.org.

Type a server name, e.g. `mastodon.social`, in the "Server" box at the top of the screen. As soon as you finish typing the name of a real Mastodon server, it will show its `Instance` entity.

The selector to the right of the "Server" input area shows the servers to which you have successfully logged in. Tokens are saved for each, so you don't need to visit the server authentication page again to login to that account. Selecting one of the servers here changes the "Server" input box, and looks up that server's `Instance` entity, but does NOT change the "Use API for" setting. You need to click "Login" or "Set Server" to do that.

The "Login" button logs in to the displayed "Server". This will use a saved token, if there is one, or redirect to the server's authorization page, where you will need to enter your userid/email and password, or, if there are cookies for that in your browser, just click to approve access. Your `Account` entity will be fetched and displayed.

The "Set Server" button uses the "server" for API requests without logging in. Only a few API requests work without logging in, but this lets you do some exploration of a server without having an account there. The server's `Instance` entity will be fetched and displayed.

The "Logout" button logs out of the "Use API for" server. This will remove it from the server selector and clear its persistent token, requiring you to reauthenticate if you login again.

If you get an error on logging in that your token has expired, you need to "Set Server", "Logout", enter the server name back in the "server" box, and "Login".

The "show tree" checkbox controls whether the "Received" and "Decoded" sections are shown as preformatted text or as expandable trees. If trees are shown, clicking on a string, number, or boolean in the tree will copy its path and value to "selected path" and a textarea, which will appear above the "show tree" checkbox. It also copies the value to the clipboard. This makes it easy to paste values, e.g. IDs, and to view them with line-wrap.

If you hold down the "Alt" key ("Option" on Macintosh) while clicking on a tree value, the value will be copied into the selected path and to the clipboard, but the selected path textarea will not be focused or selected, nor will it be scrolled into view. You can use this when you want to paste somewhere other than a field on this page, and don't want the scroll position to change.

If the "selected path" textarea shows a URL, there will be an "open URL in new tab" link which will do that.

The "prettify" checkbox, which is only shown if "show tree" is not checked, controls whether the JSON output lines are wrapped to fit the screen. If selected, then the non-tree output will not necessarily be valid JSON. If NOT selected, then it will, and you can copy and paste it into environments that expect JSON.

The "elm button names" checkbox controls whether the buttons are labelled with HTTP methods and URLs (with the "/api/v1/" prefix elided) or with the names of the Elm type variants.

The "Headers" section, if enabled, shows the headers received from the server for the last request.

The "Received" section, if enabled, shows the JSON received from the server for the last request.

The "Decoded" section, if enabled, shows the decoded JSON received from the server for the last request. If it differs from "Received", there is either a decoder bug, one or more fields are not yet supported, or the received JSON uses defaults for one or more fields.

The "Clear" button on the same line as the "Prettify" checkbox clears the "Sent", "Received", and "Decoded" sections, making this help easier to see.

This page saves state in the JavaScript `localStorage` database. The "Clear All Persistent State" button near the bottom of the page removes all that state, after you click "Erase" on a confirmation dialog.

This page does NOT use cookies, but logging in to a Mastodon/Pleroma server will set cookies for that server, so if you want to switch users on a server, you need to "Logout" here, go to the Mastodon/Pleroma server's web page, logout there, and "Login" again here.

The "Dark Mode" checkbox toggles between light and dark mode.

If you look at the [code for this page](https://github.com/billstclair/elm-mastodon/blob/master/example/src/Main.elm), and search for `SendGetVerifyCredentials`, you'll see examples of using the `Mastodon.Request` module.
            """


convertJsonNewlines : String -> String
convertJsonNewlines json =
    String.replace "\\r" "" json
        |> String.replace "\\n" "\n"


wrapJsonLine : Int -> String -> List String
wrapJsonLine width line =
    let
        body =
            String.trimLeft line

        indentN =
            String.length line - String.length body + 2

        initialIndent =
            String.repeat (indentN - 2) " "

        indent =
            String.repeat indentN " "

        wrapped =
            convertJsonNewlines body
                |> String.split "\n"
                |> List.map (SE.softWrap <| max 20 (width - indentN))
                |> String.join "\n"

        lines =
            String.split "\n" wrapped
    in
    case lines of
        [] ->
            []

        first :: rest ->
            (initialIndent ++ first)
                :: List.map ((++) indent) rest


wrapJsonLines : Int -> String -> String
wrapJsonLines width string =
    String.split "\n" string
        |> List.concatMap (wrapJsonLine width)
        |> String.join "\n"


wrapColumns : Int
wrapColumns =
    80


encodeWrap : Bool -> Value -> String
encodeWrap prettify value =
    JE.encode 2 value
        |> (if prettify then
                wrapJsonLines wrapColumns

            else
                identity
           )



---
--- Persistence
---


type alias SavedModel =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , server : String
    , feedSetDefinition : FeedSetDefinition
    , supportsAccountByUsername : Dict String Bool
    , postState : PostState
    , features : Features
    , scrollPillState : ScrollPillState
    , showLeftColumn : Bool
    , prettify : Bool
    , selectedRequest : SelectedRequest
    , username : String
    , accountId : String
    , accountIds : String
    , showMetadata : Bool
    , q : String
    , resolve : Bool
    , following : Bool
    , groupId : String
    , showReceived : Bool
    , showEntity : Bool
    , whichGroups : WhichGroups
    , followReblogs : Bool
    , onlyMedia : Bool
    , pinned : Bool
    , excludeReplies : Bool
    , excludeReblogs : Bool
    , pagingInput : PagingInput
    , local : Bool
    , hashtag : String
    , listId : String
    , smartPaging : Bool
    , showJsonTree : Bool
    , showUpdateCredentials : Bool
    , statusId : String
    , useElmButtonNames : Bool
    , showPostStatus : Bool
    , excludedNotificationTypes : List NotificationType
    , notificationsAccountId : String
    , notificationId : String
    , muteNotifications : Bool
    , groupIds : String
    , offset : String
    , listTitle : String
    , filterId : String
    , filterInput : FilterInput
    , scheduledStatusId : String
    , userNameInput : String
    , accountInput : Maybe Account
    , groupNameInput : String
    , groupInput : Maybe Group
    , hashtagInput : String
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { renderEnv = model.renderEnv
    , page = model.page
    , token = model.token
    , server = model.server
    , feedSetDefinition = model.feedSetDefinition
    , supportsAccountByUsername = model.supportsAccountByUsername
    , postState = model.postState
    , features = model.features
    , scrollPillState = model.scrollPillState
    , showLeftColumn = model.showLeftColumn
    , prettify = model.prettify
    , selectedRequest = model.selectedRequest
    , username = model.username
    , accountId = model.accountId
    , accountIds = model.accountIds
    , showMetadata = model.showMetadata
    , q = model.q
    , resolve = model.resolve
    , following = model.following
    , groupId = model.groupId
    , showReceived = model.showReceived
    , showEntity = model.showEntity
    , whichGroups = model.whichGroups
    , followReblogs = model.followReblogs
    , onlyMedia = model.onlyMedia
    , pinned = model.pinned
    , excludeReplies = model.excludeReplies
    , excludeReblogs = model.excludeReblogs
    , pagingInput = model.pagingInput
    , local = model.local
    , hashtag = model.hashtag
    , listId = model.listId
    , smartPaging = model.smartPaging
    , showJsonTree = model.showJsonTree
    , showUpdateCredentials = model.showUpdateCredentials
    , statusId = model.statusId
    , useElmButtonNames = model.useElmButtonNames
    , showPostStatus = model.showPostStatus
    , excludedNotificationTypes = model.excludedNotificationTypes
    , notificationsAccountId = model.notificationsAccountId
    , notificationId = model.notificationId
    , muteNotifications = model.muteNotifications
    , groupIds = model.groupIds
    , offset = model.offset
    , listTitle = model.listTitle
    , filterId = model.filterId
    , filterInput = model.filterInput
    , scheduledStatusId = model.scheduledStatusId
    , userNameInput = model.userNameInput
    , accountInput = model.accountInput
    , groupNameInput = model.groupNameInput
    , groupInput = model.groupInput
    , hashtagInput = model.hashtagInput
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    let
        renderEnv =
            model.renderEnv

        savedRenderEnv =
            savedModel.renderEnv
    in
    { model
        | renderEnv =
            { renderEnv
                | loginServer = savedRenderEnv.loginServer
                , style = savedRenderEnv.style
                , fontSize = savedRenderEnv.fontSize
                , columnWidth = savedRenderEnv.columnWidth
            }
        , page = savedModel.page
        , token = savedModel.token
        , server = savedModel.server
        , feedSetDefinition = savedModel.feedSetDefinition
        , supportsAccountByUsername = savedModel.supportsAccountByUsername
        , postState = savedModel.postState
        , features = savedModel.features
        , scrollPillState = savedModel.scrollPillState
        , showLeftColumn = savedModel.showLeftColumn
        , prettify = savedModel.prettify
        , selectedRequest = savedModel.selectedRequest
        , username = savedModel.username
        , accountId = savedModel.accountId
        , accountIds = savedModel.accountIds
        , showMetadata = savedModel.showMetadata
        , q = savedModel.q
        , resolve = savedModel.resolve
        , following = savedModel.following
        , groupId = savedModel.groupId
        , showReceived = savedModel.showReceived
        , showEntity = savedModel.showEntity
        , whichGroups = savedModel.whichGroups
        , followReblogs = savedModel.followReblogs
        , onlyMedia = savedModel.onlyMedia
        , pinned = savedModel.pinned
        , excludeReplies = savedModel.excludeReplies
        , excludeReblogs = savedModel.excludeReblogs
        , pagingInput = savedModel.pagingInput
        , local = savedModel.local
        , hashtag = savedModel.hashtag
        , listId = savedModel.listId
        , smartPaging = savedModel.smartPaging
        , showJsonTree = savedModel.showJsonTree
        , showUpdateCredentials = savedModel.showUpdateCredentials
        , statusId = savedModel.statusId
        , useElmButtonNames = savedModel.useElmButtonNames
        , showPostStatus = savedModel.showPostStatus
        , excludedNotificationTypes = savedModel.excludedNotificationTypes
        , notificationsAccountId = savedModel.notificationsAccountId
        , notificationId = savedModel.notificationId
        , muteNotifications = model.muteNotifications
        , groupIds = savedModel.groupIds
        , offset = savedModel.offset
        , listTitle = savedModel.listTitle
        , filterId = savedModel.filterId
        , filterInput = savedModel.filterInput
        , scheduledStatusId = savedModel.scheduledStatusId
        , userNameInput = savedModel.userNameInput
        , accountInput = savedModel.accountInput
        , groupNameInput = savedModel.groupNameInput
        , groupInput = savedModel.groupInput
        , hashtagInput = savedModel.hashtagInput
    }


encodeWhichGroups : WhichGroups -> Value
encodeWhichGroups whichGroups =
    JE.string <|
        case whichGroups of
            Request.MemberGroups ->
                "MemberGroups"

            Request.FeaturedGroups ->
                "FeaturedGroups"

            Request.AdminGroups ->
                "AdminGroups"


whichGroupsDecoder : Decoder WhichGroups
whichGroupsDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "MemberGroups" ->
                        JD.succeed Request.MemberGroups

                    "FeaturedGroups" ->
                        JD.succeed Request.FeaturedGroups

                    "AdminGroups" ->
                        JD.succeed Request.AdminGroups

                    _ ->
                        JD.fail <| "Unknown WhichGroups value: " ++ s
            )


{-| Encode `Paging` into `Value`
-}
encodePagingInput : PagingInput -> Value
encodePagingInput { max_id, since_id, min_id, limit } =
    JE.object
        [ ( "max_id", JE.string max_id )
        , ( "since_id", JE.string since_id )
        , ( "min_id", JE.string min_id )
        , ( "limit", JE.string limit )
        ]


{-| Decode `PagingInput`
-}
pagingInputDecoder : Decoder PagingInput
pagingInputDecoder =
    JD.succeed PagingInput
        |> required "max_id" JD.string
        |> required "since_id" JD.string
        |> required "min_id" JD.string
        |> required "limit" JD.string


encodePage : Page -> Value
encodePage page =
    JE.string <|
        case page of
            HomePage ->
                "HomePage"

            ColumnsPage ->
                "ColumnsPage"

            ExplorerPage ->
                "ExplorerPage"


stringToPage : String -> Page
stringToPage string =
    case string of
        "ColumnsPage" ->
            ColumnsPage

        "ExplorerPage" ->
            ExplorerPage

        _ ->
            HomePage


pageDecoder : Decoder Page
pageDecoder =
    JD.string
        |> JD.andThen
            (\p ->
                JD.succeed <| stringToPage p
            )


encodeStyle : Style -> Value
encodeStyle style =
    case style of
        DarkStyle ->
            JE.string "DarkStyle"

        LightStyle ->
            JE.string "LightStyle"


styleDecoder : Decoder Style
styleDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "DarkStyle" ->
                        JD.succeed DarkStyle

                    "LightStyle" ->
                        JD.succeed LightStyle

                    _ ->
                        JD.fail <| "Unknown Style: " ++ s
            )


encodeRenderEnv : RenderEnv -> Value
encodeRenderEnv env =
    JE.object
        [ ( "loginServer", ED.encodeMaybe JE.string env.loginServer )
        , ( "style", encodeStyle env.style )
        , ( "fontSizePct", JE.int env.fontSizePct )
        , ( "fontSize", JE.string env.fontSize )
        , ( "columnWidth", JE.int env.columnWidth )
        ]


renderEnvDecoder : Decoder RenderEnv
renderEnvDecoder =
    JD.succeed
        (\loginServer style fontSizePct_ fontSize_ columnWidth ->
            let
                ( fontSizePct, fontSize ) =
                    if fontSizePct_ == 0 then
                        case String.toInt fontSize_ of
                            Just int ->
                                ( int, fontSize_ )

                            Nothing ->
                                ( 100, "100" )

                    else
                        ( fontSizePct_, String.fromInt fontSizePct_ )
            in
            { emptyRenderEnv
                | loginServer = loginServer
                , style = style
                , fontSizePct = fontSizePct
                , fontSize = fontSize
                , columnWidth = columnWidth
            }
        )
        |> required "loginServer" (JD.nullable JD.string)
        |> required "style" styleDecoder
        |> optional "fontSizePct" JD.int 0
        |> optional "fontSize" JD.string "100"
        |> optional "columnWidth" JD.int 300


encodeFeatures : Features -> Value
encodeFeatures features =
    JE.dict identity (JE.dict identity JE.bool) features


featuresDecoder : Decoder Features
featuresDecoder =
    JD.dict <| JD.dict JD.bool


encodeReplyType : ReplyType -> Value
encodeReplyType replyType =
    case replyType of
        ReplyToPost ->
            JE.string "ReplyToPost"

        QuotePost ->
            JE.string "QuotePost"

        NoReply ->
            JE.string "NoReply"


replyTypeDecoder : Decoder ReplyType
replyTypeDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                if s == "ReplyToPost" then
                    JD.succeed ReplyToPost

                else if s == "QuotePost" then
                    JD.succeed QuotePost

                else
                    JD.succeed NoReply
            )


encodePostState : PostState -> Value
encodePostState postState =
    JE.object
        [ ( "replyTo", ED.encodeMaybe ED.encodeStatus postState.replyTo )
        , ( "replyType", encodeReplyType postState.replyType )
        , ( "text", JE.string postState.text )
        , ( "mentionsString", JE.string postState.mentionsString )
        , ( "sensitive", JE.bool postState.sensitive )
        , ( "media_ids", JE.list JE.string postState.media_ids )
        , ( "fileNames", JE.list JE.string postState.fileNames )
        , ( "fileUrls", JE.list JE.string postState.fileUrls )
        , ( "groupName", JE.string postState.groupName )
        , ( "group_id", ED.encodeMaybe JE.string postState.group_id )
        ]


postStateDecoder : Decoder PostState
postStateDecoder =
    JD.succeed PostState
        |> required "replyTo" (JD.nullable ED.statusDecoder)
        |> optional "replyType" replyTypeDecoder NoReply
        |> required "text" JD.string
        |> required "mentionsString" JD.string
        |> required "sensitive" JD.bool
        |> required "media_ids" (JD.list JD.string)
        |> required "fileNames" (JD.list JD.string)
        |> required "fileUrls" (JD.list JD.string)
        |> optional "groupName" JD.string ""
        |> optional "group_id" (JD.nullable JD.string) Nothing
        |> JD.andThen
            (fixDecodedPostState >> JD.succeed)


fixDecodedPostState : PostState -> PostState
fixDecodedPostState postState =
    let
        { media_ids, fileNames, fileUrls } =
            postState

        len =
            min (List.length media_ids) <|
                min (List.length fileNames) (List.length fileUrls)
    in
    { postState
        | media_ids = List.take len media_ids
        , fileNames = List.take len fileNames
        , fileUrls = List.take len fileUrls
    }


encodeScrollPillState : ScrollPillState -> Value
encodeScrollPillState scrollPillState =
    JE.object
        [ ( "showScrollPill", JE.bool scrollPillState.showScrollPill )
        , ( "showServer", JE.bool scrollPillState.showServer )
        ]


scrollPillStateDecoder : Decoder ScrollPillState
scrollPillStateDecoder =
    JD.succeed ScrollPillState
        |> optional "showScrollPill" JD.bool True
        |> required "showServer" JD.bool


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object <|
        List.concat
            [ encodePropertyAsList "renderEnv"
                savedModel.renderEnv
                encodeRenderEnv
                emptyRenderEnv
            , [ ( "page", encodePage savedModel.page ) ]
            , encodePropertyAsList "token"
                savedModel.token
                (ED.encodeMaybe JE.string)
                Nothing
            , [ ( "server", JE.string savedModel.server ) ]
            , encodePropertyAsList "feedSetDefinition"
                savedModel.feedSetDefinition
                MED.encodeFeedSetDefinition
                Types.emptyFeedSetDefinition
            , encodePropertyAsList "supportsAccountByUsername"
                savedModel.supportsAccountByUsername
                (JE.dict identity JE.bool)
                Dict.empty
            , encodePropertyAsList "postState"
                savedModel.postState
                encodePostState
                initialPostState
            , encodePropertyAsList "features"
                savedModel.features
                encodeFeatures
                Dict.empty
            , encodePropertyAsList "scrollPillState"
                savedModel.scrollPillState
                encodeScrollPillState
                initialScrollPillState
            , encodePropertyAsList "showLeftColumn"
                savedModel.showLeftColumn
                JE.bool
                True
            , encodePropertyAsList "accountId"
                savedModel.accountId
                JE.string
                ""
            , encodePropertyAsList "accountIds"
                savedModel.accountIds
                JE.string
                ""
            , encodePropertyAsList "showMetadata"
                savedModel.showMetadata
                JE.bool
                False
            , encodePropertyAsList "q"
                savedModel.q
                JE.string
                ""
            , encodePropertyAsList "resolve"
                savedModel.resolve
                JE.bool
                False
            , encodePropertyAsList "following"
                savedModel.following
                JE.bool
                False
            , encodePropertyAsList "groupId"
                savedModel.groupId
                JE.string
                ""
            , encodePropertyAsList "showReceived"
                savedModel.showReceived
                JE.bool
                True
            , encodePropertyAsList "showEntity"
                savedModel.showEntity
                JE.bool
                False
            , encodePropertyAsList "whichGroups"
                savedModel.whichGroups
                encodeWhichGroups
                Request.MemberGroups
            , encodePropertyAsList "followReblogs"
                savedModel.followReblogs
                JE.bool
                True
            , encodePropertyAsList "onlyMedia"
                savedModel.onlyMedia
                JE.bool
                False
            , encodePropertyAsList "pinned"
                savedModel.pinned
                JE.bool
                False
            , encodePropertyAsList "excludeReplies"
                savedModel.excludeReplies
                JE.bool
                False
            , encodePropertyAsList "excludeReblogs"
                savedModel.excludeReblogs
                JE.bool
                False
            , encodePropertyAsList "pagingInput"
                savedModel.pagingInput
                encodePagingInput
                emptyPagingInput
            , encodePropertyAsList "local"
                savedModel.local
                JE.bool
                False
            , encodePropertyAsList "hashtag"
                savedModel.hashtag
                JE.string
                ""
            , encodePropertyAsList "listId"
                savedModel.listId
                JE.string
                ""
            , encodePropertyAsList "smartPaging"
                savedModel.smartPaging
                JE.bool
                False
            , encodePropertyAsList "showJsonTree"
                savedModel.showJsonTree
                JE.bool
                True
            , encodePropertyAsList "showUpdateCredentials"
                savedModel.showUpdateCredentials
                JE.bool
                False
            , encodePropertyAsList "statusId"
                savedModel.statusId
                JE.string
                ""
            , encodePropertyAsList "useElmButtonNames"
                savedModel.useElmButtonNames
                JE.bool
                False
            , encodePropertyAsList "showPostStatus"
                savedModel.showPostStatus
                JE.bool
                False
            , encodePropertyAsList "excludedNotificationTypes"
                savedModel.excludedNotificationTypes
                (JE.list ED.encodeNotificationType)
                []
            , encodePropertyAsList "notificationsAccountId"
                savedModel.notificationsAccountId
                JE.string
                ""
            , encodePropertyAsList "notificationId"
                savedModel.notificationId
                JE.string
                ""
            , encodePropertyAsList "muteNotifications"
                savedModel.muteNotifications
                JE.bool
                True
            , encodePropertyAsList "groupIds"
                savedModel.groupIds
                JE.string
                ""
            , encodePropertyAsList "offset"
                savedModel.offset
                JE.string
                ""
            , encodePropertyAsList "listTitle"
                savedModel.listTitle
                JE.string
                ""
            , encodePropertyAsList "filterId"
                savedModel.filterId
                JE.string
                ""
            , encodePropertyAsList "filterInput"
                savedModel.filterInput
                encodeFilterInput
                emptyFilterInput
            , encodePropertyAsList "scheduledStatusId"
                savedModel.scheduledStatusId
                JE.string
                ""
            , encodePropertyAsList "userNameInput"
                savedModel.userNameInput
                JE.string
                ""
            , encodePropertyAsList "accountInput"
                savedModel.accountInput
                (ED.encodeMaybe ED.encodeAccount)
                Nothing
            , encodePropertyAsList "groupNameInput"
                savedModel.groupNameInput
                JE.string
                ""
            , encodePropertyAsList "groupInput"
                savedModel.groupInput
                (ED.encodeMaybe ED.encodeGroup)
                Nothing
            , encodePropertyAsList "hashtagInput"
                savedModel.hashtagInput
                JE.string
                ""
            ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "renderEnv" renderEnvDecoder emptyRenderEnv
        |> required "page" pageDecoder
        |> optional "token" (JD.nullable JD.string) Nothing
        |> required "server" JD.string
        |> optional "feedSetDefinition"
            MED.feedSetDefinitionDecoder
            Types.defaultFeedSetDefinition
        |> optional "supportsAccountByUsername" (JD.dict JD.bool) Dict.empty
        |> optional "postState" postStateDecoder initialPostState
        |> optional "features" featuresDecoder Dict.empty
        |> optional "scrollPillState" scrollPillStateDecoder initialScrollPillState
        |> optional "showLeftColumn" JD.bool True
        |> optional "prettify" JD.bool True
        |> optional "selectedRequest" selectedRequestDecoder LoginSelected
        |> optional "username" JD.string ""
        |> optional "accountId" JD.string ""
        |> optional "accountIds" JD.string ""
        |> optional "showMetadata" JD.bool False
        |> optional "q" JD.string ""
        |> optional "resolve" JD.bool False
        |> optional "following" JD.bool False
        |> optional "groupId" JD.string ""
        |> optional "showReceived" JD.bool True
        |> optional "showEntity" JD.bool False
        |> optional "whichGroups" whichGroupsDecoder Request.MemberGroups
        |> optional "followReblogs" JD.bool True
        |> optional "onlyMedia" JD.bool False
        |> optional "pinned" JD.bool False
        |> optional "excludeReplies" JD.bool False
        |> optional "excludeReblogs" JD.bool False
        |> optional "pagingInput" pagingInputDecoder emptyPagingInput
        |> optional "local" JD.bool False
        |> optional "hashtag" JD.string ""
        |> optional "listId" JD.string ""
        |> optional "smartPaging" JD.bool False
        |> optional "showJsonTree" JD.bool True
        |> optional "showUpdateCredentials" JD.bool False
        |> optional "statusId" JD.string ""
        |> optional "useElmButtonNames" JD.bool False
        |> optional "showPostStatus" JD.bool False
        |> optional "excludedNotificationTypes" (JD.list ED.notificationTypeDecoder) []
        |> optional "notificationsAccountId" JD.string ""
        |> optional "notificationId" JD.string ""
        |> optional "muteNotifications" JD.bool True
        |> optional "groupIds" JD.string ""
        |> optional "offset" JD.string ""
        |> optional "listTitle" JD.string ""
        |> optional "filterId" JD.string ""
        |> optional "filterInput" filterInputDecoder emptyFilterInput
        |> optional "scheduledStatusId" JD.string ""
        |> optional "userNameInput" JD.string ""
        |> optional "accountInput" (JD.nullable ED.accountDecoder) Nothing
        |> optional "groupNameInput" JD.string ""
        |> optional "groupInput" (JD.nullable ED.groupDecoder) Nothing
        |> optional "hashtagInput" JD.string ""


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


tokenStorageKey : String -> String
tokenStorageKey server =
    pk.token ++ "." ++ server


tokenStorageKeyServer : String -> String
tokenStorageKeyServer key =
    String.dropLeft (String.length pk.token + 1) key


getToken : String -> Cmd Msg
getToken server =
    getLabeled pk.token <| tokenStorageKey server


putToken : String -> Maybe String -> Cmd Msg
putToken server token =
    put (tokenStorageKey server) <|
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| JE.string tok


getFeedSetDefinition : String -> Cmd Msg
getFeedSetDefinition server =
    get <| pk.feedSetDefinition ++ "." ++ server


maybePutFeedSetDefinition : Model -> FeedSetDefinition -> Cmd Msg
maybePutFeedSetDefinition model feedSetDefinition =
    case model.renderEnv.loginServer of
        Just server ->
            putFeedSetDefinition server feedSetDefinition

        _ ->
            Cmd.none


putFeedSetDefinition : String -> FeedSetDefinition -> Cmd Msg
putFeedSetDefinition server feedSetDefinition =
    put (pk.feedSetDefinition ++ "." ++ server)
        (Just <| MED.encodeFeedSetDefinition feedSetDefinition)


getAccountIds : String -> Cmd Msg
getAccountIds server =
    get <| pk.accountIds ++ "." ++ server


putAccountIds : String -> List AccountId -> Cmd Msg
putAccountIds server accountIds =
    put (pk.accountIds ++ "." ++ server) (Just <| MED.encodeAccountIds accountIds)


clear : Cmd Msg
clear =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "mammudeck"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort (GlobalMsg << Process) moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , token = "token"
    , feedSetDefinition = "feedSetDefinition"
    , accountIds = "accountIds"
    }



---
--- Special characters
---


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    , biohazard = stringFromCode 9763 -- \u2623
    , black_star = stringFromCode 10036 -- \u2734
    , hourglass = stringFromCode 8987 -- \u231B
    , hourglass_flowing = stringFromCode 9203 -- \u23F3
    }


feedLoadingEmoji : String
feedLoadingEmoji =
    special.hourglass_flowing



---
--- Button names
---


dollarButtonNameDict : Dict String ExplorerSendMsg
dollarButtonNameDict =
    Dict.fromList
        [ ( "GetGroups", SendGetGroups )
        , ( "GetGroup", SendGetGroup )
        , ( "GetGroupAccounts", SendGetGroupAccounts )
        , ( "GetGroupRemovedAccounts", SendGetGroupRemovedAccounts )
        , ( "GetGroupRelationships", SendGetGroupRelationships )
        , ( "PostGroupJoin", SendPostGroupJoin )
        , ( "DeleteGroupJoin", SendDeleteGroupJoin )
        , ( "PostGroupRemovedAccounts", SendPostGroupRemovedAccounts )
        , ( "DeleteGroupRemovedAccounts", SendDeleteGroupRemovedAccounts )
        , ( "PatchGroupAddAdministrator", SendPatchGroupAddAdministrator )
        , ( "DeleteGroupStatus", SendDeleteGroupStatus )
        , ( "PostGroup", SendPostGroup )
        , ( "PutGroup", SendPutGroup )
        , ( "GetLists", SendGetLists )
        , ( "GetList", SendGetList )
        , ( "GetListAccounts", SendGetListAccounts )
        , ( "GetAccountLists", SendGetAccountLists )
        , ( "PostList", SendPostList )
        , ( "PutList", SendPutList )
        , ( "DeleteList", SendDeleteList )
        , ( "PostListAccounts", SendPostListAccounts )
        , ( "DeleteListAccounts", SendDeleteListAccounts )
        , ( "GetBlocks", SendGetBlocks )
        , ( "PostBlock", SendPostBlock )
        , ( "PostUnblock", SendPostUnblock )
        , ( "GetCustomEmojis", SendGetCustomEmojis )
        , ( "GetEndorsements", SendGetEndorsements )
        , ( "PostPinAccount", SendPostPinAccount )
        , ( "PostUnpinAccount", SendPostUnpinAccount )
        , ( "GetFavourites", SendGetFavourites )
        , ( "PostFavourite", SendPostFavourite )
        , ( "PostUnfavourite", SendPostUnfavourite )
        , ( "GetFilters", SendGetFilters )
        , ( "GetFilter", SendGetFilter )
        , ( "PostFilter", SendPostFilter )
        , ( "PutFilter", SendPutFilter )
        , ( "DeleteFilter", SendDeleteFilter )
        , ( "GetFollowRequests", SendGetFollowRequests )
        , ( "PostAuthorizeFollow", SendPostAuthorizeFollow )
        , ( "PostRejectFollow", SendPostRejectFollow )
        , ( "GetFollowSuggestions", SendGetFollowSuggestions )
        , ( "DeleteFollowSuggestions", SendDeleteFollowSuggestions )
        , ( "GetAccountMutes", SendGetAccountMutes )
        , ( "PostAccountMute", SendPostAccountMute )
        , ( "PostAccountUnmute", SendPostAccountUnmute )
        , ( "PostStatusMute", SendPostStatusMute )
        , ( "PostStatusUnmute", SendPostStatusUnmute )
        , ( "GetNotifications", SendGetNotifications )
        , ( "GetNotification", SendGetNotification )
        , ( "PostDismissNotification", SendPostDismissNotification )
        , ( "PostClearNotifications", SendPostClearNotifications )
        , ( "GetSearch", SendGetSearch )
        , ( "PostReports", SendPostReports )
        , ( "GetStatus", SendGetStatus )
        , ( "GetStatusContext", SendGetStatusContext )
        , ( "GetStatusCard", SendGetStatusCard )
        , ( "GetStatusRebloggedBy", SendGetStatusRebloggedBy )
        , ( "GetStatusFavouritedBy", SendGetStatusFavouritedBy )
        , ( "DeleteStatus", SendDeleteStatus )
        , ( "PostReblogStatus", SendPostReblogStatus )
        , ( "PostUnreblogStatus", SendPostUnreblogStatus )
        , ( "PostPinStatus", SendPostPinStatus )
        , ( "PostUnpinStatus", SendPostUnpinStatus )
        , ( "PostStatus", SendPostStatus )
        , ( "PostMedia", SendPostMedia )
        , ( "PutMedia", SendPutMedia )
        , ( "PostStatus", SendPostStatus )
        , ( "GetConversations", SendGetConversations )
        , ( "GetProTimeline", SendGetProTimeline )
        , ( "GetPublicTimeline", SendGetPublicTimeline )
        , ( "GetTagTimeline", SendGetTagTimeline )
        , ( "GetListTimeline", SendGetListTimeline )
        , ( "GetGroupTimeline", SendGetGroupTimeline )
        , ( "GetInstance", SendGetInstance )
        , ( "GetActivity", SendGetActivity )
        , ( "GetPeers", SendGetPeers )
        , ( "GetTrends", SendGetTrends )
        , ( "GetVerifyCredentials", SendGetVerifyCredentials )
        , ( "GetAccountByUsername", SendGetAccountByUsername )
        , ( "GetAccount", SendGetAccount )
        , ( "GetFollowers", SendGetFollowers )
        , ( "GetFollowing", SendGetFollowing )
        , ( "GetStatuses", SendGetStatuses )
        , ( "GetRelationships", SendGetRelationships )
        , ( "GetScheduledStatuses", SendGetScheduledStatuses )
        , ( "GetScheduledStatus", SendGetScheduledStatus )
        , ( "PutScheduledStatus", SendPutScheduledStatus )
        , ( "DeleteScheduledStatus", SendDeleteScheduledStatus )
        , ( "GetSearchAccounts", SendGetSearchAccounts )
        , ( "PostFollow", SendPostFollow )
        , ( "PostUnfollow", SendPostUnfollow )
        , ( "PatchUpdateCredentials", SendPatchUpdateCredentials )
        ]


dollarButtonNameToMsg : String -> ExplorerSendMsg
dollarButtonNameToMsg dollarButtonName =
    Dict.get (String.dropLeft 1 dollarButtonName) dollarButtonNameDict
        |> Maybe.withDefault SendNothing


buttonNameAlist : List ( ExplorerSendMsg, ( String, String ) )
buttonNameAlist =
    [ ( SendGetGroups, ( "GetGroups", "GET groups" ) )
    , ( SendGetGroup, ( "GetGroup", "GET groups/:id" ) )
    , ( SendGetGroupAccounts, ( "GetGroupAccounts", "GET groups/:id/accounts" ) )
    , ( SendGetGroupRemovedAccounts, ( "GetGroupRemovedAccounts", "GET groups/:id/removed_accounts" ) )
    , ( SendGetGroupRelationships, ( "GetGroupRelationships", "GET groups/:id/relationships" ) )
    , ( SendPostGroupJoin, ( "PostGroupJoin", "POST groups/:id/accounts" ) )
    , ( SendDeleteGroupJoin, ( "DeleteGroupJoin", "DELETE groups/:id/accounts" ) )
    , ( SendPostGroupRemovedAccounts, ( "PostGroupRemovedAccounts", "POST groups/:id/removed_accounts" ) )
    , ( SendDeleteGroupRemovedAccounts, ( "DeleteGroupRemovedAccounts", "DELETE groups/:id/removed_accounts" ) )
    , ( SendPatchGroupAddAdministrator, ( "PatchGroupAddAdministrator", "PATCH groups/:id/accounts" ) )
    , ( SendDeleteGroupStatus, ( "DeleteGroupStatus", "DELETE groups/:id/statuses/:status_id" ) )
    , ( SendPostGroup, ( "PostGroup", "POST groups" ) )
    , ( SendPutGroup, ( "PutGroup", "PUT groups" ) )
    , ( SendGetLists, ( "GetLists", "GET lists" ) )
    , ( SendGetList, ( "GetList", "GET lists/:id" ) )
    , ( SendGetListAccounts, ( "GetListAccounts", "GET lists/:id/accounts" ) )
    , ( SendGetAccountLists, ( "GetAccountLists", "GET accounts/:id/lists" ) )
    , ( SendPostList, ( "PostList", "POST lists" ) )
    , ( SendPutList, ( "PutList", "PUT lists/:id" ) )
    , ( SendDeleteList, ( "DeleteList", "DELETE lists/:id" ) )
    , ( SendPostListAccounts, ( "PostListAccounts", "POST lists/:id/accounts" ) )
    , ( SendDeleteListAccounts, ( "DeleteListAccounts", "DELETE lists/:id/accounts" ) )
    , ( SendGetBlocks, ( "GetBlocks", "GET blocks" ) )
    , ( SendPostBlock, ( "PostBlock", "POST accounts/:id/block" ) )
    , ( SendPostUnblock, ( "PostUnblock", "POST accounts/:id/unblock" ) )
    , ( SendGetCustomEmojis, ( "GetCustomEmojis", "GET custom_emojis" ) )
    , ( SendGetEndorsements, ( "GetEndorsements", "GET endorsements" ) )
    , ( SendPostPinAccount, ( "PostPinAccount", "POST accounts/:id/pin" ) )
    , ( SendPostUnpinAccount, ( "PostUnpinAccount", "POST accounts/:id/unpin" ) )
    , ( SendGetFavourites, ( "GetFavourites", "GET favourites" ) )
    , ( SendPostFavourite, ( "PostFavourite", "POST statuses/:id/favourite" ) )
    , ( SendPostUnfavourite, ( "PostUnfavourite", "POST statuses/:id/unfavourite" ) )
    , ( SendGetFilters, ( "GetFilters", "GET filters" ) )
    , ( SendGetFilter, ( "GetFilter", "GET filters/:id" ) )
    , ( SendPostFilter, ( "PostFilter", "POST filters" ) )
    , ( SendPutFilter, ( "PutFilter", "PUT filters/:id" ) )
    , ( SendDeleteFilter, ( "DeleteFilter", "DELETE filters/:id" ) )
    , ( SendGetFollowRequests, ( "GetFollowRequests", "GET follow_requests" ) )
    , ( SendPostAuthorizeFollow, ( "PostAuthorizeFollow", "POST follow_requests/:id/authorize" ) )
    , ( SendPostRejectFollow, ( "PostRejectFollow", "POST follow_requests/:id/reject" ) )
    , ( SendGetFollowSuggestions, ( "GetFollowSuggestions", "GET suggestions" ) )
    , ( SendDeleteFollowSuggestions, ( "DeleteFollowSuggestions", "DELETE suggestions/:account_id" ) )
    , ( SendGetAccountMutes, ( "GetAccountMutes", "GET mutes" ) )
    , ( SendPostAccountMute, ( "PostAccountMute", "POST accounts/:id/mute" ) )
    , ( SendPostAccountUnmute, ( "PostAccountUnmute", "POST accounts/:id/unmute" ) )
    , ( SendPostStatusMute, ( "PostStatusMute", "POST statuses/:id/mute" ) )
    , ( SendPostStatusUnmute, ( "PostStatusUnmute", "POST statuses/:id/unmute" ) )
    , ( SendGetNotifications, ( "GetNotifications", "GET notifications" ) )
    , ( SendGetNotification, ( "GetNotification", "GET notifications/:id" ) )
    , ( SendPostDismissNotification, ( "PostDismissNotification", "POST notifications/dismiss" ) )
    , ( SendPostClearNotifications, ( "PostClearNotifications", "POST notifications/clear" ) )
    , ( SendGetSearch, ( "GetSearch", "GET search" ) )
    , ( SendPostReports, ( "PostReports", "POST reports" ) )
    , ( SendGetStatus, ( "GetStatus", "GET statuses/:id" ) )
    , ( SendGetStatusContext, ( "GetStatusContext", "GET statuses/:id/context" ) )
    , ( SendGetStatusCard, ( "GetStatusCard", "GET statuses/:id/card" ) )
    , ( SendGetStatusRebloggedBy, ( "GetStatusRebloggedBy", "GET statuses/:id/reblogged_by" ) )
    , ( SendGetStatusFavouritedBy, ( "GetStatusFavouritedBy", "GET statuses/:id/favourited_by" ) )
    , ( SendDeleteStatus, ( "DeleteStatus", "DELETE statuses/:id" ) )
    , ( SendPostReblogStatus, ( "PostReblogStatus", "POST statuses/:id/reblog" ) )
    , ( SendPostUnreblogStatus, ( "PostUnreblogStatus", "POST statuses/:id/unreblog" ) )
    , ( SendPostPinStatus, ( "PostPinStatus", "POST statuses/:id/pin" ) )
    , ( SendPostUnpinStatus, ( "PostUnpinStatus", "POST statuses/:id/unpin" ) )
    , ( SendPostStatus, ( "PostStatus", "POST statuses" ) )
    , ( SendPostMedia, ( "PostMedia", "POST media" ) )
    , ( SendPutMedia, ( "PutMedia", "PUT media" ) )
    , ( SendGetHomeTimeline, ( "GetHomeTimeline", "GET timelines/home" ) )
    , ( SendGetConversations, ( "GetConversations", "GET conversations" ) )
    , ( SendGetProTimeline, ( "GetProTimeline", "GET timelines/pro" ) )
    , ( SendGetPublicTimeline, ( "GetPublicTimeline", "GET timelines/public" ) )
    , ( SendGetTagTimeline, ( "GetTagTimeline", "GET timelines/tag/:tag" ) )
    , ( SendGetListTimeline, ( "GetListTimeline", "GET timeslines/list/:id" ) )
    , ( SendGetGroupTimeline, ( "GetGroupTimeline", "GET timelines/group/:id" ) )
    , ( SendGetInstance, ( "GetInstance", "GET instance" ) )
    , ( SendGetActivity, ( "GetActivity", "GET instance/activity" ) )
    , ( SendGetPeers, ( "GetPeers", "GET instance/peers" ) )
    , ( SendGetTrends, ( "GetTrends", "GET trends" ) )
    , ( SendGetVerifyCredentials, ( "GetVerifyCredentials", "GET accounts/verify_credentials" ) )
    , ( SendGetAccountByUsername, ( "GetAccountByUsername", "GET account_by_username/:username" ) )
    , ( SendGetAccount, ( "GetAccount", "GET accounts/:id" ) )
    , ( SendGetFollowers, ( "GetFollowers", "GET accounts/:id/followers" ) )
    , ( SendGetFollowing, ( "GetFollowing", "GET accounts/:id/following" ) )
    , ( SendGetStatuses, ( "GetStatuses", "GET accounts/:id/statuses" ) )
    , ( SendGetRelationships, ( "GetRelationships", "GET accounts/relationships" ) )
    , ( SendGetScheduledStatuses, ( "GetScheduledStatuses", "GET scheduled_statuses" ) )
    , ( SendGetScheduledStatus, ( "GetScheduledStatus", "GET scheduled_statuses/:id" ) )
    , ( SendPutScheduledStatus, ( "PutScheduledStatus", "PUT scheduled_statuses/:id" ) )
    , ( SendDeleteScheduledStatus, ( "DeleteScheduledStatus", "DELETE scheduled_statuses/:id" ) )
    , ( SendGetSearchAccounts, ( "GetSearchAccounts", "GET accounts/search" ) )
    , ( SendPostFollow, ( "PostFollow", "POST accounts/:id/follow" ) )
    , ( SendPostUnfollow, ( "PostUnfollow", "POST accounts/:id/unfollow" ) )
    , ( SendPatchUpdateCredentials, ( "PatchUpdateCredentials", "PATCH accounts/update_credentials" ) )
    ]


tupleToButtonName : Bool -> ( String, String ) -> String
tupleToButtonName useElmButtonNames ( elm, http ) =
    if useElmButtonNames then
        elm

    else
        http


unknownButtonName : String
unknownButtonName =
    "**Unknown**"


sendButtonName : Bool -> ExplorerSendMsg -> String
sendButtonName useElmButtonNames msg =
    case LE.find (\( m, _ ) -> m == msg) buttonNameAlist of
        Nothing ->
            unknownButtonName

        Just ( _, tuple ) ->
            tupleToButtonName useElmButtonNames tuple


enabledSendButton : Bool -> ExplorerSendMsg -> Model -> Html Msg
enabledSendButton enabled msg model =
    enabledButton enabled (ExplorerSendMsg msg) <|
        sendButtonName model.useElmButtonNames msg


sendButton : ExplorerSendMsg -> Model -> Html Msg
sendButton =
    enabledSendButton True

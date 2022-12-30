----------------------------------------------------------------
--
-- Model.elm
-- Model & Msg for Mammudeck.
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------


module Mammudeck.Model exposing
    ( AccountDialogContent(..)
    , AccountDialogStatuses
    , AreYouSureReason(..)
    , AttachmentView
    , BoundingBox
    , ColumnsSendMsg(..)
    , ColumnsUIMsg(..)
    , Command(..)
    , DaysHoursMinutes
    , Dialog(..)
    , DocSection(..)
    , ExplorerSendMsg(..)
    , ExplorerUIMsg(..)
    , Features
    , FeedBodyEnv
    , FeedEnv
    , FilterInput
    , FocusInput
    , GlobalMsg(..)
    , InitialPage
    , LastInstance
    , LinkInfo
    , Model
    , Msg(..)
    , Page(..)
    , PagingInput
    , Popup(..)
    , PopupChoice(..)
    , PopupExplorer(..)
    , PostPopupSearch
    , PostPopupType(..)
    , PostState
    , ReceiveFeedType(..)
    , Reference(..)
    , ReferenceDict
    , RenderEnv
    , ReplyType(..)
    , SavedModel
    , ScrollColumns
    , ScrollDirection(..)
    , ScrollPillState
    , ScrolledStatus
    , SelectedRequest(..)
    , Started(..)
    , StreamingApi(..)
    , Style(..)
    , ThreadExplorerState
    , TokenApi
    , VerticalDirection(..)
    , WhichJson(..)
    , daysHoursMinutesToSeconds
    , defaultDaysHoursMinutes
    , emptyAttachmentView
    , emptyDaysHoursMinutes
    , emptyFeedBodyEnv
    , emptyFeedEnv
    , emptyFilterInput
    , emptyInitialPage
    , emptyPagingInput
    , emptyRenderEnv
    , emptyThreadExplorerState
    , emptyTokenApi
    , encodeSavedModel
    , initialPostState
    , initialScrollPillState
    , makeFeedEnv
    , modelToSavedModel
    , savedModelDecoder
    , savedModelToModel
    , selectedRequestFromUrlDict
    , selectedRequestToString
    , selectedRequestToUrlValue
    , stringToPage
    )

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Navigation as Navigation exposing (Key)
import CustomElement.TextAreaTracker as TextAreaTracker exposing (Coordinates)
import CustomElement.WatchColorScheme as WatchColorScheme exposing (ColorScheme(..))
import Dict exposing (Dict)
import DropZone
import DynamoDB.AppState as AppState exposing (AppState, Updates)
import DynamoDB.Types
import File exposing (File)
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
        , UndisplayedElements(..)
        , UserFeedFlags
        , UserFeedParams
        )
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
        , Instance
        , ListEntity
        , Mention
        , Meta(..)
        , Notification
        , NotificationType(..)
        , Poll
        , PollOption
        , Privacy(..)
        , Relationship
        , Status
        , UrlString
        , Visibility(..)
        , WrappedStatus(..)
        )
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
import PortFunnels exposing (State)
import Set exposing (Set)
import Svg.Button as Button
import Time exposing (Posix, Zone)
import Time.Extra as TE exposing (Interval(..))
import Time.Format as Format
import Time.Format.Config.Configs as Configs
import Url exposing (Url)


type Style
    = DarkStyle
    | LightStyle
    | SystemStyle


type alias RenderEnv =
    { loginServer : Maybe String
    , accountId : String
    , style : Style
    , colorScheme : Maybe ColorScheme
    , fontSizePct : Int
    , fontSize : String --percent
    , columnWidth : Int --pixels
    , showIds : Bool

    -- not persistent
    , emojis : Dict String Emoji
    , emojisList : List Emoji
    , windowSize : ( Int, Int )
    , here : Zone
    }


emptyRenderEnv : RenderEnv
emptyRenderEnv =
    { loginServer = Nothing
    , accountId = ""
    , style = SystemStyle
    , colorScheme = Nothing
    , fontSizePct = 100
    , fontSize = "100"
    , columnWidth = 300
    , showIds = False

    -- Not persistent
    , emojis = Dict.empty
    , emojisList = []
    , windowSize = ( 1024, 768 )
    , here = Time.utc
    }


type Page
    = HomePage
    | ColumnsPage
    | ExplorerPage


type ReplyType
    = ReplyToPost
    | QuotePost
    | NoReply


type alias DaysHoursMinutes =
    { days : String
    , hours : String
    , minutes : String
    }


emptyDaysHoursMinutes : DaysHoursMinutes
emptyDaysHoursMinutes =
    DaysHoursMinutes "" "" ""


defaultDaysHoursMinutes =
    DaysHoursMinutes "1" "" ""


daysHoursMinutesToSeconds : DaysHoursMinutes -> Maybe Int
daysHoursMinutesToSeconds dhm =
    let
        toInt x =
            if x == "" then
                Just 0

            else
                String.toInt x
    in
    case toInt dhm.days of
        Nothing ->
            Nothing

        Just days ->
            case toInt dhm.hours of
                Nothing ->
                    Nothing

                Just hours ->
                    case toInt dhm.minutes of
                        Nothing ->
                            Nothing

                        Just minutes ->
                            if days < 0 || hours < 0 || minutes < 0 then
                                Nothing

                            else
                                case 60 * (minutes + 60 * (hours + 24 * days)) of
                                    0 ->
                                        Nothing

                                    seconds ->
                                        Just seconds


type alias PostState =
    { replyTo : Maybe Status
    , replyType : ReplyType
    , text : String
    , mentionsString : String
    , sensitive : Bool
    , visibility : Visibility
    , setVisibility : Visibility
    , media_ids : List String
    , fileNames : List String
    , fileUrls : List String
    , pollDefinition : Maybe PollDefinition
    , daysHoursMinutes : DaysHoursMinutes
    , groupName : String
    , group_id : Maybe String
    , deleteAndRedraft : Bool
    , posting : Bool
    }


initialPostState : PostState
initialPostState =
    { replyTo = Nothing
    , replyType = NoReply
    , text = ""
    , mentionsString = ""
    , sensitive = False
    , visibility = PublicVisibility
    , setVisibility = PublicVisibility
    , media_ids = []
    , fileNames = []
    , fileUrls = []
    , pollDefinition = Nothing
    , daysHoursMinutes = DaysHoursMinutes "" "" ""
    , groupName = ""
    , group_id = Nothing
    , deleteAndRedraft = False
    , posting = False
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


type alias BoundingBox =
    { bottom : Int
    , height : Int
    , left : Int
    , right : Int
    , top : Int
    , width : Int
    , x : Int
    , y : Int
    }


type DocSection
    = DocIntro
    | DocLogin
    | DocColumns
    | DocColumnEntry
    | DocLeftColumn
    | DocScrollPill
    | DocApi
    | DocSettingsDialog
    | DocDynamoDBDialog
    | DocEditColumnsDialog
    | DocSaveRestoreDialog
    | DocKeyboardShortcutsDialog
    | DocPostDialog


type alias LastInstance =
    { server : String
    , instance : Instance
    }


type alias InitialPage =
    { page : Maybe Page
    , request : Maybe SelectedRequest
    }


emptyInitialPage : InitialPage
emptyInitialPage =
    InitialPage Nothing Nothing


type alias ScrolledStatus =
    { status : Status
    , visited : Set String
    , displayed : List Status
    , scroll : Maybe Float
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


type PopupExplorer
    = NoPopupExplorer
    | ThreadPopupExplorer ThreadExplorerState


type AreYouSureReason
    = AreYouSureBlock (Maybe Relationship)
    | AreYouSureMute (Maybe Relationship)
    | AreYouSureDeleteStatus


type alias AccountDialogStatuses =
    { flags : UserFeedFlags
    , statuses : List Status
    }


type AccountDialogContent
    = StatusesContent AccountDialogStatuses
    | FollowingContent (List Account)
    | FollowersContent (List Account)


type Dialog
    = NoDialog
    | AlertDialog String
    | ConfirmDialog String String Msg
    | EditColumnsDialog
    | ServerDialog
    | PostDialog
    | SettingsDialog
    | DynamoDBDialog
    | KeyboardShortcutsDialog
    | SaveRestoreDialog
    | ReportDialog Status
    | AreYouSureDialog AreYouSureReason Status
    | DocsDialog
    | AccountDialog Account (Maybe AccountDialogContent)


type alias AttachmentView =
    { attachments : List Attachment
    , index : Int
    }


emptyAttachmentView : AttachmentView
emptyAttachmentView =
    { attachments = []
    , index = 0
    }


type PostPopupType
    = PostPopupAtsign
    | PostPopupSharp
    | PostPopupColon


type alias PostPopupSearch =
    { popupType : PostPopupType
    , string : String
    , position : Int
    }


type Popup
    = NoPopup
    | UserNamePopup
    | GroupNamePopup
    | HashtagPopup
    | PostGroupPopup
    | PostTextPopup PostPopupSearch
    | CommandsPopup String


type Command
    = MuteConversationCommand
    | PinOnProfileCommand
    | DeleteStatusCommand
    | DeleteAndRedraftCommand
      -- other user
    | MentionCommand
    | MuteCommand (Maybe Relationship)
    | BlockCommand (Maybe Relationship)
    | ReportCommand
      --
    | SeparatorCommand
    | CancelCommand


type PopupChoice
    = AccountChoice Account
    | GroupChoice Group
    | HashtagChoice String
    | PostGroupChoice Group
    | PostEmojiChoice Emoji
    | PostEmojiCharChoice EmojiChar
    | CommandChoice Command Status


type Reference
    = ReferencedAccount Account
    | ReferencedMention Mention


type alias ReferenceDict =
    Dict String Reference


type alias FeedBodyEnv =
    { feedId : String
    , group : Maybe Group
    , references : ReferenceDict
    , missingReplyToAccountIds : Set String
    , pollSelections : Dict String (List Int)
    , pollsSubmitted : Set String
    , now : Posix
    }


type alias FeedEnv =
    { list : Maybe ListEntity
    , headerHeight : Maybe Float
    , newElementsLeft : Int
    , newColumnsLeft : Int
    , newElementsRight : Int
    , newColumnsRight : Int
    , bodyEnv : FeedBodyEnv
    }


emptyFeedBodyEnv : FeedBodyEnv
emptyFeedBodyEnv =
    { feedId = ""
    , group = Nothing
    , references = Dict.empty
    , missingReplyToAccountIds = Set.empty
    , pollSelections = Dict.empty
    , pollsSubmitted = Set.empty
    , now = Time.millisToPosix 0
    }


emptyFeedEnv : FeedEnv
emptyFeedEnv =
    { list = Nothing
    , headerHeight = Nothing
    , newElementsLeft = 0
    , newColumnsLeft = 0
    , newElementsRight = 0
    , newColumnsRight = 0
    , bodyEnv = emptyFeedBodyEnv
    }


makeFeedEnv : String -> FeedEnv
makeFeedEnv feedId =
    { emptyFeedEnv
        | bodyEnv =
            { emptyFeedBodyEnv | feedId = feedId }
    }


type alias ScrollColumns =
    { left : Int
    , right : Int
    }


type ScrollDirection
    = ScrollRight
    | ScrollLeft


type StreamingApi
    = UnknownApi
    | NoApi
    | UrlApi String


type alias TokenApi =
    { token : Maybe String
    , api : StreamingApi
    }


emptyTokenApi : TokenApi
emptyTokenApi =
    { token = Nothing
    , api = UnknownApi
    }


type alias FocusInput =
    { x : String, y : String }


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias Model =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , streaming_api : Maybe String
    , max_toot_chars : Int
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

    -- server -> (feedid -> timestamp)
    , timestamps : Dict String (Dict String String)
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
    , accountDialogFlags : UserFeedFlags
    , userColumnFlags : UserFeedFlags

    -- Non-persistent below here
    , awaitingContext : Maybe Context
    , pollSelections : Dict String (List Int)
    , boundingBox : Maybe BoundingBox
    , appState : AppState
    , appStateAccount : DynamoDB.Types.Account
    , appStateUpdating : Bool
    , docSection : DocSection
    , maxTootCharsString : Maybe String
    , tokenText : String
    , lastInstance : Maybe LastInstance
    , initialPage : InitialPage
    , popupExplorer : PopupExplorer
    , code : Maybe String
    , dialog : Dialog
    , attachmentView : Maybe AttachmentView
    , popup : Popup
    , popupElement : Maybe Dom.Element
    , popupChoices : List PopupChoice
    , searchActive : Bool
    , nextSearch : Cmd Msg
    , sideEffectCmd : Cmd Msg
    , feedSet : FeedSet
    , webSocketFeeds : Set String
    , accountIdDict : Dict String (List AccountId)
    , dropZone : DropZone.Model
    , loadingFeeds : Set String --feed ids
    , groupDict : Dict String Group
    , feedEnvs : Dict String FeedEnv
    , showFullScrollPill : Bool
    , isTouchAware : Bool
    , bodyScroll : ScrollNotification
    , scrollColumns : ScrollColumns
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
    , clickPosition : ( Int, Int )
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
    , tokens : Dict String TokenApi
    , account : Maybe Account
    , displayName : String
    , relationships : Dict String Relationship
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
    , idleTime : Int
    }


type alias SavedModel =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , streaming_api : Maybe String
    , max_toot_chars : Int
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
    , accountDialogFlags : UserFeedFlags
    , userColumnFlags : UserFeedFlags
    }



{--Commands --}


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GlobalMsg GlobalMsg
    | ColumnsUIMsg ColumnsUIMsg
    | ColumnsSendMsg ColumnsSendMsg
    | ExplorerUIMsg ExplorerUIMsg
    | ExplorerSendMsg ExplorerSendMsg
    | ProcessLinkInfo LinkInfo
    | ScrollNotify Value
    | FocusNotify Bool
    | IdleNotify Int
    | ApplyToModel (Model -> ( Model, Cmd Msg ))
    | BoundingBoxRequest String
    | BoundingBoxNotify BoundingBox


type alias LinkInfo =
    { id : String
    , selectedRequest : SelectedRequest
    , label : String
    , setId : Model -> Model
    , message : ExplorerSendMsg
    }


type WhichJson
    = ResponseJson
    | DecodedJson


type GlobalMsg
    = WindowResize Int Int
    | SetColorScheme ColorScheme
    | Here Zone
    | SetPage String
    | SetMaxTootChars String
    | SetResponseState JsonTree.State
    | SetEntityState JsonTree.State
    | ExpandAll WhichJson
    | CollapseAll WhichJson
    | SelectTreeNode WhichJson JsonTree.KeyPath
    | SetDialog Dialog
    | SetDocSection String
    | OnKeyPress Bool String
    | OnMouseClick ( Int, Int )
    | SetServer String
    | Process Value
    | SetLoginServer
    | SetTokenText String
    | SetTokenFromText
    | LoginServer String
    | Login
    | Logout
    | ClearAllDialog
    | ClearAll
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance String (Result Error Response)
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
    | ShowServerDialog
    | SetStyle String
    | ToggleStyle
    | ReloadAllColumns
    | MarkFeedRead FeedType
    | ShowUndisplayed FeedType
    | ShowAllUndisplayed
    | RefreshFeed FeedType
    | FeedRendered Value
    | Tick Posix
    | ShowEditColumnsDialog
    | SimpleButtonMsg Button.Msg ColumnsUIMsg
    | ShowFullScrollPill
    | TimestampedCmd (Posix -> ColumnsUIMsg) Posix
    | ScrollToNew ScrollDirection FeedType
    | ScrollPage ScrollDirection
    | ScrollPageAtTime ScrollDirection Posix
    | ReloadFromServer
    | ClearFeatures
    | ShowUserDialog UserFeedParams
    | ShowMentionDialog Mention
    | ReceiveAccountDialogAccount Mention (Result Error Response)
    | ShowAccountDialog Account
    | ToggleShowAccountDialogId
    | AccountDialogCommand (List ( String, ColumnsUIMsg )) String
    | AccountDialogSetShowStatuses Bool
    | AccountDialogShowFollowing
    | AccountDialogShowFollowers
    | AccountDialogSetFlags UserFeedFlags
    | AccountDialogShowHeader Account
    | ToggleFollowAccount Bool Account
    | FetchRelationships (List Account)
    | MuteAccount Bool Account
    | BlockAccount Bool Account
    | ShowDocsDialog
    | ShowPostDialog (Maybe Status)
    | PostWithMention String
    | PostWithGroupId String
    | ShowAttachmentDialog Int Status
    | ShowSettingsDialog
    | ShowDynamoDBDialog
    | ShowKeyboardShortcutsDialog
    | ShowSaveRestoreDialog
    | YesImSure AreYouSureReason Status
    | DismissDialog
    | DismissAttachmentView
    | AddFeedType FeedType
    | DeleteFeedType FeedType
    | SetUserColumnFlags UserFeedFlags
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
    | StatusEllipsisPopup String Status
    | ClearFeedsText
    | ClearModelText
    | SetFeedsText String
    | SetModelText String
    | PostGroupNameInput String
    | TogglePostGroup
    | SetPostText String
    | SetPostVisibility String
    | PostCommand String
    | ChoosePostAttachment
    | PostAttachmentChosen File
    | PostDrop (DropZone.DropZoneMessage (List File))
    | PostAttachmentUrl String
    | DeletePostAttachment Int
    | TogglePostSensitive
    | IncrementAttachmentIndex Int
    | ShowNewFeedStatuses FeedType
    | ShowStatusImages String
    | SetPostReplyType ReplyType
    | ClearPostStateReplyTo
    | Post
    | ClearPostState
    | ProbeGroupsFeature String
    | ToggleShowLeftColumn
    | ToggleShowScrollPill
    | ToggleShowScrollPillServer
    | SetAppStateAccount DynamoDB.Types.Account
    | CommitDynamoDBDialog
    | DynamoDBSave String (Maybe Value)
    | AppStateSaved (Result AppState.Error Int)
    | AppStateUpdated (Result AppState.Error (Maybe Updates))
    | AppStateUpdateDone Bool
    | RefreshStatus Status
    | ReceiveRefreshStatus Status (Result Error Response)
    | SelectPollOption String Int Bool
    | SubmitPollVotes String String
    | ReceivePollVotes String (Result Error Response)
    | SetPollDefinitionOption Int String
    | AddPollDefinitionOption
    | RemovePollDefinitionOption Int
    | TogglePollDefinitionMultiple
    | SetDaysHoursMinutes String String


type ReceiveFeedType
    = ReceiveWholeFeed
    | ReceiveMoreFeed
    | ReceiveNewFeed


type ColumnsSendMsg
    = ColumnsSendNoop
    | ReceiveAccountByUsername (Maybe Paging) FeedType (Result Error Response)
    | ReceiveFeed Request (Maybe Paging) FeedType (Result Error Response)
    | ReceiveStatusContext Request (Result Error Response)


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
    | SendGetStatusAncestors
    | SendGetStatusDescendants
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
      -- Except posting from the Post dialog
    | ReceivePostResponse Request (Result Error Response)



--- Persistence


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { renderEnv = model.renderEnv
    , page = model.page
    , token = model.token
    , streaming_api = model.streaming_api
    , max_toot_chars = model.max_toot_chars
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
    , accountDialogFlags = model.accountDialogFlags
    , userColumnFlags = model.userColumnFlags
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
                , showIds = savedRenderEnv.showIds
            }
        , page = savedModel.page
        , token = savedModel.token
        , streaming_api = savedModel.streaming_api
        , max_toot_chars = savedModel.max_toot_chars
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
        , accountDialogFlags = savedModel.accountDialogFlags
        , userColumnFlags = savedModel.userColumnFlags
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

        SystemStyle ->
            JE.string "SystemStyle"


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

                    "SystemStyle" ->
                        JD.succeed SystemStyle

                    _ ->
                        JD.fail <| "Unknown Style: " ++ s
            )


encodeRenderEnv : RenderEnv -> Value
encodeRenderEnv env =
    JE.object
        [ ( "loginServer", ED.encodeMaybe JE.string env.loginServer )
        , ( "accountId", JE.string env.accountId )
        , ( "style", encodeStyle env.style )
        , ( "colorScheme"
          , ED.encodeMaybe WatchColorScheme.encodeColorScheme env.colorScheme
          )
        , ( "fontSizePct", JE.int env.fontSizePct )
        , ( "fontSize", JE.string env.fontSize )
        , ( "columnWidth", JE.int env.columnWidth )
        , ( "showIds", JE.bool env.showIds )
        ]


renderEnvDecoder : Decoder RenderEnv
renderEnvDecoder =
    JD.succeed
        (\loginServer accountId style colorScheme fontSizePct_ fontSize_ columnWidth showIds ->
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
                , accountId = accountId
                , style = style
                , colorScheme = colorScheme
                , fontSizePct = fontSizePct
                , fontSize = fontSize
                , columnWidth = columnWidth
                , showIds = showIds
            }
        )
        |> required "loginServer" (JD.nullable JD.string)
        |> optional "accountId" JD.string ""
        |> required "style" styleDecoder
        |> optional "colorScheme"
            (JD.map Just <| WatchColorScheme.colorSchemeDecoder)
            Nothing
        |> optional "fontSizePct" JD.int 0
        |> optional "fontSize" JD.string "100"
        |> optional "columnWidth" JD.int 300
        |> optional "showIds" JD.bool False


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
    JE.object <|
        List.concat
            [ encodePropertyAsList "replyTo"
                postState.replyTo
                (ED.encodeMaybe ED.encodeStatus)
                Nothing
            , encodePropertyAsList "replyType"
                postState.replyType
                encodeReplyType
                NoReply
            , [ ( "text", JE.string postState.text ) ]
            , encodePropertyAsList "mentionsString"
                postState.mentionsString
                JE.string
                ""
            , encodePropertyAsList "sensitive"
                postState.sensitive
                JE.bool
                False
            , encodePropertyAsList "visibility"
                postState.visibility
                ED.encodeVisibility
                PublicVisibility
            , encodePropertyAsList "setVisibility"
                postState.setVisibility
                ED.encodeVisibility
                PublicVisibility
            , encodePropertyAsList "media_ids"
                postState.media_ids
                (JE.list JE.string)
                []
            , encodePropertyAsList "fileNames"
                postState.fileNames
                (JE.list JE.string)
                []
            , encodePropertyAsList "fileUrls"
                postState.fileUrls
                (JE.list JE.string)
                []
            , encodePropertyAsList "pollDefinition"
                postState.pollDefinition
                (ED.encodeMaybe MED.encodePollDefinition)
                Nothing
            , encodePropertyAsList "daysHoursMinutes"
                postState.daysHoursMinutes
                encodeDaysHoursMinutes
                emptyDaysHoursMinutes
            , encodePropertyAsList "groupName"
                postState.groupName
                JE.string
                ""
            , encodePropertyAsList "group_id"
                postState.group_id
                (ED.encodeMaybe JE.string)
                Nothing
            , encodePropertyAsList "deleteAndRedraft"
                postState.deleteAndRedraft
                JE.bool
                False
            ]


postStateDecoder : Decoder PostState
postStateDecoder =
    JD.succeed PostState
        |> optional "replyTo" (JD.nullable ED.statusDecoder) Nothing
        |> optional "replyType" replyTypeDecoder NoReply
        |> required "text" JD.string
        |> optional "mentionsString" JD.string ""
        |> optional "sensitive" JD.bool False
        |> optional "visibility" ED.visibilityDecoder PublicVisibility
        |> optional "setVisibility" ED.visibilityDecoder PublicVisibility
        |> optional "media_ids" (JD.list JD.string) []
        |> optional "fileNames" (JD.list JD.string) []
        |> optional "fileUrls" (JD.list JD.string) []
        |> optional "pollDefinition" (JD.nullable MED.pollDefinitionDecoder) Nothing
        |> optional "daysHoursMinutes" daysHoursMinutesDecoder emptyDaysHoursMinutes
        |> optional "groupName" JD.string ""
        |> optional "group_id" (JD.nullable JD.string) Nothing
        |> optional "deleteAndRedraft" JD.bool False
        -- "posting"
        |> custom (JD.succeed False)
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


encodeDaysHoursMinutes : DaysHoursMinutes -> Value
encodeDaysHoursMinutes { days, hours, minutes } =
    JE.object
        [ ( "d", JE.string days )
        , ( "h", JE.string hours )
        , ( "m", JE.string minutes )
        ]


daysHoursMinutesDecoder : Decoder DaysHoursMinutes
daysHoursMinutesDecoder =
    JD.succeed DaysHoursMinutes
        |> required "d" JD.string
        |> required "h" JD.string
        |> required "m" JD.string


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
            , encodePropertyAsList "streaming_api"
                savedModel.streaming_api
                (ED.encodeMaybe JE.string)
                Nothing
            , encodePropertyAsList "max_toot_chars"
                savedModel.max_toot_chars
                JE.int
                300
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
            , encodePropertyAsList "prettify" savedModel.prettify JE.bool True
            , encodePropertyAsList "selectedRequest"
                savedModel.selectedRequest
                encodeSelectedRequest
                LoginSelected
            , encodePropertyAsList "username" savedModel.username JE.string ""
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
            , encodePropertyAsList "accountDialogFlags"
                savedModel.accountDialogFlags
                MED.encodeUserFeedFlags
                Types.defaultAccountDialogFlags
            , encodePropertyAsList "userColumnFlags"
                savedModel.userColumnFlags
                MED.encodeUserFeedFlags
                Types.defaultUserFeedFlags
            ]


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


selectedRequestFromUrlDict : Dict String SelectedRequest
selectedRequestFromUrlDict =
    Dict.fromList selectedRequestUrlDictPairs


selectedRequestFromString : String -> SelectedRequest
selectedRequestFromString s =
    case Dict.get s selectedRequestFromStringDict of
        Nothing ->
            LoginSelected

        Just request ->
            request


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


encodeSelectedRequest : SelectedRequest -> Value
encodeSelectedRequest selectedRequest =
    JE.string <| selectedRequestToString selectedRequest


selectedRequestDecoder : Decoder SelectedRequest
selectedRequestDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                JD.succeed <|
                    selectedRequestFromString s
            )


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "renderEnv" renderEnvDecoder emptyRenderEnv
        |> required "page" pageDecoder
        |> optional "token" (JD.nullable JD.string) Nothing
        |> optional "streaming_api" (JD.nullable JD.string) Nothing
        |> optional "max_toot_chars" JD.int 300
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
        |> optional "accountDialogFlags" MED.userFeedFlagsDecoder Types.defaultAccountDialogFlags
        |> optional "userColumnFlags" MED.userFeedFlagsDecoder Types.defaultUserFeedFlags

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
    , initialPostState
    , initialScrollPillState
    , makeFeedEnv
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
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import JsonTree exposing (TaggedValue(..))
import Mammudeck.EmojiChar as EmojiChar exposing (EmojiChar)
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

    -- Non-persistent below here
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
      -- Except posting from the Post dialog
    | ReceivePostResponse Request (Result Error Response)

----------------------------------------------------------------------
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


port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import CustomElement.WriteClipboard as WriteClipboard
import Dialog
import Dict exposing (Dict)
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
        , autofocus
        , checked
        , class
        , cols
        , disabled
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
        )
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseDown)
import Html.Lazy as Lazy
import Html.Parser as Parser exposing (Node)
import Html.Parser.Util as Util
import Http
import Iso8601
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import JsonTree exposing (TaggedValue(..))
import List.Extra as LE
import Mammudeck.EncodeDecode as MED
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
        , Renderer
        , ScrollNotification
        , ScrollState(..)
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
        , Datetime
        , Entity(..)
        , Field
        , FilterContext(..)
        , Focus
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
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Regex
import Set exposing (Set)
import String.Extra as SE
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
port openWindow : JE.Value -> Cmd msg


{-| Scroll monitor requests.

JSON: {id: <string>, enable: <bool>}

-}
port scrollRequest : JE.Value -> Cmd msg


{-| Scroll monitor notifications.

JSON: {id: <string>, scrollX: <int>, scrollY: <int>}

-}
port scrollNotify : (Value -> msg) -> Sub msg


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


type Dialog
    = NoDialog
    | ConfirmDialog String String Msg
    | EditColumnsDialog


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

    -- not persistent
    , windowSize : ( Int, Int )
    , here : Zone
    }


emptyRenderEnv : RenderEnv
emptyRenderEnv =
    { loginServer = Nothing
    , style = LightStyle
    , windowSize = ( 1024, 768 )
    , here = Time.utc
    }


type alias Model =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , server : String

    -- Columns page state
    , feedSetDefinition : FeedSetDefinition
    , supportsAccountByUsername : Dict String Bool

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

    -- Non-persistent below here
    , initialPage : InitialPage
    , dialog : Dialog
    , feedSet : FeedSet
    , loadingFeeds : Set String --loading older posts, that is
    , scrollState : Dict String ScrollState
    , accountIdDict : Dict String (List AccountId)

    -- API Explorer state
    , altKeyDown : Bool
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
    , now : Maybe Posix
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
    | ScrollNotify JE.Value


type GlobalMsg
    = WindowResize Int Int
    | GotViewportOf String (Result Dom.Error Viewport)
    | Here Zone
    | Now Posix
    | SetPage String
    | SetResponseState JsonTree.State
    | SetEntityState JsonTree.State
    | ExpandAll WhichJson
    | CollapseAll WhichJson
    | SelectTreeNode WhichJson JsonTree.KeyPath
    | SetDialog Dialog
    | OnKeyPress String
    | OnAltKey Bool
    | SetServer String
    | Process Value
    | SetLoginServer
    | Login
    | Logout
    | ClearAll
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | ReceiveFetchAccount (Result ( String, Error ) ( String, String, Account ))
    | ReceiveGetVerifyCredentials (Result Error Response)
    | ReceiveAccountIdRelationships Bool (Result Error Response)


type ColumnsUIMsg
    = ColumnsUINoop
    | ReloadAllColumns
    | ScrollRequests
    | DoScrollRequests (Cmd Msg)
    | ShowEditColumnsDialog
    | DismissDialog
    | AddFeedColumn FeedType
    | DeleteFeedColumn FeedType
    | MoveFeedColumn FeedType Int
    | UserNameInput String


type ReceiveFeedType
    = ReceiveWholeFeed
    | ReceiveMoreFeed
    | ReceiveNewFeed


type ColumnsSendMsg
    = ColumnsSendNoop
    | ReceiveAccountByUsername FeedType (Result Error Response)
    | ReceiveFeed ReceiveFeedType FeedType (Result Error Response)


type ExplorerUIMsg
    = SetWhichGroups String
    | ClearSentReceived
    | TogglePrettify
    | ToggleShowMetadata
    | ToggleShowReceived
    | ToggleShowEntity
    | ToggleStyle
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


keyDecoder : Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.map (GlobalMsg << OnKeyPress)


altKeyDecoder : Bool -> Decoder Msg
altKeyDecoder down =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if key == "Alt" then
                    JD.succeed (GlobalMsg <| OnAltKey down)

                else
                    JD.fail "Not Alt key"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions (GlobalMsg << Process) model
        , Events.onResize (\w h -> GlobalMsg <| WindowResize w h)
        , scrollNotify ScrollNotify
        , if model.dialog /= NoDialog then
            Events.onKeyDown keyDecoder

          else
            Sub.batch
                [ Events.onKeyDown <| altKeyDecoder True
                , Events.onKeyUp <| altKeyDecoder False
                ]
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

    -- Non-persistent below here
    , initialPage = initialPage
    , dialog = NoDialog
    , feedSet = Types.emptyFeedSet
    , loadingFeeds = Set.empty
    , scrollState = Dict.empty
    , accountIdDict = Dict.empty
    , altKeyDown = False
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
    , now = Nothing
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
            , Task.perform (GlobalMsg << Now) Time.now
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
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
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            handleGetResponse label key value mdl

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys model

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
            , getFeedSetDefinition
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
                    if mdl2.renderEnv.loginServer == Nothing then
                        ( mdl2
                        , Task.perform (GlobalMsg << SetServer) <|
                            Task.succeed mdl2.server
                        )

                    else
                        getVerifyCredentials mdl2


handleGetFeedSetDefinition : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetFeedSetDefinition maybeValue model =
    let
        feedSetDefinition =
            case maybeValue of
                Nothing ->
                    Types.defaultFeedSetDefinition

                Just value ->
                    case JD.decodeValue MED.feedSetDefinitionDecoder value of
                        Err _ ->
                            Types.defaultFeedSetDefinition

                        Ok fsd ->
                            fsd

        feedSet =
            Types.feedSetDefinitionToFeedSet feedSetDefinition
    in
    { model
        | feedSetDefinition = feedSetDefinition
        , feedSet = feedSet
    }
        |> withCmd
            (if model.page == ColumnsPage then
                Task.perform ColumnsUIMsg <|
                    Task.succeed ReloadAllColumns

             else
                Cmd.none
            )


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
                |> withNoCmd


pkAccountIdsLength : Int
pkAccountIdsLength =
    String.length pk.accountIds


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if key == pk.model then
                handleGetModel maybeValue model

            else if key == pk.feedSetDefinition then
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


scrollNotificationDecoder : Decoder ScrollNotification
scrollNotificationDecoder =
    JD.succeed ScrollNotification
        |> required "id" JD.string
        |> required "scrollLeft" JD.int
        |> required "scrollTop" JD.int


emptyScrollNotification : ScrollNotification
emptyScrollNotification =
    { id = ""
    , scrollLeft = 0
    , scrollTop = 0
    }


processScroll : JE.Value -> Model -> ( Model, Cmd Msg )
processScroll value model =
    case JD.decodeValue scrollNotificationDecoder value of
        Err _ ->
            model |> withNoCmd

        Ok notification ->
            let
                id =
                    notification.id

                ( scrollState, cmd ) =
                    case Dict.get id model.scrollState of
                        Just AwaitingGetViewportScroll ->
                            ( NotifyReceivedScroll, Cmd.none )

                        Just NotifyReceivedScroll ->
                            ( NotifyReceivedScroll, Cmd.none )

                        _ ->
                            ( AwaitingGetViewportScroll
                            , Task.attempt (GlobalMsg << GotViewportOf id) <|
                                Dom.getViewportOf id
                            )
            in
            { model
                | scrollState =
                    Dict.insert id scrollState model.scrollState
            }
                |> withCmd cmd


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

        GotViewportOf id result ->
            let
                scrollState =
                    Dict.remove id model.scrollState
            in
            if Set.member id model.loadingFeeds then
                { model | scrollState = scrollState }
                    |> withNoCmd

            else
                case result of
                    Err _ ->
                        { model | scrollState = scrollState }
                            |> withNoCmd

                    Ok viewport ->
                        let
                            vp =
                                viewport.viewport

                            overhang =
                                viewport.scene.height - (vp.y + vp.height)

                            h =
                                Tuple.second model.renderEnv.windowSize
                                    |> toFloat
                        in
                        if h / 4 > overhang then
                            { model | scrollState = scrollState }
                                |> withCmd (loadMoreCmd id model)

                        else
                            case Dict.get id model.scrollState of
                                Just NotifyReceivedScroll ->
                                    { model
                                        | scrollState =
                                            Dict.insert id
                                                AwaitingGetViewportScroll
                                                model.scrollState
                                    }
                                        |> withCmd
                                            (Task.attempt
                                                (GlobalMsg << GotViewportOf id)
                                             <|
                                                Dom.getViewportOf id
                                            )

                                _ ->
                                    { model | scrollState = scrollState }
                                        |> withNoCmd

        Here zone ->
            { model
                | renderEnv =
                    { renderEnv | here = zone }
            }
                |> withNoCmd

        Now posix ->
            { model | now = Just posix }
                |> withNoCmd

        SetPage pageString ->
            let
                page =
                    stringToPage pageString
            in
            { model | page = page }
                |> withCmd
                    (if page == ColumnsPage then
                        Cmd.batch
                            [ Task.perform ColumnsUIMsg <|
                                Task.succeed ScrollRequests
                            , if feedsNeedLoading model then
                                Task.perform ColumnsUIMsg <|
                                    Task.succeed ReloadAllColumns

                              else
                                Cmd.none
                            ]

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
            { model | dialog = dialog }
                |> withCmd
                    (if model.dialog == NoDialog then
                        Cmd.none

                     else
                        Task.attempt (\_ -> Noop) <|
                            Dom.focus cancelButtonId
                    )

        OnKeyPress key ->
            { model
                | dialog =
                    if key == "Escape" then
                        NoDialog

                    else
                        model.dialog
            }
                |> withNoCmd

        OnAltKey isDown ->
            { model
                | altKeyDown = isDown
            }
                |> withNoCmd

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
            in
            case Login.loginTask sau <| Dict.get model.server model.tokens of
                Redirect task ->
                    ( model, Task.attempt (GlobalMsg << ReceiveRedirect) task )

                FetchAccount task ->
                    ( model, Task.attempt (GlobalMsg << ReceiveFetchAccount) task )

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

        ClearAll ->
            let
                mdl =
                    { model
                        | dialog = NoDialog
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
                        |> withCmd cmd

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

                        ( mdl3, cmd2 ) =
                            mergeAccountId accountId server mdl2
                    in
                    mdl3
                        |> withCmds
                            [ cmd
                            , cmd2
                            , checkAccountByUsername server mdl2
                            , getAccountIdRelationships False mdl2
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

                        ( mdl2, cmd ) =
                            let
                                accountId =
                                    Types.accountToAccountId account
                            in
                            mergeAccountId accountId loginServer mdl
                    in
                    mdl2
                        |> withCmds
                            [ cmd
                            , checkAccountByUsername loginServer mdl2
                            , getAccountIdRelationships False mdl2
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


checkAccountByUsername : String -> Model -> Cmd Msg
checkAccountByUsername server model =
    case Dict.get server model.supportsAccountByUsername of
        Nothing ->
            Task.perform ExplorerSendMsg <| Task.succeed SendGetAccountByUsername

        Just _ ->
            Cmd.none


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


loadMoreCmd : String -> Model -> Cmd Msg
loadMoreCmd id model =
    case Types.feedIdToType id of
        Nothing ->
            Cmd.none

        Just feedType ->
            case LE.find (\feed -> feedType == feed.feedType) model.feedSet.feeds of
                Nothing ->
                    Cmd.none

                Just feed ->
                    let
                        i =
                            Debug.log "loadMoreCmd" id
                    in
                    -- TODO
                    Cmd.none


feedsNeedLoading : Model -> Bool
feedsNeedLoading model =
    model.account
        /= Nothing
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
    JE.object
        [ ( "id", JE.string <| Types.feedID feedType )
        , ( "enable", JE.bool enable )
        ]
        |> scrollRequest


{-| Process UI messages from the columns page.

These change the Model, but don't send anything over the wire to any instances.

-}
columnsUIMsg : ColumnsUIMsg -> Model -> ( Model, Cmd Msg )
columnsUIMsg msg model =
    case msg of
        ColumnsUINoop ->
            model |> withNoCmd

        ReloadAllColumns ->
            let
                getFeed : Feed -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                getFeed feed ( mdl, cmds ) =
                    let
                        ( mdl2, cmd ) =
                            reloadFeed feed mdl

                        scrollCmd =
                            -- Disable scroll processing while fetching
                            -- Is there a race condition here?
                            -- If so we'll need Model.noScrollFeedTypes
                            makeScrollRequest feed.feedType False
                    in
                    ( mdl2, Cmd.batch [ cmd, cmds, scrollCmd ] )
            in
            List.foldr getFeed ( model, Cmd.none ) model.feedSet.feeds

        ScrollRequests ->
            let
                getScrollRequest : FeedType -> Cmd Msg -> Cmd Msg
                getScrollRequest feedType cmds =
                    Cmd.batch [ cmds, makeScrollRequest feedType True ]

                cmd =
                    List.foldr getScrollRequest
                        Cmd.none
                        model.feedSetDefinition.feedTypes
            in
            ( model
              -- Delay for one update round-trip so view is called.
            , Task.perform ColumnsUIMsg <|
                Task.succeed (DoScrollRequests cmd)
            )

        DoScrollRequests cmd ->
            model |> withCmd cmd

        ShowEditColumnsDialog ->
            { model | dialog = EditColumnsDialog }
                |> withNoCmd

        DismissDialog ->
            { model | dialog = NoDialog }
                |> withNoCmd

        AddFeedColumn feedType ->
            addFeedType (fillinFeedType feedType model) model

        DeleteFeedColumn feedType ->
            deleteFeedType feedType model

        MoveFeedColumn feedType direction ->
            moveFeedType feedType direction model

        UserNameInput userNameInput ->
            { model | userNameInput = userNameInput }
                |> withNoCmd


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

        _ ->
            feedType


moveFeedType : FeedType -> Int -> Model -> ( Model, Cmd Msg )
moveFeedType feedType direction model =
    let
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
            let
                newFeedSetDefinition =
                    { feedSetDefinition
                        | feedTypes =
                            moveElementAt index direction feedTypes
                    }

                newFeedSet =
                    { feedSet
                        | feeds =
                            moveElementAt index direction feedSet.feeds
                    }
            in
            { model
                | feedSetDefinition = newFeedSetDefinition
                , feedSet = newFeedSet
            }
                |> withCmd (putFeedSetDefinition newFeedSetDefinition)


moveElementAt : Int -> Int -> List a -> List a
moveElementAt index direction list =
    let
        length =
            List.length list
    in
    if index == 0 && direction < 0 then
        List.concat
            [ List.drop 1 list
            , List.take 1 list
            ]

    else if index >= length - 1 && direction >= 0 then
        List.concat
            [ List.drop (length - 1) list
            , List.take (length - 1) list
            ]

    else if direction < 0 then
        let
            tail =
                List.drop (index - 1) list
        in
        List.concat
            [ List.take (index - 1) list
            , List.take 1 <| List.drop 1 tail
            , List.take 1 tail
            , List.drop 2 tail
            ]

    else
        let
            tail =
                List.drop index list
        in
        List.concat
            [ List.take index list
            , List.take 1 <| List.drop 1 tail
            , List.take 1 tail
            , List.drop 2 tail
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
    in
    { model
        | feedSetDefinition = newFeedSetDefinition
        , feedSet = newFeedSet
    }
        |> withCmds
            [ putFeedSetDefinition newFeedSetDefinition
            , makeScrollRequest feedType False
            ]


addFeedType : FeedType -> Model -> ( Model, Cmd Msg )
addFeedType feedType model =
    let
        feedSetDefinition =
            model.feedSetDefinition

        feedTypes =
            feedSetDefinition.feedTypes

        newFeedSetDefinition =
            { feedSetDefinition
                | feedTypes = List.append feedTypes [ feedType ]
            }

        feedSet =
            model.feedSet

        newFeed =
            { feedType = feedType
            , elements = Types.feedTypeToElements feedType
            }

        newFeedSet =
            { feedSet
                | feeds =
                    List.append feedSet.feeds [ newFeed ]
            }
    in
    { model
        | feedSetDefinition = newFeedSetDefinition
        , feedSet = newFeedSet
    }
        |> reloadFeed newFeed
        |> addCmd (putFeedSetDefinition newFeedSetDefinition)


{-| TODO:

This needs to be sent to `server`, not `model.renderEnv.loginServer`.
Or maybe that should be a parameter.

-}
startReloadUserFeed : UserFeedParams -> Model -> Request
startReloadUserFeed params model =
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
                            if server == "" || server == loginServer then
                                username

                            else
                                username ++ "@" ++ server
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
                            getStatusesRequest acctId.id params


{-| This processes the result of the `GetSearchAccounts` request above.
-}
continueReloadUserFeed : FeedType -> List Account -> Model -> ( Model, Cmd Msg )
continueReloadUserFeed feedType accounts model =
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
                            getStatusesRequest id params

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
                                    << ReceiveFeed ReceiveWholeFeed (UserFeed params)
                                )
                                req
                                mdl
                    in
                    mdl2 |> withCmds [ cmd, cmd2 ]

        _ ->
            model |> withNoCmd


getStatusesRequest : String -> UserFeedParams -> Request
getStatusesRequest id params =
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
            , paging = Nothing
            }


reloadFeed : Feed -> Model -> ( Model, Cmd Msg )
reloadFeed { feedType } model =
    let
        request =
            case feedType of
                HomeFeed ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetHomeTimeline { paging = Nothing }

                UserFeed params ->
                    Just <| startReloadUserFeed params model

                PublicFeed { flags } ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetPublicTimeline <|
                                -- Need to handle paging
                                case flags of
                                    Nothing ->
                                        { local = True
                                        , only_media = False
                                        , paging = Nothing
                                        }

                                    Just { local, only_media } ->
                                        { local = local
                                        , only_media = only_media
                                        , paging = Nothing
                                        }

                NotificationFeed { accountId, exclusions } ->
                    Just <|
                        NotificationsRequest <|
                            Request.GetNotifications
                                { paging = Nothing
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
            sendGeneralRequest
                (case req of
                    AccountsRequest (Request.GetAccountByUsername _) ->
                        ColumnsSendMsg
                            << ReceiveAccountByUsername feedType

                    _ ->
                        ColumnsSendMsg
                            << ReceiveFeed ReceiveWholeFeed feedType
                )
                req
                model


{-| Process Requests sent from the columns page.

These send requests over the wire to instances.

-}
columnsSendMsg : ColumnsSendMsg -> Model -> ( Model, Cmd Msg )
columnsSendMsg msg model =
    case msg of
        ColumnsSendNoop ->
            model |> withNoCmd

        ReceiveAccountByUsername feedType result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok response ->
                    case response.request of
                        AccountsRequest (Request.GetAccountByUsername _) ->
                            case response.entity of
                                AccountEntity account ->
                                    continueReloadUserFeed feedType [ account ] model

                                _ ->
                                    model |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveFeed receiveType feedType result ->
            let
                -- Dummy request
                request =
                    TimelinesRequest <|
                        Request.GetHomeTimeline { paging = Nothing }

                ( mdl, cmd ) =
                    receiveResponse request result model
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
                                elements =
                                    case e of
                                        StatusListEntity statuses ->
                                            Just <| StatusElements statuses

                                        NotificationListEntity notifications ->
                                            Just <| NotificationElements notifications

                                        AccountListEntity accounts ->
                                            Just <| AccountElements accounts

                                        _ ->
                                            Nothing

                                feedSet =
                                    mdl.feedSet

                                ( feeds, ( mdl2, cmd2 ) ) =
                                    case elements of
                                        Nothing ->
                                            ( feedSet.feeds, ( mdl, Cmd.none ) )

                                        Just elem ->
                                            case elem of
                                                AccountElements accounts ->
                                                    ( feedSet.feeds
                                                    , continueReloadUserFeed
                                                        feedType
                                                        accounts
                                                        mdl
                                                    )

                                                _ ->
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
                                                    )
                            in
                            { mdl2
                                | feedSet =
                                    { feedSet | feeds = feeds }
                            }
                                |> withCmds
                                    [ cmd
                                    , cmd2
                                    , makeScrollRequest feedType True
                                    ]


updateReceivedFeed : ReceiveFeedType -> FeedElements -> Feed -> Feed
updateReceivedFeed receiveType elements feed =
    { feed
        | elements =
            case receiveType of
                ReceiveWholeFeed ->
                    elements

                ReceiveMoreFeed ->
                    appendFeedElements feed.elements elements feed.elements

                ReceiveNewFeed ->
                    appendFeedElements elements feed.elements feed.elements
    }


appendFeedElements : FeedElements -> FeedElements -> FeedElements -> FeedElements
appendFeedElements fe1 fe2 default =
    case fe1 of
        StatusElements els1 ->
            case fe2 of
                StatusElements els2 ->
                    StatusElements <| List.append els1 els2

                _ ->
                    default

        NotificationElements els1 ->
            case fe2 of
                NotificationElements els2 ->
                    NotificationElements <| List.append els1 els2

                _ ->
                    default

        AccountElements els1 ->
            case fe2 of
                AccountElements els2 ->
                    AccountElements <| List.append els1 els2

                _ ->
                    default

        ConversationsElements els1 ->
            case fe2 of
                ConversationsElements els2 ->
                    ConversationsElements <| List.append els1 els2

                _ ->
                    default

        ResultsElements els1 ->
            case fe2 of
                ResultsElements els2 ->
                    ResultsElements <| List.append els1 els2

                _ ->
                    default


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

        ToggleStyle ->
            let
                renderEnv =
                    model.renderEnv
            in
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
                        , poll = Debug.log "poll" <| pollDefinition model
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
                            ( "BadUrl: " ++ url, threeStrikes )

                        Timeout ->
                            ( "Timeout", threeStrikes )

                        NetworkError ->
                            ( "Network Error", threeStrikes )

                        BadStatus meta status ->
                            ( "Bad status: " ++ status, ( Nothing, Nothing, Just meta ) )

                        BadBody meta jderr json ->
                            let
                                res =
                                    case JD.decodeString JD.value json of
                                        Err _ ->
                                            Nothing

                                        Ok v ->
                                            Just v

                                m =
                                    "BadBody: " ++ decodeErrorToString jderr
                            in
                            ( m, ( res, Nothing, Just meta ) )

                supportsAccountByUsername =
                    let
                        dict =
                            model.supportsAccountByUsername
                    in
                    case model.renderEnv.loginServer of
                        Nothing ->
                            dict

                        Just server ->
                            case request of
                                AccountsRequest (Request.GetAccountByUsername _) ->
                                    Dict.insert server False dict

                                _ ->
                                    dict
            in
            { model
                | msg = Just msg
                , response = response
                , entity = entity
                , metadata = metadata
                , selectedKeyPath = ""
                , selectedKeyValue = ""
                , supportsAccountByUsername = supportsAccountByUsername
            }
                |> updateJsonTrees
                |> withNoCmd

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model
            in
            { mdl
                | msg = Nothing
                , metadata = Just response.metadata
                , response = Just <| ED.entityValue response.entity
                , entity = Just response.entity
            }
                |> updateJsonTrees
                |> withNoCmd


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

        FiltersRequest (Request.PostFilter _) ->
            case response.entity of
                FilterEntity { id } ->
                    { model | filterId = id }

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

        StatusesRequest (Request.PostStatus _) ->
            case response.entity of
                StatusEntity { id } ->
                    { model
                        | statusId = id
                        , status = ""
                        , in_reply_to_id = ""
                        , quote_of_id = ""
                        , spoiler_text = ""
                        , scheduled_at = ""
                        , idempotencyKey = ""
                        , pollOptions = [ "", "" ]
                    }

                _ ->
                    model

        MediaAttachmentsRequest mediaReq ->
            case response.entity of
                AttachmentEntity { id } ->
                    let
                        mdl =
                            case mediaReq of
                                Request.PostMedia _ ->
                                    { model
                                        | media_id = id
                                        , media_ids =
                                            splitMediaIds model.media_ids
                                                |> (\ids ->
                                                        List.concat [ ids, [ id ] ]
                                                   )
                                                |> String.join ","
                                    }

                                _ ->
                                    model
                    in
                    { mdl
                        | mediaFile = Nothing
                        , mediaDescription = ""
                        , mediaFocus = { x = "", y = "" }
                    }

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

                Request.GetPublicTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetTagTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetListTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetGroupTimeline { paging } ->
                    statusSmartPaging response.entity paging model

        _ ->
            model


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


statusSmartPaging : Entity -> Maybe Paging -> Model -> Model
statusSmartPaging entity paging model =
    case entity of
        StatusListEntity statuses ->
            smartPaging statuses .id paging model

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
                { model | pagingInput = Debug.log "pagingInput" { pagingInput | max_id = getid e } }

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
            }
                |> withCmd
                    (Request.rawRequestToCmd tagger rawRequest)


saveAuthorization : String -> Authorization -> Model -> ( Model, Cmd Msg )
saveAuthorization server authorization model =
    let
        tokens =
            model.tokens
    in
    { model
        | tokens =
            Dict.insert server
                authorization.token
                tokens
    }
        |> withCmd (putToken server <| Just authorization.token)


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
    , color : String
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
        , color = "#eee"
        }
    , light =
        { backgroundColor = "white"
        , color = "black"
        }
    }


getStyle : Style -> StyleProperties
getStyle style =
    case style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light


theStyle : Style
theStyle =
    DarkStyle


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
    , h : String
    }


imageLink : ImageSpec -> Html Msg
imageLink { imageUrl, linkUrl, altText, h } =
    a
        [ href linkUrl
        , blankTarget
        ]
        [ img
            [ src imageUrl
            , alt altText
            , style "height" h
            , title altText
            ]
            []
        ]


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


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        ]
        [ b label ]


button : Msg -> String -> Html Msg
button =
    enabledButton True


view : Model -> Document Msg
view model =
    { title = "Mammudeck"
    , body =
        [ renderDialog model
        , case model.page of
            HomePage ->
                renderHome model

            ColumnsPage ->
                renderColumns model

            ExplorerPage ->
                renderExplorer model
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
    renderCenteredScreen model
        "40em"
        [ h2 [ style "text-align" "center" ]
            [ text "Mammudeck" ]
        , pageSelector True (model.renderEnv.loginServer /= Nothing) model.page
        , if model.renderEnv.loginServer == Nothing then
            p []
                [ text "Enter a 'server' name and click 'Login' or 'Set Server'."
                ]

          else
            primaryServerLine model
        , loginSelectedUI model
        , Markdown.toHtml []
            """
Mammudeck is a TweetDeck-like columnar interface to Mastodon/Pleroma. It is a work in progress. Keep an eye on the "Columns" page for new features. Use the "API Explorer" page to do low-level API hacking.

[Wikipedia says](https://en.wikipedia.org/wiki/Mastodon) that "Mastodons... are any species of extinct proboscideans in the genus Mammut (family Mammutidae), distantly related to elephants..." I removed the ending "t" from "Mammut" and added "deck" to get "Mammudeck".

There's a huge list of servers at [fediverse.network](https://fediverse.network/). This webapp doesn't know how to register a new account (yet), so you'll have to do that on the server's web site, then come back here to log in.
            """
        , p [ style "text-align" "center" ]
            [ img
                [ src "images/mammoth-500x360.png"
                , style "width" "500"
                , style "height" "360"
                , alt "Mammoth"
                ]
                []
            ]
        , p [ style "text-align" "center" ]
            [ checkBox (ExplorerUIMsg ToggleStyle)
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
                , h = "32px"
                }
            ]
        ]


{-| Pixels
-}
columnWidth : Int
columnWidth =
    300


leftColumnWidth : Int
leftColumnWidth =
    120


renderLeftColumn : RenderEnv -> Html Msg
renderLeftColumn renderEnv =
    let
        { color } =
            getStyle renderEnv.style
    in
    div
        [ style "color" color
        , style "width" <| px leftColumnWidth
        , style "padding-top" "5px"
        ]
        [ case renderEnv.loginServer of
            Nothing ->
                text ""

            Just server ->
                span []
                    [ link server <| "https://" ++ server
                    , br
                    ]
        , pageSelector False (renderEnv.loginServer /= Nothing) ColumnsPage
        , br
        , button (ColumnsUIMsg ReloadAllColumns) "reload"
        , br
        , button (ColumnsUIMsg ShowEditColumnsDialog) "edit"
        , br
        , checkBox (ExplorerUIMsg ToggleStyle)
            (renderEnv.style == DarkStyle)
            "dark"
        ]


renderFeed : RenderEnv -> Feed -> Html Msg
renderFeed renderEnv { feedType, elements } =
    let
        { color } =
            getStyle renderEnv.style

        ( _, h ) =
            renderEnv.windowSize
    in
    div
        [ style "width" <| px columnWidth
        , style "height" <| px (h - 20)
        , style "border" <| "1px solid " ++ color
        ]
        [ div
            [ style "border" <| "1px solid " ++ color
            , style "text-align" "center"
            , style "color" color
            ]
            [ feedTitle feedType ]
        , div
            [ style "height" "calc(100% - 1.4em)"
            , style "overflow-y" "auto"
            , style "overflow-x" "hidden"
            , id <| Types.feedID feedType
            ]
          <|
            case elements of
                StatusElements statuses ->
                    List.map (renderStatus renderEnv) statuses

                NotificationElements notifications ->
                    let
                        gangedNotifications =
                            gangNotifications notifications

                        ( _, _ ) =
                            ( Debug.log "notifications" <| List.length notifications
                            , Debug.log "  ganged" <| List.length gangedNotifications
                            )
                    in
                    List.map (renderGangedNotification renderEnv) gangedNotifications

                _ ->
                    [ text "" ]
        ]


renderGangedNotification : RenderEnv -> GangedNotification -> Html Msg
renderGangedNotification renderEnv gangedNotification =
    -- TODO
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
            notificationDescriptionWithDisplayName display_name notification

        timeString =
            formatIso8601 renderEnv.here notification.created_at
    in
    div
        [ style "border" <| "1px solid " ++ color
        , style "color" color
        , style "padding" "0 3px"
        ]
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
                    , h = "1.5em"
                    }
            )
            (account :: others)
            |> List.intersperse (text " ")
            |> span []
        , renderNotificationBody renderEnv notification
        ]


notificationStatusId : Notification -> String
notificationStatusId notification =
    case notification.status of
        Just { id } ->
            id

        Nothing ->
            ""


gangNotifications : List Notification -> List GangedNotification
gangNotifications notifications =
    let
        loop : List Notification -> List GangedNotification -> List GangedNotification
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

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
                            loop cdr <|
                                { id = id
                                , notification = car
                                , accounts = [ car.account ]
                                }
                                    :: res

                        Just gn ->
                            loop cdr <|
                                { gn
                                    | accounts = car.account :: gn.accounts
                                }
                                    :: List.filter ((/=) gn) res
    in
    loop notifications []


notificationDescription : Notification -> Html Msg
notificationDescription notification =
    notificationDescriptionWithDisplayName notification.account.display_name
        notification


notificationDescriptionWithDisplayName : String -> Notification -> Html Msg
notificationDescriptionWithDisplayName display_name notification =
    let
        postName =
            if notification.type_ == PollNotification then
                text "poll"

            else
                text "your post"
    in
    case notification.type_ of
        FollowNotification ->
            span [] [ b display_name, text " followed you" ]

        MentionNotification ->
            span [] [ b display_name, text " mentioned you" ]

        ReblogNotification ->
            span [] [ b display_name, text " reblogged ", postName ]

        FavouriteNotification ->
            span [] [ b display_name, text " favorited ", postName ]

        PollNotification ->
            span [] [ b display_name, text "'s ", postName, text " is closed" ]


renderNotification : RenderEnv -> Notification -> Html Msg
renderNotification renderEnv notification =
    let
        description =
            notificationDescription notification

        { color } =
            getStyle renderEnv.style
    in
    div [ style "border" <| "1px solid " ++ color ]
        [ div []
            [ renderAccount color
                renderEnv.here
                notification.account
                description
                notification.created_at
                Nothing
            , renderNotificationBody renderEnv notification
            ]
        ]


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
                    case Parser.run status.content of
                        Ok nodes ->
                            Util.toVirtualDom nodes

                        Err _ ->
                            [ text status.content ]

                timeString =
                    formatIso8601 renderEnv.here status.created_at

                postLink =
                    case status.url of
                        Nothing ->
                            text timeString

                        Just url ->
                            link timeString url
            in
            div []
                [ hr
                , div
                    [ class "content"
                    , style "color" color
                    ]
                  <|
                    postLink
                        :: body
                , div [] <|
                    List.map (renderAttachment renderEnv) status.media_attachments
                ]


feedTitle : FeedType -> Html Msg
feedTitle feedType =
    case feedType of
        HomeFeed ->
            b "Home"

        UserFeed { username } ->
            b <| "User: " ++ username

        PublicFeed _ ->
            b "Public"

        NotificationFeed _ ->
            b "Notifications"

        _ ->
            text ""


renderAccount : String -> Zone -> Account -> Html Msg -> Datetime -> Maybe UrlString -> Html Msg
renderAccount color zone account description datetime url =
    table []
        [ tr []
            [ td []
                [ imageLink
                    { imageUrl = account.avatar
                    , linkUrl = account.url
                    , altText = ""
                    , h = "3em"
                    }
                ]
            , td [ style "color" color ]
                [ description
                , br
                , link ("@" ++ account.username) account.url
                , br
                , let
                    timeString =
                        formatIso8601 zone datetime
                  in
                  case url of
                    Nothing ->
                        text timeString

                    Just u ->
                        link timeString u
                ]
            ]
        ]


renderStatus : RenderEnv -> Status -> Html Msg
renderStatus renderEnv statusIn =
    let
        ( status, account, reblogAccount ) =
            case statusIn.reblog of
                Nothing ->
                    ( statusIn, statusIn.account, Nothing )

                Just (WrappedStatus reblog) ->
                    ( reblog, reblog.account, Just statusIn.account )

        { color } =
            getStyle renderEnv.style

        body =
            case Parser.run status.content of
                Ok nodes ->
                    Util.toVirtualDom nodes

                Err _ ->
                    [ text status.content ]
    in
    div [ style "border" <| "1px solid " ++ color ]
        [ div []
            [ div
                [ class "content"
                , style "color" color
                ]
                [ case reblogAccount of
                    Nothing ->
                        text ""

                    Just acct ->
                        span []
                            [ link acct.display_name acct.url
                            , text " reblogged:"
                            ]
                , renderAccount color
                    renderEnv.here
                    account
                    (b account.display_name)
                    status.created_at
                    status.url
                ]
            , hr
            , div
                [ class "content"
                , style "color" color
                ]
                body
            , div [] <|
                List.map (renderAttachment renderEnv) status.media_attachments
            ]
        ]


hrpct : Int -> Html msg
hrpct pct =
    Html.hr [ style "width" <| String.fromInt pct ++ "%" ] []


hr : Html msg
hr =
    hrpct 90


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
            img
                [ src attachment.preview_url
                , alt "image"
                , style "width" "100%"
                ]
                []

        _ ->
            text ""


px : Int -> String
px int =
    String.fromInt int ++ "px"


pxBang : Int -> String
pxBang int =
    px int ++ " !important"


renderColumns : Model -> Html Msg
renderColumns model =
    let
        renderEnv =
            model.renderEnv

        { feeds } =
            model.feedSet

        ( _, h ) =
            renderEnv.windowSize

        tableWidth =
            leftColumnWidth + List.length feeds * columnWidth
    in
    renderCenteredScreen model
        ""
        [ table
            [--style "width" <| px tableWidth
            ]
            [ tr
                [-- style "width" <| px tableWidth
                ]
              <|
                List.concat
                    [ [ td [ style "vertical-align" "top" ]
                            [ Lazy.lazy renderLeftColumn renderEnv ]
                      ]
                    , List.map
                        (\feed ->
                            td [ style "vertical-align" "top" ]
                                [ Lazy.lazy2 renderFeed renderEnv feed ]
                        )
                        feeds
                    ]
            ]
        ]


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
                    loginSelectedUI
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
                            if model.altKeyDown then
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
                [ button
                    ((GlobalMsg << SetDialog) <|
                        ConfirmDialog
                            "Do you really want to erase everything?"
                            "Erase"
                            (GlobalMsg ClearAll)
                    )
                    "Clear All Persistent State"
                ]
            , br
            , p
                [ onClick (ExplorerUIMsg ToggleStyle)
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
                [ text <| "Copyright " ++ special.copyright ++ " 2019, Bill St. Clair"
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


checkBox : Msg -> Bool -> String -> Html Msg
checkBox msg isChecked label =
    span
        [ onClick msg
        , style "cursor" "default"
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            ]
            []
        , b label
        ]


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


loginSelectedUI : Model -> Html Msg
loginSelectedUI model =
    p []
        [ pspace
        , b "server: "
        , input
            [ size 30
            , onInput (GlobalMsg << SetServer)
            , value model.server
            , placeholder "mastodon.social"
            , onKeyUp
                (\code ->
                    if code == 13 then
                        GlobalMsg Login

                    else
                        Noop
                )
            ]
            []
        , text " "
        , serverSelect model
        , br
        , Html.button
            [ onClick (GlobalMsg Login)
            , disabled <| model.server == ""
            ]
            [ b "Login" ]
        , text " "
        , button (GlobalMsg SetLoginServer) "Set Server"
        ]


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
                , style "font-size" "14px"
                ]
        ]


cancelButtonId : String
cancelButtonId =
    "cancelButton"


renderDialog : Model -> Html Msg
renderDialog model =
    case model.dialog of
        NoDialog ->
            text ""

        EditColumnsDialog ->
            editColumnsDialog model

        ConfirmDialog content okButtonText msg ->
            Dialog.render
                { styles = [ ( "width", "40%" ) ]
                , title = "Confirm"
                , content = [ text content ]
                , actionBar =
                    [ Html.button
                        [ onClick <| (GlobalMsg << SetDialog) NoDialog
                        , id cancelButtonId
                        ]
                        [ b "Cancel" ]
                    , text <| String.repeat 4 special.nbsp
                    , button msg okButtonText
                    ]
                }
                (model.dialog /= NoDialog)


editColumnsDialog : Model -> Html Msg
editColumnsDialog model =
    Dialog.render
        { styles = [ ( "width", "40%" ) ]
        , title = "Edit Columns"
        , content = editColumnDialogRows model
        , actionBar =
            [ button (ColumnsUIMsg DismissDialog) "OK" ]
        }
        True


plus : String
plus =
    "+"


editColumnDialogRows : Model -> List (Html Msg)
editColumnDialogRows model =
    let
        feedTypes =
            model.feedSetDefinition.feedTypes

        row : List (Html Msg) -> Msg -> Html Msg
        row td1 msg =
            tr []
                [ td [] td1
                , td [] [ button msg "+" ]
                ]
    in
    -- This will change quite a bit when I add multiple-server support
    [ table [] <|
        List.concat
            [ if List.member HomeFeed feedTypes then
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

                            _ ->
                                False
                    )
                    feedTypes
              of
                Just _ ->
                    []

                Nothing ->
                    [ row [ b "Public" ]
                        (ColumnsUIMsg <|
                            AddFeedColumn (PublicFeed { flags = Nothing })
                        )
                    ]
            , [ row
                    [ b "User: "
                    , input
                        [ size 30
                        , onInput (ColumnsUIMsg << UserNameInput)
                        , value model.userNameInput
                        , placeholder <|
                            "username@"
                                ++ Maybe.withDefault "server" model.renderEnv.loginServer
                        ]
                        []
                    ]
                    (ColumnsUIMsg <| AddFeedColumn Types.defaultUserFeedType)
              ]
            ]
    , hrpct 100
    , let
        feedRow feedType =
            tr []
                [ td [] [ feedTitle feedType ]
                , td [] [ button (ColumnsUIMsg <| DeleteFeedColumn feedType) "Delete" ]
                , td [] [ button (ColumnsUIMsg <| MoveFeedColumn feedType -1) "<-" ]
                , td [] [ button (ColumnsUIMsg <| MoveFeedColumn feedType 1) "->" ]
                ]
      in
      table [] <|
        List.map feedRow feedTypes
    ]


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

Click "$PostStatus" to create a new status, using "status", "in reply to id", "group id", "quote of id", "spoiler text", "visibility", "scheduled at", "language", "idempotency key", and "media ids".

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
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { renderEnv = model.renderEnv
    , page = model.page
    , token = model.token
    , server = model.server
    , feedSetDefinition = model.feedSetDefinition
    , supportsAccountByUsername = model.supportsAccountByUsername
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
            }
        , page = savedModel.page
        , token = savedModel.token
        , server = savedModel.server
        , feedSetDefinition = savedModel.feedSetDefinition
        , supportsAccountByUsername = savedModel.supportsAccountByUsername
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
        ]


renderEnvDecoder : Decoder RenderEnv
renderEnvDecoder =
    JD.succeed
        (\loginServer style ->
            { emptyRenderEnv
                | loginServer = loginServer
                , style = style
            }
        )
        |> required "loginServer" (JD.nullable JD.string)
        |> required "style" styleDecoder


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "renderEnv", encodeRenderEnv savedModel.renderEnv )
        , ( "page", encodePage savedModel.page )
        , ( "token", ED.encodeMaybe JE.string savedModel.token )
        , ( "server", JE.string savedModel.server )
        , ( "feedSetDefinition"
          , MED.encodeFeedSetDefinition savedModel.feedSetDefinition
          )
        , ( "supportsAccountByUsername"
          , JE.dict identity JE.bool savedModel.supportsAccountByUsername
          )
        , ( "accountId", JE.string savedModel.accountId )
        , ( "accountIds", JE.string savedModel.accountIds )
        , ( "showMetadata", JE.bool savedModel.showMetadata )
        , ( "q", JE.string savedModel.q )
        , ( "resolve", JE.bool savedModel.resolve )
        , ( "following", JE.bool savedModel.following )
        , ( "groupId", JE.string savedModel.groupId )
        , ( "showReceived", JE.bool savedModel.showReceived )
        , ( "showEntity", JE.bool savedModel.showEntity )
        , ( "whichGroups", encodeWhichGroups savedModel.whichGroups )
        , ( "followReblogs", JE.bool savedModel.followReblogs )
        , ( "onlyMedia", JE.bool savedModel.onlyMedia )
        , ( "pinned", JE.bool savedModel.pinned )
        , ( "excludeReplies", JE.bool savedModel.excludeReplies )
        , ( "excludeReblogs", JE.bool savedModel.excludeReblogs )
        , ( "pagingInput", encodePagingInput savedModel.pagingInput )
        , ( "local", JE.bool savedModel.local )
        , ( "hashtag", JE.string savedModel.hashtag )
        , ( "listId", JE.string savedModel.listId )
        , ( "smartPaging", JE.bool savedModel.smartPaging )
        , ( "showJsonTree", JE.bool savedModel.showJsonTree )
        , ( "showUpdateCredentials", JE.bool savedModel.showUpdateCredentials )
        , ( "statusId", JE.string savedModel.statusId )
        , ( "useElmButtonNames", JE.bool savedModel.useElmButtonNames )
        , ( "showPostStatus", JE.bool savedModel.showPostStatus )
        , ( "excludedNotificationTypes"
          , JE.list ED.encodeNotificationType savedModel.excludedNotificationTypes
          )
        , ( "notificationsAccountId"
          , JE.string savedModel.notificationsAccountId
          )
        , ( "notificationId", JE.string savedModel.notificationId )
        , ( "muteNotifications", JE.bool savedModel.muteNotifications )
        , ( "groupIds", JE.string savedModel.groupIds )
        , ( "offset", JE.string savedModel.offset )
        , ( "listTitle", JE.string savedModel.listTitle )
        , ( "filterId", JE.string savedModel.filterId )
        , ( "filterInput", encodeFilterInput savedModel.filterInput )
        , ( "scheduledStatusId", JE.string savedModel.scheduledStatusId )
        , ( "userNameInput", JE.string savedModel.userNameInput )
        ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "renderEnv" renderEnvDecoder
        |> required "page" pageDecoder
        |> optional "token" (JD.nullable JD.string) Nothing
        |> required "server" JD.string
        |> optional "feedSetDefinition"
            MED.feedSetDefinitionDecoder
            Types.defaultFeedSetDefinition
        |> optional "supportsAccountByUsername" (JD.dict JD.bool) Dict.empty
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


getFeedSetDefinition : Cmd Msg
getFeedSetDefinition =
    get pk.feedSetDefinition


putFeedSetDefinition : FeedSetDefinition -> Cmd Msg
putFeedSetDefinition feedSetDefinition =
    put pk.feedSetDefinition (Just <| MED.encodeFeedSetDefinition feedSetDefinition)


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
    }



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

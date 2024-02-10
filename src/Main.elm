---------------------------------------------------------------
--
-- Main.elm
-- Mammudeck, a TweetDeck-like columnar interface to Mastodon/Pleroma.
-- Copyright (c) 2019-2022 Bill St. Clair <billstclair@gmail.com>
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
--   ?page=[home|columns|api]
--   &api=[key in selectedRequestFromUrlDict]
--
----------------------------------------------------------------------
{--Immediate TODOs

See ../TODO.md for the full list.

* /api/v1/pleroma/statuses/:id/quotes
  Returns a list of Status entities, each of which quotes :id.

* Where to get "Display Lanauge" as the default 'to' language for translation.
  Allow input of that field, as well?

* Chat, compatible with Soapbox/Rebased.

* https://github.com/nostr-protocol/
  Log in to nostr-protocol relays

* Support notification type "move".
  It has a "target" field, which is the new Account entity for a moved account.
  Besides displaying the new account, update internal tables and persistent
  state with the new address.
  Example, Gleasonator.com notification id: 729894

* account.is_verified came from Gab. Pleroma represents this as
  account.pleroma.is_admin, .is_confirmed, .is_moderator, and .is_suggested
  Mastodon doesn't appear to have any such property, but I need to look more.
  Look for Soapbox's icons for the four fields.
  I already have one for account.is_verified. Different colors?

* Post quoting.
  <Instance>.pleroma.metadata.features contains "editing" and "quote_posting"
    on Rebased servers.
  See https://gleasonator.com/@billstclair/posts/AQ4iSUavHlJyNeA9bM
  If a status quotes another, its `pleroma.quote` field will contain the quoted status.
    It also has a weird `<span class="quote-inline">...</span>` just before
    the `</p>` of the final user paragraph. The `span` contains a link
    to the quoted post.
    e.g. id: 
  The `<status>.pleroma.quotes_count` field says how many posts quote this one.
  You can fetch them with:
    https://gleasonator.com/api/v1/pleroma/statuses/AQ6zpQ36BCMlwNTswK/quotes?limit=20&max_id=AQ6zqgqPDk9aJt9Mci&offset=0

* Speed up display of hell threads in the thread explorer.

* Switch attachments on right/left keys. Move buttons closer.
  Wrap arround, with some visual clue that you're doing that.

* Adjust z-index of thread explorer, account dialog, and attachment
  viewer, so that if you get to account dialog from thread explorer,
  or vice-versa, everything works as expected.

* Mini account dialog on hover?

* `"pleroma:emoji-reaction"` as `Notification` type, with an `emoji`
  string.

* Brave.com and TruthSocial.com pop up a little window saying that "An
  update is available". Much easier to click on that than to navigate
  to the Settings dialog and click the "Reload from Server" button. Do
  it by saving a small version file and probing that periodically.

* Clicking on #foo should go to that column, if it exists, or bring
  up a dialog, showing those posts, with an "Add Column" button.

* A link to a post on another server should come up in a thread explorer.
  Except I don't know how to turn that post's ID on the other server
  into an ID on the local server. Maybe just pull the post directly from
  the other server, if it allows that. If not, open in a new browser tab.
  Brings up all sorts of issues, though, when clicking on accounts.
  ID conversion problems there, too.

* Better link parsing. Shouldn't need "https://" prefix, and links at
  beginning of post, or just before punctuation, should work.
  Soapbox/Rebased does this for us, but Mastodon doesn't.

* Look up custom emojis on the poster's server, but only if he uses one.

* Auto-refresh:
    "Show all undisplayed", maybe on "u".
    Periodic auto-update of user feeds.
    Manual update of user feed should include the undisplayed posts as new.
        (may be hard to do and I'll skip it).

* Refresh:
    <Shift> is a bad choice for the non-incremental indicator.
        It sometimes selects text.
        Pick some other way.
    Finally, automatic update, using WebSockets when possible or
        polling when not.

* ThreadExplorer:
    ** Be less eager to pull from the server.
    ** When click on existing status, scroll it to the top.
        We want to see the replies, since we already saw the
        reply-to in the previous screen.
        First status wants go at the bottom, as it does now.

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
    Feeds for servers with no login. No replies for those.

* Ellipsis dialog: block, mute, (un)follow, delete, edit, (un)mute status

--}


port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import CustomElement.WatchColorScheme as WatchColorScheme exposing (ColorScheme(..))
import Delay
import Dict exposing (Dict)
import DropZone
import DynamoDB
import DynamoDB.AppState as AppState exposing (AppState, Updates)
import DynamoDB.Types
import File exposing (File)
import File.Select
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import JsonTree exposing (TaggedValue(..))
import List.Extra as LE
import Mammudeck.EmojiChar as EmojiChar exposing (EmojiChar)
import Mammudeck.EncodeDecode as MED exposing (encodePropertyAsList)
import Mammudeck.Model
    exposing
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
        , defaultedStatusLanguage
        , defaultedTargetLanguage
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
        , knownContentTypes
        , makeFeedEnv
        , markdownContentType
        , modelToSavedModel
        , plainTextContentType
        , savedModelDecoder
        , savedModelToModel
        , selectedRequestFromUrlDict
        , selectedRequestToUrlValue
        , stringToPage
        )
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
        , makeUserFeed
        )
import Mammudeck.UI
    exposing
        ( NodeId(..)
        , ScrollInfo
        , columnScrollInfo
        , defaultPollDefinition
        , docSections
        , featureNames
        , feedTypeEqual
        , findFeed
        , focusId
        , getFeedEnv
        , getGroup
        , headerFeedId
        , isKeyDown
        , isReplyChain
        , isSpecialKeyDown
        , keyboard
        , labelToStyle
        , leftColumnWidth
        , nodeIds
        , parseFocus
        , pollInvalidReason
        , pollTimeUntil
        , postCommand
        , serverHasFeature
        , serverHasPostFormat
        , serverKnowsFeature
        , serverPostFormats
        , serverSupportsMarkdown
        , setServerHasFeature
        , threadExplorerStatusId
        , usernameAtServer
        , view
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
        , PollDefinition
        , PollOption
        , Privacy(..)
        , Relationship
        , Status
        , UrlString
        , Visibility(..)
        , WrappedStatus(..)
        )
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.Request as Request
    exposing
        ( EditedStatus
        , Error(..)
        , FieldUpdate
        , Paging
        , PartialContext(..)
        , PostedStatus
        , RawRequest
        , Request(..)
        , Response
        , WhichGroups
        , emptyPaging
        )
import Mastodon.WebSocket exposing (Event(..), StreamType(..))
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Set exposing (Set)
import Svg.Button as Button exposing (Button, TriangularButtonDirection(..))
import Task exposing (Task)
import Time exposing (Month, Posix, Zone)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


{-| This is used by links created by Util.toVirtualDom calls below.

It forces them to open in a new tab/window.

-}
port openWindow : Value -> Cmd msg


{-| Bounding box requests.
-}
port boundingBoxRequest : String -> Cmd msg


{-| Bounding box result.
-}
port boundingBoxNotify : (BoundingBox -> msg) -> Sub msg


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


{-| Track idle time, so we don't probe DynamoDB if nobody is here.
-}
port idleNotify : (Int -> msg) -> Sub msg


{-| Reset a `select` element to its `selected` option.
-}
port resetSelectOption : String -> Cmd msg


{-| Stop all videos that have an `id` from playing.
-}
port stopVideos : () -> Cmd msg


{-| How many followers or following to fetch.
-}
accountDialogRelationsCount : Int
accountDialogRelationsCount =
    100


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
        [ ( "h", ShowDocsDialog )
        , ( "p", ShowPostDialog Nothing )
        , ( "r", ReloadAllColumns )
        , ( "R", ReloadAllColumns )
        , ( "u", ShowAllUndisplayed )
        , ( ".", ShowSettingsDialog )
        , ( "t", ToggleStyle )
        , ( ",", ShowSettingsDialog )
        , ( "s", ShowDynamoDBDialog )
        , ( "v", ShowServerDialog )
        , ( "c", ShowEditColumnsDialog )
        , ( "?", ShowKeyboardShortcutsDialog )
        , ( "o", ShowSaveRestoreDialog )
        , ( "j", ScrollPage ScrollLeft )
        , ( "a", ScrollPage ScrollLeft )
        , ( "l", ScrollPage ScrollRight )
        , ( "d", ScrollPage ScrollRight )
        , ( "Escape", DismissDialog )
        ]


keyDecoder : Bool -> Decoder Msg
keyDecoder keyDown =
    JD.field "key" JD.string
        |> JD.map (GlobalMsg << OnKeyPress keyDown)


mouseDecoder : Decoder Msg
mouseDecoder =
    JD.field "screenX" JD.int
        |> JD.andThen
            (\screenX ->
                JD.field "screenY" JD.int
                    |> JD.andThen
                        (\screenY ->
                            JD.succeed (GlobalMsg <| OnMouseClick ( screenX, screenY ))
                        )
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions (GlobalMsg << Process) model
        , Events.onResize (\w h -> GlobalMsg <| OnResize w h)
        , boundingBoxNotify BoundingBoxNotify
        , scrollNotify ScrollNotify
        , focusNotify FocusNotify
        , idleNotify IdleNotify
        , Events.onKeyDown <| keyDecoder True
        , Events.onKeyUp <| keyDecoder False
        , Events.onClick mouseDecoder
        , Time.every 1000 (ColumnsUIMsg << Tick)
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


type alias CodeAndError =
    { code : Maybe String
    , error : Maybe String
    }


parseQuery : String -> CodeAndError
parseQuery queryString =
    let
        url =
            { emptyUrl | query = Just queryString }

        qp =
            QP.map2 CodeAndError
                (QP.string "code")
                (QP.string "error")
    in
    Parser.parse (Parser.s emptyElement <?> qp) url
        |> Maybe.withDefault (CodeAndError Nothing Nothing)


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
                                    "home" ->
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


type CodeOrError
    = CodeCode String
    | CodeError String
    | NoCode


{-| This recognizes `?code=<code>` or `?error=<error>`

in the URL from the redirect from authentication.

-}
receiveCodeOrError : Url -> CodeOrError
receiveCodeOrError url =
    case url.query of
        Nothing ->
            NoCode

        Just q ->
            case parseQuery q of
                { code, error } ->
                    case code of
                        Just cod ->
                            CodeCode cod

                        Nothing ->
                            case error of
                                Just err ->
                                    CodeError err

                                Nothing ->
                                    NoCode


emptyAppStateAccount : DynamoDB.Types.Account
emptyAppStateAccount =
    let
        account =
            AppState.emptyAccount
    in
    { account
        | name = "mammudeck"
        , tableName = "mammudeck"
    }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        hideClientId =
            case JD.decodeValue JD.bool value of
                Err _ ->
                    False

                Ok hide ->
                    hide

        ( code, msg ) =
            case receiveCodeOrError url of
                CodeCode cod ->
                    ( Just cod, Nothing )

                CodeError m ->
                    ( Nothing, Just m )

                NoCode ->
                    ( Nothing, Nothing )

        initialPage =
            Debug.log "initialPage" <| parseInitialPage url
    in
    { renderEnv = emptyRenderEnv
    , page = HomePage
    , token = Nothing
    , streaming_api = Nothing
    , max_toot_chars = 300
    , server = ""
    , feedSetDefinition = Types.emptyFeedSetDefinition
    , supportsAccountByUsername = Dict.empty
    , postState = initialPostState
    , scrollPillState = initialScrollPillState
    , showLeftColumn = True
    , prettify = True
    , selectedRequest = LoginSelected
    , username = ""
    , accountId = ""
    , accountIds = ""
    , timestamps = Dict.empty
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
    , accountDialogFlags = Types.defaultAccountDialogFlags
    , userColumnFlags = Types.defaultUserFeedFlags
    , publicColumnFlags = Types.defaultPublicFeedFlags
    , notificationColumnParams = Types.defaultNotificationFeedParams

    -- Non-persistent below here
    , awaitingContext = Nothing
    , pollSelections = Dict.empty
    , boundingBox = Nothing
    , appState = AppState.makeAppState emptyAppStateAccount
    , appStateAccount = emptyAppStateAccount
    , appStateUpdating = False
    , docSection = DocIntro --should be persistent?
    , maxTootCharsString = Nothing
    , tokenText = ""
    , lastInstance = Nothing
    , initialPage = initialPage
    , popupExplorer = NoPopupExplorer
    , code = code
    , dialog = NoDialog
    , attachmentView = Nothing
    , popup = NoPopup
    , popupElement = Nothing
    , popupChoices = []
    , searchActive = False
    , nextSearch = Cmd.none
    , sideEffectCmd = Cmd.none
    , feedSet = Types.emptyFeedSet
    , webSocketFeeds = Set.empty
    , accountIdDict = Dict.empty
    , dropZone = DropZone.init
    , loadingFeeds = Set.empty
    , groupDict = Dict.empty
    , feedEnvs = Dict.empty
    , showFullScrollPill = False
    , isTouchAware = False
    , bodyScroll = emptyScrollNotification
    , scrollColumns = { left = 0, right = 0 }
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
    , clickPosition = ( 0, 0 )
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
    , relationships = Dict.empty
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
    , content_type = ""
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
    , idleTime = 0
    }
        -- As soon as the localStorage module reports in,
        -- we'll load the saved model,
        -- and then all the saved tokens.
        -- See `storageHandler` below, `get pk.model`.
        -- If there was a `code`, receiving the saved model will
        -- load the saved `App` (saved by the `putApp` call in the
        -- `ReceiveRedirect` message handler), and receiving that will
        -- do `Login.getTokenTask`.
        |> withCmds
            [ Navigation.replaceUrl key url.path
            , Task.perform getViewport Dom.getViewport
            , Task.perform (GlobalMsg << Here) Time.here
            , makeScrollRequestWithId "body" True
            , Task.perform (ColumnsUIMsg << Tick) Time.now
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
                    , get pk.dynamoDBAccount
                    , listKeysLabeled pk.token (pk.token ++ ".")
                    , listKeysLabeled pk.timestamps (pk.timestamps ++ ".")
                    , listKeysLabeled pk.timestamp (pk.timestamp ++ ".")
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


serverRequest : (id -> Result Error Response -> Msg) -> id -> Request -> Model -> Cmd Msg
serverRequest wrapper id request model =
    case model.renderEnv.loginServer of
        Nothing ->
            Cmd.none

        Just server ->
            Request.serverRequest wrapper
                []
                { server = server
                , token = model.token
                }
                id
                request


getInstance : Model -> Cmd Msg
getInstance model =
    let
        serverInfo =
            { server = model.server
            , token = Nothing
            }
    in
    Request.serverRequest (\id -> GlobalMsg << ReceiveInstance model.server)
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
                    --mergeAccountId Types.emptyAccountId server model
                    ( model, Cmd.none )

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
            -- label will be pk.token or pk.timestamps or pk.timestamp,
            -- but we won't care about that until the value comes in to
            -- handleGetResponse below.
            model |> withCmds (List.map (getLabeled label) keys)



-- TODO: Handle model.code not Nothing by getting saved App, then
-- minting token, then continuing as if logged in initially.
-- Save App on ReceiveRedirect.


handleGetModel : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModel maybeValue model =
    let
        model2 =
            { model
                | started = Started
                , msg = Nothing
            }
    in
    case maybeValue of
        Nothing ->
            model2 |> processInitialPage

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model2
                        | msg =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> processInitialPage

                Ok savedModel ->
                    let
                        mdl =
                            savedModelToModel savedModel model2
                    in
                    case mdl.code of
                        Just code ->
                            mdl |> withCmd (getApp mdl.server)

                        Nothing ->
                            let
                                ( mdl3, cmd3 ) =
                                    mdl |> processLoginServer

                                ( mdl4, cmd4 ) =
                                    mdl3 |> processInitialPage
                            in
                            mdl4 |> withCmds [ cmd3, cmd4 ]


processInitialPage : Model -> ( Model, Cmd Msg )
processInitialPage model =
    let
        { page, request } =
            model.initialPage
    in
    { model
        | page =
            case page of
                Nothing ->
                    if request == Nothing then
                        model.page

                    else
                        ExplorerPage

                Just p ->
                    p
        , selectedRequest =
            case request of
                Nothing ->
                    model.selectedRequest

                Just r ->
                    r
    }
        |> withCmds
            [ if model.page == HomePage then
                focusId LoginServerId

              else
                Cmd.none
            , case model.renderEnv.loginServer of
                Just server ->
                    getFeedSetDefinition server

                _ ->
                    Cmd.none
            ]


processLoginServer : Model -> ( Model, Cmd Msg )
processLoginServer model =
    let
        mdl =
            { model
                | started = Started
                , msg = Nothing
            }
    in
    case mdl.renderEnv.loginServer of
        Nothing ->
            let
                foo =
                    Debug.log "No mdl.renderEnv.loginServer, mdl.server"
                        mdl.server
            in
            mdl
                |> withCmds
                    [ Task.perform (GlobalMsg << SetServer) <|
                        Task.succeed mdl.server
                    , getAccountIds mdl.server
                    ]

        Just server ->
            let
                foo =
                    Debug.log "Processing login for loginServer" server

                mdl2 =
                    { mdl
                        | maxTootCharsString =
                            Just <| String.fromInt mdl.max_toot_chars
                    }

                ( mdl3, cmd3 ) =
                    sendCustomEmojisRequest mdl2

                ( mdl4, cmd4 ) =
                    getVerifyCredentials mdl3

                ( mdl5, cmd5 ) =
                    sendRequest (ListsRequest Request.GetLists) mdl4
            in
            mdl5
                |> withCmds
                    [ cmd3
                    , cmd4
                    , cmd5
                    , fetchFeatures server mdl3
                    , getAccountIds server
                    ]


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
                            { emptyFeedEnv
                                | bodyEnv =
                                    { emptyFeedBodyEnv
                                        | group = Just group
                                        , feedId = feedId
                                    }
                            }
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
                            { emptyFeedEnv
                                | list = Just list
                                , bodyEnv =
                                    { emptyFeedBodyEnv
                                        | feedId = feedId
                                    }
                            }
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


encodeStreamingApi : StreamingApi -> Value
encodeStreamingApi api =
    case api of
        UnknownApi ->
            JE.null

        NoApi ->
            JE.string ""

        UrlApi url ->
            JE.string url


streamingApiDecoder : Decoder StreamingApi
streamingApiDecoder =
    JD.nullable JD.string
        |> JD.andThen
            (\s ->
                case s of
                    Nothing ->
                        JD.succeed UnknownApi

                    Just url ->
                        if url == "" then
                            JD.succeed NoApi

                        else
                            JD.succeed <| UrlApi url
            )


encodeTokenApi : TokenApi -> Value
encodeTokenApi { token, api } =
    JE.object
        [ ( "token", ED.encodeMaybe JE.string token )
        , ( "api", encodeStreamingApi api )
        ]


tokenApiDecoder : Decoder TokenApi
tokenApiDecoder =
    JD.oneOf
        [ JD.succeed TokenApi
            |> required "token" (JD.nullable JD.string)
            |> required "api" streamingApiDecoder
        , JD.string
            |> JD.andThen
                (\token ->
                    JD.succeed { emptyTokenApi | token = Just token }
                )
        ]


handleGetToken : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetToken key value model =
    case JD.decodeValue tokenApiDecoder value of
        Err err ->
            let
                ignore =
                    Debug.log ("Error decoding " ++ key) err
            in
            model |> withNoCmd

        Ok tokenApi ->
            let
                tokens =
                    model.tokens

                ( savedApi, cmd ) =
                    case tokenApi.api of
                        UnknownApi ->
                            ( Nothing, getInstance model )

                        UrlApi api ->
                            ( Just api, Cmd.none )

                        _ ->
                            ( Nothing, Cmd.none )

                server =
                    Debug.log "Received token for server" <|
                        tokenStorageKeyServer key

                streaming_api =
                    if
                        (savedApi /= Nothing)
                            && (model.renderEnv.loginServer == Just server)
                    then
                        savedApi

                    else
                        model.streaming_api
            in
            { model
                | tokens = Dict.insert server tokenApi tokens
                , streaming_api = streaming_api
            }
                |> withCmds [ cmd, fetchFeatures server model ]


pkAccountIdsLength : Int
pkAccountIdsLength =
    String.length pk.accountIds


pkFeedSetDefinitionLength : Int
pkFeedSetDefinitionLength =
    String.length pk.feedSetDefinition


pkMaxTootCharsLength : Int
pkMaxTootCharsLength =
    String.length pk.maxTootChars


handleGetApp : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetApp key value model =
    case JD.decodeValue ED.appDecoder value of
        Err err ->
            let
                ignore =
                    Debug.log ("Error Decoding " ++ key) err
            in
            { model | code = Nothing }
                |> withNoCmd

        Ok app ->
            case model.code of
                Just code ->
                    let
                        server =
                            appStorageKeyServer key

                        cmd =
                            Login.getTokenTask
                                { code = code, server = server, app = app }
                                |> Task.attempt (GlobalMsg << ReceiveAuthorization)
                    in
                    { model | code = Nothing }
                        |> withCmd cmd

                _ ->
                    model |> withNoCmd


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if Debug.log "handleGetResponse, key" key == pk.model then
                handleGetModel maybeValue model

            else if key == pk.dynamoDBAccount then
                handleGetDynamoDBAccount key maybeValue model

            else if pk.feedSetDefinition == String.left pkFeedSetDefinitionLength key then
                handleGetFeedSetDefinition maybeValue model

            else if pk.accountIds == String.left pkAccountIdsLength key then
                handleGetAccountIds key maybeValue model

            else if pk.maxTootChars == String.left pkMaxTootCharsLength key then
                handleGetMaxTootChars key maybeValue model

            else
                model |> withNoCmd

        Just label ->
            case maybeValue of
                Nothing ->
                    model |> withNoCmd

                Just value ->
                    if label == pk.token then
                        handleGetToken key value model

                    else if label == pk.timestamps then
                        handleGetTimestamps key value model

                    else if label == pk.timestamp then
                        -- Backward compatibility
                        handleGetTimestamp key value model

                    else if label == pk.app then
                        handleGetApp key value model

                    else
                        model |> withNoCmd


handleGetDynamoDBAccount : String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetDynamoDBAccount key maybeValue model =
    case maybeValue of
        Nothing ->
            model |> withNoCmd

        Just value ->
            case JD.decodeValue DynamoDB.accountDecoder value of
                Err err ->
                    let
                        ignore =
                            Debug.log ("Error decoding" ++ key) err
                    in
                    model |> withNoCmd

                Ok account ->
                    { model
                        | appStateAccount = account
                        , appState = AppState.mergeAccount account model.appState
                    }
                        |> withNoCmd


handleGetTimestamps : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetTimestamps key value model =
    case JD.decodeValue (JD.dict JD.string) value of
        Err err ->
            let
                ignore =
                    Debug.log ("Error decoding " ++ key) err
            in
            model |> withNoCmd

        Ok dict ->
            let
                server =
                    String.dropLeft (pkTimestampsLength + 1) key
            in
            { model
                | timestamps =
                    Dict.insert server dict model.timestamps
            }
                |> withNoCmd


{-| This is for the old timestamp storage.
Just delete the key/value pair in LocalStorage.
This causes timestamps to be forgotten at transition time,
but I doubt anyone will notice.
-}
handleGetTimestamp : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetTimestamp key value model =
    model |> withCmd (put key Nothing)


handleGetAccountIds : String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetAccountIds key maybeValue model =
    let
        server =
            String.dropLeft (pkAccountIdsLength + 1) key

        mergeEmpty mdl =
            mergeAccountId Types.emptyAccountId server mdl
    in
    case maybeValue of
        Nothing ->
            mergeEmpty model

        Just value ->
            case JD.decodeValue MED.accountIdsDecoder value of
                Err _ ->
                    mergeEmpty model

                Ok accountIds ->
                    let
                        newIds =
                            case Dict.get server model.accountIdDict of
                                Nothing ->
                                    accountIds

                                Just ids ->
                                    let
                                        remover aid list =
                                            LE.filterNot ((==) aid.id << .id) list
                                    in
                                    List.append ids <|
                                        List.foldr remover accountIds ids
                    in
                    mergeEmpty
                        { model
                            | accountIdDict =
                                Dict.insert server newIds model.accountIdDict
                        }


handleGetMaxTootChars : String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetMaxTootChars key maybeValue model =
    case maybeValue of
        Nothing ->
            model |> withNoCmd

        Just value ->
            let
                server =
                    String.dropLeft (pkMaxTootCharsLength + 1) key
            in
            case JD.decodeValue JD.string value of
                Err _ ->
                    model |> withNoCmd

                Ok maxTootCharsString ->
                    let
                        max_toot_chars =
                            if Just server /= model.renderEnv.loginServer then
                                model.max_toot_chars

                            else
                                case String.toInt maxTootCharsString of
                                    Just mtc ->
                                        mtc

                                    Nothing ->
                                        model.max_toot_chars
                    in
                    { model
                        | maxTootCharsString = Just maxTootCharsString
                        , max_toot_chars = max_toot_chars
                    }
                        |> withNoCmd


webSocketFeedTypes : String -> List FeedType
webSocketFeedTypes feedId =
    case Types.feedIdToType feedId of
        Nothing ->
            []

        Just feedType ->
            case feedType of
                HomeFeed ->
                    [ HomeFeed, Types.defaultNotificationFeedType ]

                _ ->
                    [ feedType ]


addWebSocketFeed : String -> Model -> Model
addWebSocketFeed feedId model =
    let
        enableUndisplayed : FeedType -> FeedSet -> FeedSet
        enableUndisplayed feedType feedSet =
            case findFeed feedType feedSet of
                Nothing ->
                    feedSet

                Just feed ->
                    let
                        newFeed =
                            case feed.undisplayedElements of
                                NeverUndisplayed ->
                                    { feed | undisplayedElements = NoUndisplayed }

                                _ ->
                                    feed
                    in
                    replaceFeed newFeed feedSet
    in
    { model
        | webSocketFeeds =
            Set.insert feedId model.webSocketFeeds
        , feedSet =
            List.foldr enableUndisplayed
                model.feedSet
            <|
                webSocketFeedTypes feedId
    }


removeWebSocketFeed : String -> Model -> Model
removeWebSocketFeed feedId model =
    let
        disableUndisplayed feedType feedSet =
            case findFeed feedType feedSet of
                Nothing ->
                    feedSet

                Just feed ->
                    let
                        newFeed =
                            { feed | undisplayedElements = NeverUndisplayed }
                    in
                    replaceFeed newFeed feedSet
    in
    { model
        | webSocketFeeds =
            Set.remove feedId model.webSocketFeeds
        , feedSet =
            List.foldr disableUndisplayed
                model.feedSet
            <|
                webSocketFeedTypes feedId
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

                WebSocket.SocketConnectingError key ->
                    removeWebSocketFeed key model
                        |> withNoCmd

                _ ->
                    { model | msg = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse { key, message } ->
            webSocketMessageReceived key message model

        WebSocket.ClosedResponse { key } ->
            removeWebSocketFeed key model
                |> withNoCmd

        WebSocket.ConnectedResponse _ ->
            model |> withNoCmd

        _ ->
            model |> withNoCmd


webSocketMessageReceived : String -> String -> Model -> ( Model, Cmd Msg )
webSocketMessageReceived key message model =
    case Mastodon.WebSocket.decodeEvent message of
        Err _ ->
            model |> withNoCmd

        Ok event ->
            case event of
                UpdateEvent status ->
                    webSocketUpdate key status model

                DeleteEvent status ->
                    -- TODO
                    model |> withNoCmd

                NotificationEvent notification ->
                    webSocketNotification key notification model

                ClosedEvent string ->
                    removeWebSocketFeed string model
                        |> withNoCmd

                _ ->
                    model |> withNoCmd


statusSatisfiesFeedParams : FeedType -> Status -> Bool
statusSatisfiesFeedParams feedType status =
    let
        ( local, only_media ) =
            case feedType of
                PublicFeed params ->
                    case params.flags of
                        Nothing ->
                            ( True, False )

                        Just p ->
                            ( p.local, p.only_media )

                ProFeed params ->
                    case params.flags of
                        Nothing ->
                            ( False, False )

                        Just p ->
                            ( False, p.only_media )

                -- UserFeed has parameters, but there's no stream for that
                _ ->
                    ( False, False )
    in
    (not local || (not <| String.contains "@" status.account.acct))
        && (not only_media || [] /= status.media_attachments)


webSocketUpdate : String -> Status -> Model -> ( Model, Cmd Msg )
webSocketUpdate key status model =
    case Types.feedIdToType key of
        Nothing ->
            model |> withNoCmd

        Just feedType ->
            if not <| statusSatisfiesFeedParams feedType status then
                model |> withNoCmd

            else
                let
                    feedSet =
                        model.feedSet
                in
                case findFeed feedType feedSet of
                    Nothing ->
                        model |> withNoCmd

                    Just feed ->
                        let
                            newFeed =
                                case feed.undisplayedElements of
                                    NoUndisplayed ->
                                        { feed
                                            | undisplayedElements =
                                                Undisplayed <| StatusElements [ status ]
                                        }

                                    Undisplayed (StatusElements statuses) ->
                                        let
                                            new =
                                                (status :: statuses)
                                                    |> List.take maxFeedLength
                                        in
                                        { feed
                                            | undisplayedElements =
                                                Undisplayed <| StatusElements new
                                        }

                                    _ ->
                                        feed
                        in
                        { model
                            | feedSet =
                                replaceFeed newFeed feedSet
                        }
                            |> updateNewElementsLeftRight
                            |> withNoCmd


webSocketNotification : String -> Notification -> Model -> ( Model, Cmd Msg )
webSocketNotification key notification model =
    if key /= "home" then
        model |> withNoCmd

    else
        case Types.feedIdToType "notifications" of
            Nothing ->
                model |> withNoCmd

            Just feedType ->
                -- TODO: add notificationSatisfiesFeedParams if there ever are any.
                let
                    feedSet =
                        model.feedSet
                in
                case findFeed feedType feedSet of
                    Nothing ->
                        model |> withNoCmd

                    Just feed ->
                        let
                            newFeed =
                                case feed.undisplayedElements of
                                    NoUndisplayed ->
                                        { feed
                                            | undisplayedElements =
                                                Undisplayed <| NotificationElements [ notification ]
                                        }

                                    Undisplayed (NotificationElements notifications) ->
                                        let
                                            new =
                                                (notification :: notifications)
                                                    |> List.take maxFeedLength
                                        in
                                        { feed
                                            | undisplayedElements =
                                                Undisplayed <| NotificationElements new
                                        }

                                    _ ->
                                        feed
                        in
                        { model
                            | feedSet =
                                replaceFeed newFeed feedSet
                        }
                            |> withNoCmd


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


openExternalUrl : String -> Cmd Msg
openExternalUrl url =
    Task.perform OnUrlRequest <| Task.succeed (External url)


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    let
        renderEnv =
            model.renderEnv
    in
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest urlRequest ->
            case Debug.log "OnUrlRequest" urlRequest of
                External url ->
                    model |> withCmd (openWindow <| JE.string url)

                Internal url ->
                    case url.fragment of
                        Nothing ->
                            model |> withNoCmd

                        Just fragment ->
                            -- This is created by `replaceMentionLinks`
                            -- It assumes there are no periods in a user ID.
                            if String.startsWith "mentionDialog." fragment then
                                let
                                    idAndUrl =
                                        Debug.log "idAndUrl" <|
                                            (String.dropLeft 14 fragment
                                                |> String.split "."
                                            )
                                in
                                case idAndUrl of
                                    [] ->
                                        model |> withNoCmd

                                    id :: rest ->
                                        let
                                            mention =
                                                { url = String.join "." rest
                                                , username = ""
                                                , acct = ""
                                                , id = id
                                                }
                                        in
                                        model
                                            |> withCmd
                                                (Task.perform ColumnsUIMsg <|
                                                    Task.succeed (ShowMentionDialog mention)
                                                )

                            else
                                model |> withNoCmd

        OnUrlChange url ->
            let
                url2 =
                    Debug.log "OnUrlChange" url
            in
            case url2.fragment of
                Nothing ->
                    model |> withNoCmd

                Just fragment ->
                    if String.startsWith "help." fragment then
                        let
                            name =
                                String.dropLeft 5 fragment
                        in
                        case LE.find (\( _, _, dn ) -> dn == name) docSections of
                            Nothing ->
                                model |> withNoCmd

                            Just ( section, _, _ ) ->
                                { model
                                    | dialog = DocsDialog
                                    , docSection = section
                                }
                                    |> withNoCmd

                    else
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

        ProcessLinkInfo { selectedRequest, setId, message } ->
            { model
                | page = ExplorerPage
                , dialog = NoDialog
                , selectedRequest = selectedRequest
            }
                |> setId
                |> withCmd (Task.perform ExplorerSendMsg <| Task.succeed message)

        ScrollNotify value ->
            processScroll value model

        FocusNotify focused ->
            (if focused then
                { model | keysDown = Set.empty }

             else
                model
            )
                |> withNoCmd

        IdleNotify idleTime ->
            { model | idleTime = idleTime }
                |> withNoCmd

        ApplyToModel f ->
            f model

        BoundingBoxRequest elementId ->
            { model | boundingBox = Nothing }
                |> withCmd (boundingBoxRequest elementId)

        BoundingBoxNotify boundingBox ->
            { model
                | boundingBox =
                    Debug.log "BoundingBoxNotify" <|
                        Just boundingBox
            }
                |> withNoCmd


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
                let
                    mdl =
                        { model
                            | bodyScroll = notification
                        }

                    scrollColumns =
                        columnScrollInfo mdl
                            |> computeScrollColumns

                    mdl2 =
                        if scrollColumns == mdl.scrollColumns then
                            mdl

                        else
                            { mdl | scrollColumns = scrollColumns }
                                |> updateNewElementsLeftRight
                in
                mdl2
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
        OnResize w h ->
            -- Work around a bug in iOS. h is sent as w.
            -- Calling WindowResize directly doesn't work right.
            model |> withCmd (Task.perform getViewport Dom.getViewport)

        WindowResize w h ->
            { model
                | renderEnv =
                    { renderEnv | windowSize = Debug.log "windowSize" ( w, h ) }
            }
                |> withNoCmd

        SetColorScheme colorScheme ->
            { model
                | renderEnv =
                    { renderEnv
                        | colorScheme = Just colorScheme
                    }
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

        SetMaxTootChars maxTootCharsString ->
            { model | maxTootCharsString = Just maxTootCharsString }
                |> withNoCmd

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

        SetDocSection string ->
            case LE.find (\( section, name, _ ) -> name == string) docSections of
                Nothing ->
                    model |> withNoCmd

                Just ( section, _, _ ) ->
                    { model | docSection = section }
                        |> withNoCmd

        OnKeyPress isDown key ->
            let
                k =
                    Debug.log "OnKeyPress" key

                mdl =
                    { model
                        | keysDown =
                            if isDown then
                                Set.insert key model.keysDown

                            else
                                Set.remove key model.keysDown
                    }

                isDialog =
                    case model.dialog of
                        NoDialog ->
                            False

                        _ ->
                            True
            in
            mdl
                |> withCmd
                    (if
                        not isDown
                            || ((key /= keyboard.escape)
                                    && (isSpecialKeyDown mdl || isDialog)
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

        OnMouseClick ( x, y ) ->
            { model | clickPosition = ( x, y ) }
                |> withNoCmd

        SetServer server ->
            let
                mdl =
                    { model
                        | server = server
                        , maxTootCharsString = Nothing
                    }
            in
            mdl
                |> withCmd
                    (if String.contains "." server then
                        Cmd.batch
                            [ getInstance mdl
                            , getMaxTootChars server
                            ]

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
                        { renderEnv
                            | loginServer = Nothing
                            , accountId = ""
                        }
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
                                { renderEnv
                                    | loginServer = Just server

                                    -- This is set in ReceiveGetVerifyCredentials
                                    , accountId = ""
                                }
                            , token = Nothing
                            , streaming_api = Nothing
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

        SetTokenText text ->
            { model | tokenText = text } |> withNoCmd

        SetTokenFromText ->
            let
                server =
                    model.server

                tokenApi =
                    case Dict.get server model.tokens of
                        Just api ->
                            { api | token = Just model.tokenText }

                        Nothing ->
                            { emptyTokenApi | token = Just model.tokenText }

                mdl =
                    { model
                        | tokens = Dict.insert server tokenApi model.tokens
                        , tokenText = ""
                        , renderEnv =
                            { renderEnv
                                | loginServer = Just model.server

                                -- This is set in ReceiveGetVerifyCredentials
                                , accountId = ""
                            }
                    }

                cmd =
                    putToken server (Just tokenApi)
            in
            mdl |> withCmd cmd

        LoginServer server ->
            globalMsg Login { model | server = server }

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

                token =
                    case Dict.get model.server model.tokens of
                        Just tokenApi ->
                            tokenApi.token

                        _ ->
                            Nothing

                putMaxTootCharsCmd =
                    putMaxTootChars model.server model.maxTootCharsString
            in
            case Login.loginTask sau token of
                Redirect task ->
                    mdl
                        |> withCmds
                            [ Task.attempt (GlobalMsg << ReceiveRedirect) task
                            , putMaxTootCharsCmd
                            ]

                FetchAccount task ->
                    mdl
                        |> withCmds
                            [ Task.attempt (GlobalMsg << ReceiveFetchAccount) task
                            , putMaxTootCharsCmd
                            ]

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
                        , streaming_api = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , metadata = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
                        |> withCmds
                            [ putToken server Nothing

                            -- Maybe should keep this and use it at next login
                            -- instead of minting a new one.
                            , putApp server Nothing
                            ]

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
                            { renderEnv
                                | loginServer = Nothing
                                , accountId = ""
                            }
                        , account = Nothing
                        , token = Nothing
                        , streaming_api = Nothing
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
                        |> withCmds
                            [ cmd
                            , putApp server <| Just app
                            , fetchFeatures server model
                            ]

        ReceiveAuthorization result ->
            case result of
                -- Maybe we should also call processLoginServer here,
                -- but I don't expect this to happen,
                -- and loginServer would likely not be set.
                Err ( server, err ) ->
                    { model | msg = Just <| Debug.toString err }
                        |> processInitialPage

                Ok ( server, authorization, account ) ->
                    let
                        ( streaming_api, mdl, cmd ) =
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
                                , streaming_api = streaming_api
                                , renderEnv =
                                    { renderEnv
                                        | loginServer = Just server
                                        , accountId = account.id
                                    }
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
                            , getMaxTootChars server
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
                                    { renderEnv
                                        | loginServer = Just loginServer
                                        , accountId = account.id
                                    }
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
                            , getMaxTootChars loginServer
                            ]

        ReceiveInstance server result ->
            case result of
                Err _ ->
                    -- We'll get lots of errors, for non-existant domains
                    model |> withNoCmd

                Ok response ->
                    receiveInstance server response model

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
                                    case renderEnv.loginServer of
                                        Nothing ->
                                            ( mdl, Cmd.none )

                                        Just server ->
                                            mergeAccountId accountId
                                                server
                                                { mdl
                                                    | renderEnv =
                                                        { renderEnv
                                                            | accountId = account.id
                                                        }
                                                }
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


verifyCredentialsRepeatCnt : Int
verifyCredentialsRepeatCnt =
    5


verifyCredentialsRetryDelay : Int -> Float
verifyCredentialsRetryDelay cnt =
    10 * (toFloat cnt ^ 2)


receiveInstance : String -> Response -> Model -> ( Model, Cmd Msg )
receiveInstance server response model =
    case response.entity of
        InstanceEntity instance ->
            let
                tokens =
                    model.tokens

                ( maybeStreamingApi, newTokens, cmd ) =
                    case Dict.get server tokens of
                        Nothing ->
                            ( model.streaming_api
                            , tokens
                            , Cmd.none
                            )

                        Just tokenApi ->
                            case instance.urls of
                                Nothing ->
                                    ( model.streaming_api, tokens, Cmd.none )

                                Just { streaming_api } ->
                                    let
                                        newTokenApi =
                                            { tokenApi | api = UrlApi streaming_api }

                                        isServer =
                                            Just server == model.renderEnv.loginServer
                                    in
                                    ( if isServer then
                                        Just streaming_api

                                      else
                                        model.streaming_api
                                    , Dict.insert server newTokenApi tokens
                                    , putToken server <| Just newTokenApi
                                    )
            in
            { model
                | msg = Nothing
                , streaming_api = maybeStreamingApi
                , lastInstance = Just { server = server, instance = instance }
                , tokens = newTokens
                , request = Just response.rawRequest
                , metadata = Just response.metadata
                , response = Just instance.v
                , entity = Just response.entity
                , maxTootCharsString =
                    if model.maxTootCharsString /= Nothing then
                        model.maxTootCharsString

                    else
                        case instance.max_toot_chars of
                            Nothing ->
                                model.maxTootCharsString

                            Just max_toot_chars ->
                                Just <| String.fromInt max_toot_chars
            }
                |> updateJsonTrees
                |> withCmd cmd

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


{-| Fetch features if we don't know them already.
-}
fetchFeatures : String -> Model -> Cmd Msg
fetchFeatures server model =
    let
        cmd1 =
            if Just server /= model.renderEnv.loginServer then
                Cmd.none

            else
                Task.perform ExplorerSendMsg <|
                    Task.succeed SendGetInstance
    in
    case LE.find (\( s, _ ) -> s == server) model.featureProbeRequests of
        Just _ ->
            Cmd.none

        Nothing ->
            case serverKnowsFeature (Just server) featureNames.groups model.renderEnv of
                Just _ ->
                    cmd1

                Nothing ->
                    Cmd.batch
                        [ cmd1
                        , Task.perform ColumnsUIMsg
                            (Task.succeed <|
                                ProbeGroupsFeature server
                            )
                        ]


{-| Merge account into server's accountIdDict entry.

Write if it changed. Read if it started empty.

-}
mergeAccountId : AccountId -> String -> Model -> ( Model, Cmd Msg )
mergeAccountId accountId server model =
    let
        id =
            accountId.id
    in
    let
        accountIds =
            Maybe.withDefault [] <| Dict.get server model.accountIdDict

        putNew acctIds =
            { model
                | accountIdDict =
                    Dict.insert server acctIds model.accountIdDict
            }
                |> withCmd
                    (if List.member Types.emptyAccountId acctIds then
                        putAccountIds server acctIds

                     else
                        -- Wait for reading of accountIds
                        Cmd.none
                    )
    in
    case LE.find (.id >> (==) id) accountIds of
        Nothing ->
            putNew <| accountId :: accountIds

        Just acctId ->
            if acctId /= accountId then
                putNew <| accountId :: LE.remove acctId accountIds

            else
                model |> withNoCmd


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


processAppStateSave : Model -> Maybe ( AppState, Task AppState.Error Int ) -> ( Model, Cmd Msg )
processAppStateSave model result =
    case result of
        Nothing ->
            model |> withNoCmd

        Just ( appState, task ) ->
            ( { model | appState = Debug.log "processAppStateSave" appState }
            , Task.attempt (ColumnsUIMsg << AppStateSaved) task
            )


processAppStateUpdate : Model -> Maybe ( AppState, Task AppState.Error (Maybe Updates) ) -> ( Model, Cmd Msg )
processAppStateUpdate model maybeStuff =
    case maybeStuff of
        Nothing ->
            model |> withNoCmd

        Just ( appState, task ) ->
            ( { model | appState = Debug.log "processAppStateUpdate" appState }
            , Task.attempt (ColumnsUIMsg << AppStateUpdated) task
            )


setDialog : Model -> String -> Dialog -> ( Model, Cmd Msg )
setDialog model nodeId dialog =
    { model | dialog = dialog }
        |> withCmd (boundingBoxRequest nodeId)


setAccountDialog : Model -> Dialog -> ( Model, Cmd Msg )
setAccountDialog model dialog =
    setDialog model nodeIds.accountDialog dialog


setStatusHistoryDialog : Model -> Dialog -> ( Model, Cmd Msg )
setStatusHistoryDialog model dialog =
    setDialog model nodeIds.statusHistoryDialog dialog


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
                    -- subtracted 4 because that works.
                    (renderEnv.windowSize |> Tuple.first) - 6 - leftColWid |> toFloat

                colMargin =
                    -- Determined by experiment
                    0 |> toFloat

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

        SetStyle string ->
            let
                style =
                    case labelToStyle string of
                        Nothing ->
                            SystemStyle

                        Just s ->
                            s
            in
            { model
                | renderEnv =
                    { renderEnv | style = style }
            }
                |> withNoCmd

        ToggleStyle ->
            { model
                | renderEnv =
                    { renderEnv
                        | style =
                            case renderEnv.style of
                                LightStyle ->
                                    DarkStyle

                                DarkStyle ->
                                    LightStyle

                                SystemStyle ->
                                    case renderEnv.colorScheme of
                                        Nothing ->
                                            DarkStyle

                                        Just LightColorScheme ->
                                            DarkStyle

                                        Just DarkColorScheme ->
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

                mdl4 =
                    List.foldl showUndisplayedFeed mdl3 mdl3.feedSet.feeds

                ( mdl5, cmd5 ) =
                    List.foldl getFeed ( mdl4, Cmd.none ) mdl4.feedSet.feeds
            in
            mdl5 |> withCmds [ cmd3, cmd5 ]

        MarkFeedRead feedType ->
            markFeedRead feedType model

        ShowUndisplayed feedType ->
            showUndisplayed feedType model

        ShowAllUndisplayed ->
            let
                showOne : FeedType -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                showOne feedType ( mdl, cmd ) =
                    let
                        ( mdl2, cmd2 ) =
                            showUndisplayed feedType mdl
                    in
                    mdl2 |> withCmds [ cmd, cmd2 ]
            in
            List.foldl showOne ( model, Cmd.none ) model.feedSetDefinition.feedTypes

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

                ( mdl, cmd ) =
                    if AppState.accountIncomplete model.appState then
                        model |> withNoCmd

                    else
                        case AppState.idle millis model.appState of
                            Nothing ->
                                if model.idleTime >= 10000 then
                                    model |> withNoCmd

                                else
                                    AppState.update millis model.appState
                                        |> processAppStateUpdate model

                            res ->
                                processAppStateSave model res

                mdl2 =
                    maybeUpdateBodyEnvNow now mdl
            in
            { mdl2 | now = now }
                |> withCmd cmd

        ShowEditColumnsDialog ->
            { model | dialog = EditColumnsDialog }
                |> sendRequest (ListsRequest Request.GetLists)

        ShowServerDialog ->
            { model | dialog = ServerDialog }
                |> withCmd (focusId LoginServerId)

        TimestampedCmd wrapper now ->
            columnsUIMsg (wrapper now) model

        ScrollToNew direction feedType ->
            model |> withCmd (scrollToNew direction feedType model)

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

        ReloadFromServer ->
            model |> withCmd Navigation.reloadAndSkipCache

        ClearFeatures ->
            { model
                | renderEnv =
                    { renderEnv | features = Dict.empty }
            }
                |> withNoCmd

        ShowDocsDialog ->
            { model | dialog = DocsDialog }
                |> withNoCmd

        ShowUserDialog { username, server } ->
            case renderEnv.loginServer of
                Nothing ->
                    model |> withNoCmd

                Just loginServer ->
                    let
                        realServer =
                            if server /= "" then
                                server

                            else
                                loginServer

                        acct =
                            usernameAtServer username server renderEnv
                    in
                    case Dict.get loginServer model.accountIdDict of
                        Nothing ->
                            model |> withNoCmd

                        Just accountIds ->
                            case LE.find (\accountId -> acct == accountId.username) accountIds of
                                Nothing ->
                                    model |> withNoCmd

                                Just { id, url } ->
                                    let
                                        mention =
                                            { url = url
                                            , username = username
                                            , acct = acct
                                            , id = id
                                            }
                                    in
                                    columnsUIMsg (ShowMentionDialog mention) model

        ShowMentionDialog mention ->
            model
                |> withCmd
                    (Request.serverRequest
                        (\id res ->
                            ColumnsUIMsg <| ReceiveAccountDialogAccount id res
                        )
                        []
                        { server = Maybe.withDefault "" <| renderEnv.loginServer
                        , token = model.token
                        }
                        mention
                        (AccountsRequest <| Request.GetAccount { id = mention.id })
                    )

        ReceiveAccountDialogAccount mention result ->
            let
                openExternal () =
                    model
                        |> withCmd (openExternalUrl mention.url)
            in
            case result of
                Err _ ->
                    openExternal ()

                Ok { entity } ->
                    case entity of
                        AccountEntity account ->
                            let
                                ( mdl, cmd ) =
                                    processReceivedAccount account model
                            in
                            update (ColumnsUIMsg <| ShowAccountDialog account) mdl

                        _ ->
                            openExternal ()

        ShowAccountDialog account ->
            let
                ( mdl, cmd ) =
                    setAccountDialog model <|
                        AccountDialog account Nothing

                myAccount =
                    case mdl.account of
                        Just acc ->
                            account.id == acc.id

                        Nothing ->
                            False
            in
            if myAccount then
                mdl |> withCmd cmd

            else
                let
                    ( mdl2, cmd2 ) =
                        sendRequest
                            (AccountsRequest <|
                                Request.GetAccount { id = account.id }
                            )
                            mdl

                    ( mdl3, cmd3 ) =
                        sendRequest
                            (AccountsRequest <|
                                Request.GetRelationships { ids = [ account.id ] }
                            )
                            mdl2
                in
                mdl3 |> withCmds [ cmd, cmd2, cmd3 ]

        ToggleShowAccountDialogId ->
            { model
                | renderEnv =
                    { renderEnv | showIds = not renderEnv.showIds }
            }
                |> withNoCmd

        AccountDialogCommand options name ->
            case LE.find (\( n, _ ) -> name == n) options of
                Nothing ->
                    model |> withNoCmd

                Just ( _, m ) ->
                    model
                        |> withCmds
                            [ Task.perform ColumnsUIMsg <| Task.succeed m
                            , resetSelectOption nodeIds.accountDialogSelect
                            ]

        AccountDialogSetShowStatuses show ->
            case model.dialog of
                AccountDialog account maybeContent ->
                    if not show then
                        setAccountDialog model <|
                            AccountDialog account Nothing

                    else
                        case maybeContent of
                            Just (StatusesContent _) ->
                                model |> withNoCmd

                            _ ->
                                let
                                    ( mdl, cmd ) =
                                        setAccountDialog model
                                            (AccountDialog account <|
                                                Just
                                                    (StatusesContent
                                                        { flags = model.accountDialogFlags
                                                        , statuses = []
                                                        }
                                                    )
                                            )

                                    ( mdl2, cmd2 ) =
                                        sendRequest
                                            (getStatusesRequestFromUserFeedFlags account.id
                                                Nothing
                                                model.accountDialogFlags
                                            )
                                            mdl
                                in
                                mdl2 |> withCmds [ cmd, cmd2 ]

                _ ->
                    model |> withNoCmd

        AccountDialogShowFollowing ->
            case model.dialog of
                AccountDialog account maybeContent ->
                    case maybeContent of
                        Just (FollowingContent _) ->
                            model |> withNoCmd

                        _ ->
                            let
                                ( mdl, cmd ) =
                                    setAccountDialog model
                                        (AccountDialog account <|
                                            Just (FollowingContent [])
                                        )

                                ( mdl2, cmd2 ) =
                                    sendGetFollowing account.id
                                        (Just accountDialogRelationsCount)
                                        mdl
                            in
                            mdl2 |> withCmds [ cmd, cmd2 ]

                _ ->
                    model |> withNoCmd

        AccountDialogShowFollowers ->
            case model.dialog of
                AccountDialog account maybeContent ->
                    case maybeContent of
                        Just (FollowersContent _) ->
                            model |> withNoCmd

                        _ ->
                            let
                                ( mdl, cmd ) =
                                    setAccountDialog model
                                        (AccountDialog account <|
                                            Just (FollowersContent [])
                                        )

                                ( mdl2, cmd2 ) =
                                    sendGetFollowers account.id
                                        (Just accountDialogRelationsCount)
                                        mdl
                            in
                            mdl2 |> withCmds [ cmd, cmd2 ]

                _ ->
                    model |> withNoCmd

        AccountDialogSetFlags flags ->
            let
                mdl =
                    { model | accountDialogFlags = flags }
            in
            case mdl.dialog of
                AccountDialog account (Just content) ->
                    case content of
                        StatusesContent statuses ->
                            sendRequest
                                (getStatusesRequestFromUserFeedFlags account.id
                                    Nothing
                                    flags
                                )
                                { mdl
                                    | dialog =
                                        AccountDialog account <|
                                            Just
                                                (StatusesContent
                                                    { statuses
                                                        | flags = flags
                                                        , statuses = []
                                                    }
                                                )
                                }

                        _ ->
                            mdl |> withNoCmd

                _ ->
                    mdl |> withNoCmd

        AccountDialogShowHeader account ->
            showImageUrl account.header model

        ShowImage url ->
            showImageUrl url model

        ToggleFollowAccount following account ->
            sendRequest
                (AccountsRequest <|
                    if following then
                        Request.PostUnfollow { id = account.id }

                    else
                        Request.PostFollow { id = account.id, reblogs = True }
                )
                model

        FetchRelationships accounts ->
            fetchRelationships accounts model

        MuteAccount muting account ->
            sendRequest
                (MutesRequest <|
                    if muting then
                        Request.PostAccountUnmute { id = account.id }

                    else
                        Request.PostAccountMute
                            { id = account.id, notifications = True }
                )
                model

        BlockAccount blocking account ->
            sendRequest
                (BlocksRequest <|
                    if blocking then
                        Request.PostUnblock { id = account.id }

                    else
                        Request.PostBlock { id = account.id }
                )
                model

        ShowPostDialog maybeStatus ->
            let
                postState =
                    let
                        ps =
                            model.postState
                    in
                    if serverSupportsMarkdown renderEnv.loginServer renderEnv then
                        ps

                    else
                        { ps | content_type = plainTextContentType }

                me =
                    case model.account of
                        Nothing ->
                            ""

                        Just account ->
                            account.username

                ( ( replyTo, replyType, visibility ), ( groupName, group_id ) ) =
                    case maybeStatus of
                        Nothing ->
                            ( ( postState.replyTo
                              , postState.replyType
                              , if postState.replyTo == Nothing then
                                    postState.setVisibility

                                else
                                    postState.visibility
                              )
                            , ( postState.groupName, postState.group_id )
                            )

                        Just status ->
                            ( ( maybeStatus, ReplyToPost, status.visibility )
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
                        , visibility = visibility
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

        ShowAttachmentDialog index status ->
            { model
                | attachmentView =
                    Just
                        { attachments = status.media_attachments
                        , index = index
                        }
            }
                |> withCmd (stopVideosForAttachment index status.media_attachments)

        ShowSettingsDialog ->
            { model | dialog = SettingsDialog }
                |> withNoCmd

        ShowDynamoDBDialog ->
            { model | dialog = DynamoDBDialog }
                |> withNoCmd

        ShowKeyboardShortcutsDialog ->
            { model | dialog = KeyboardShortcutsDialog }
                |> withNoCmd

        ShowSaveRestoreDialog ->
            { model | dialog = SaveRestoreDialog }
                |> withNoCmd

        YesImSure reason status ->
            yesImSure reason status model

        DismissDialog ->
            dismissDialog model

        DismissAttachmentView ->
            { model | attachmentView = Nothing }
                |> withNoCmd

        AddFeedType feedType ->
            addFeedType feedType model

        DeleteFeedType feedType ->
            deleteFeedType feedType model

        SetUserColumnFlags flags ->
            { model | userColumnFlags = flags }
                |> withNoCmd

        SetPublicColumnFlags flags ->
            { model | publicColumnFlags = flags }
                |> withNoCmd

        SetNotificationColumnParams params ->
            { model | notificationColumnParams = params }
                |> withCmd (resetSelectOption nodeIds.notificationTypeSelect)

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

        UpdateFeedColumn feedType ->
            updateFeedColumn feedType model

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
                            makeFeedEnv id

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

        StatusEllipsisPopup ellipsisId status ->
            showEllipsisPopup ellipsisId status model

        ShowFeedTypePopup feedType ->
            let
                dialog =
                    case model.dialog of
                        FeedTypeDialog ft ->
                            NoDialog

                        _ ->
                            FeedTypeDialog feedType
            in
            { model | dialog = dialog }
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

        SetPostVisibility string ->
            let
                postState =
                    model.postState

                visibility =
                    case string of
                        "unlisted" ->
                            UnlistedVisibility

                        "private" ->
                            PrivateVisibility

                        "direct" ->
                            DirectVisibility

                        _ ->
                            PublicVisibility
            in
            { model
                | postState =
                    { postState
                        | visibility = visibility
                        , setVisibility = visibility
                    }
            }
                |> withNoCmd

        PostCommand command ->
            let
                postState =
                    model.postState

                newPostState =
                    if command == postCommand.poll then
                        case postState.pollDefinition of
                            Nothing ->
                                { postState
                                    | pollDefinition = Just defaultPollDefinition
                                    , daysHoursMinutes = defaultDaysHoursMinutes
                                }

                            Just _ ->
                                { postState
                                    | pollDefinition = Nothing
                                    , daysHoursMinutes = emptyDaysHoursMinutes
                                }

                    else
                        postState
            in
            { model | postState = newPostState }
                |> withNoCmd

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

        IncrementAttachmentIndex delta ->
            case model.attachmentView of
                Nothing ->
                    model |> withNoCmd

                Just attachmentView ->
                    let
                        index =
                            attachmentView.index + delta
                    in
                    { model
                        | attachmentView =
                            Just { attachmentView | index = index }
                    }
                        |> withCmd
                            (stopVideosForAttachment index
                                attachmentView.attachments
                            )

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
                    clearPostStateReplyTo model.postState
            in
            { model | postState = postState }
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

                                EditPost s ->
                                    ( Just s.id, Nothing )

                                _ ->
                                    ( Nothing, Just status.id )

                ( poll, pollValid ) =
                    case postState.pollDefinition of
                        Nothing ->
                            ( Nothing, True )

                        Just pollDef ->
                            case daysHoursMinutesToSeconds postState.daysHoursMinutes of
                                Nothing ->
                                    ( Nothing, False )

                                Just seconds ->
                                    ( Just { pollDef | expires_in = seconds }
                                    , True
                                    )
            in
            if not pollValid then
                { model | msg = Just "Poll expiration invalid" }
                    |> withNoCmd

            else
                let
                    mdl =
                        { model | postState = { postState | posting = True } }
                in
                case replyType of
                    EditPost s ->
                        let
                            edited =
                                { status = nothingIfBlank postState.text
                                , in_reply_to_id = in_reply_to_id
                                , quote_of_id = quote_id
                                , media_ids = Just postState.media_ids
                                , sensitive = postState.sensitive
                                , spoiler_text = Nothing
                                , visibility = Just postState.visibility
                                , content_type = Just postState.content_type
                                , poll = poll
                                , scheduled_at = Nothing
                                }
                        in
                        sendPostRequest
                            (StatusesRequest <|
                                Request.PutStatus
                                    { id = s.id
                                    , status = edited
                                    }
                            )
                            mdl

                    _ ->
                        let
                            post =
                                { status = nothingIfBlank postState.text
                                , in_reply_to_id = in_reply_to_id
                                , group_id = postState.group_id
                                , quote_of_id = quote_id
                                , media_ids = postState.media_ids
                                , poll = poll
                                , sensitive = postState.sensitive
                                , content_type = Just postState.content_type
                                , spoiler_text = Nothing
                                , visibility = Just postState.visibility
                                , scheduled_at = Nothing
                                , language = Nothing
                                , idempotencyKey = Nothing
                                }
                        in
                        sendPostRequest
                            (StatusesRequest <| Request.PostStatus post)
                            mdl

        ClearPostState ->
            clearPostState model
                |> withNoCmd

        ProbeGroupsFeature server ->
            probeGroupsFeature server model

        ToggleShowLeftColumn ->
            let
                mdl =
                    { model | showLeftColumn = not model.showLeftColumn }

                mdl2 =
                    { mdl
                        | scrollColumns =
                            columnScrollInfo mdl |> computeScrollColumns
                    }
                        |> updateNewElementsLeftRight
            in
            mdl2 |> withNoCmd

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

        SetAppStateAccount account ->
            { model
                | appState = AppState.mergeAccount account model.appState
                , appStateAccount = account
            }
                |> withNoCmd

        CommitDynamoDBDialog ->
            commitDynamoDBDialog model

        DynamoDBSave key value ->
            let
                appState =
                    model.appState
            in
            if
                (key == pk.dynamoDBAccount)
                    || model.appStateUpdating
                    || AppState.accountIncomplete appState
            then
                model |> withNoCmd

            else
                AppState.save (Time.posixToMillis model.now)
                    key
                    value
                    appState
                    |> processAppStateSave model

        AppStateSaved result ->
            case result of
                Err err ->
                    let
                        e =
                            Debug.log "AppStateSaved" err
                    in
                    model |> withNoCmd

                Ok count ->
                    let
                        s =
                            Debug.log "AppStateSaved OK" count
                    in
                    model |> withNoCmd

        AppStateUpdated result ->
            case result of
                Err err ->
                    { model | msg = Just <| Debug.toString err }
                        |> withNoCmd

                Ok maybeUpdates ->
                    case maybeUpdates of
                        Nothing ->
                            model |> withNoCmd

                        Just updates ->
                            appStateUpdated model updates

        AppStateUpdateDone doNow ->
            if doNow then
                { model | appStateUpdating = False }
                    |> withNoCmd

            else
                model
                    |> withCmd
                        (Task.perform (ColumnsUIMsg << AppStateUpdateDone) <|
                            Task.succeed True
                        )

        RefreshStatus status ->
            case model.renderEnv.loginServer of
                Nothing ->
                    model |> withNoCmd

                Just server ->
                    model
                        |> withCmd
                            (Request.serverRequest
                                (\i r -> ColumnsUIMsg <| ReceiveRefreshStatus i r)
                                []
                                { server = server
                                , token = model.token
                                }
                                status
                                (StatusesRequest <| Request.GetStatus { id = status.id })
                            )

        ReceiveRefreshStatus _ result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok response ->
                    updateColumnsStatus response.entity model
                        |> withNoCmd

        SelectPollOption statusId idx isMultiple ->
            selectPollOption statusId idx isMultiple model

        SubmitPollVotes statusId pollId ->
            submitPollVotes statusId pollId model

        ReceivePollVotes statusId result ->
            receivePollVotes statusId result model

        SetPollDefinitionOption idx string ->
            updatePostDialogPoll
                model
                (\def ->
                    { def
                        | options =
                            LE.setAt idx string def.options
                    }
                )

        AddPollDefinitionOption ->
            updatePostDialogPoll
                model
                (\def ->
                    { def | options = def.options ++ [ "" ] }
                )

        RemovePollDefinitionOption idx ->
            updatePostDialogPoll
                model
                (\def ->
                    { def
                        | options =
                            if List.length def.options < 3 then
                                def.options

                            else
                                LE.removeAt idx def.options
                    }
                )

        TogglePollDefinitionMultiple ->
            updatePostDialogPoll
                model
                (\def ->
                    { def
                        | multiple = not def.multiple
                    }
                )

        SetDaysHoursMinutes which value ->
            let
                postState =
                    model.postState

                dhm =
                    postState.daysHoursMinutes

                setter =
                    case which of
                        "days" ->
                            \s -> { dhm | days = s }

                        "hours" ->
                            \s -> { dhm | hours = s }

                        "minutes" ->
                            \s -> { dhm | minutes = s }

                        _ ->
                            \s -> dhm
            in
            { model
                | postState =
                    { postState
                        | daysHoursMinutes = setter value
                    }
            }
                |> withNoCmd

        ShowStatusHistoryDialog status ->
            let
                ( mdl, cmd ) =
                    setStatusHistoryDialog model <| StatusHistoryDialog status []

                ( mdl2, cmd2 ) =
                    sendRequest
                        (StatusesRequest <|
                            Request.GetStatusHistory
                                { id = status.id }
                        )
                        mdl
            in
            mdl2 |> withCmds [ cmd, cmd2 ]

        ReceiveStatusSource status result ->
            receiveStatusSource status result model

        TranslateStatus statusId targetLanguage ->
            sendRequest
                (StatusesRequest <|
                    Request.PostTranslate
                        { id = statusId
                        , target_language = targetLanguage
                        }
                )
                model

        UntranslateStatus statusId ->
            { model
                | renderEnv =
                    { renderEnv
                        | translationDict =
                            Dict.remove statusId renderEnv.translationDict
                    }
            }
                |> withNoCmd

        ToggleMarkdownInput ->
            case model.dialog of
                PostDialog ->
                    let
                        postState =
                            model.postState
                    in
                    { model
                        | postState =
                            { postState
                                | content_type =
                                    if postState.content_type == markdownContentType then
                                        plainTextContentType

                                    else
                                        markdownContentType
                            }
                    }
                        |> withNoCmd

                _ ->
                    model |> withNoCmd


showImageUrl : String -> Model -> ( Model, Cmd Msg )
showImageUrl url model =
    case Url.fromString url of
        Nothing ->
            model |> withNoCmd

        Just { path } ->
            let
                attachmentType =
                    if String.endsWith (String.toLower path) ".gif" then
                        GifvAttachment

                    else
                        ImageAttachment

                attachment =
                    { id = "dialogHeader"
                    , type_ = attachmentType
                    , url = url
                    , remote_url = Nothing
                    , preview_url = Nothing
                    , text_url = Nothing
                    , description = Nothing
                    , meta = Nothing
                    , v = JE.null
                    }

                attachmentView =
                    { attachments = [ attachment ]
                    , index = 0
                    }
            in
            { model
                | attachmentView = Just attachmentView
            }
                |> withNoCmd


receiveStatusSource : Status -> Result Error Response -> Model -> ( Model, Cmd Msg )
receiveStatusSource status result model =
    case result of
        Err err ->
            { model | msg = Just <| Debug.toString err }
                |> withNoCmd

        Ok response ->
            case response.entity of
                StatusSourceEntity source ->
                    let
                        mdlPostState =
                            model.postState

                        postState =
                            let
                                me =
                                    case model.account of
                                        Nothing ->
                                            ""

                                        Just account ->
                                            account.username

                                text =
                                    case statusMentionsString me status source.text of
                                        "" ->
                                            source.text

                                        mentions ->
                                            mentions ++ " " ++ source.text
                            in
                            { mdlPostState
                                | replyType = EditPost status
                                , text = text
                                , content_type =
                                    case source.content_type of
                                        Nothing ->
                                            plainTextContentType

                                        Just ct ->
                                            if List.member ct knownContentTypes then
                                                ct

                                            else
                                                plainTextContentType

                                -- So that text will be cleared
                                , mentionsString = text
                            }
                    in
                    { model
                        | dialog = PostDialog
                        , postState = postState
                    }
                        |> withNoCmd

                _ ->
                    model |> withNoCmd


alertDialogCmd : String -> Cmd Msg
alertDialogCmd text =
    Task.perform (GlobalMsg << SetDialog)
        (Task.succeed <| AlertDialog text)


updateFeedColumn : FeedType -> Model -> ( Model, Cmd Msg )
updateFeedColumn feedType model =
    let
        feeds =
            LE.updateIf
                (\feed ->
                    feedTypeEqual feedType feed.feedType
                )
                (\feed ->
                    { feed | feedType = feedType }
                )
                model.feedSet.feeds

        dialog =
            case model.dialog of
                FeedTypeDialog ft ->
                    if feedTypeEqual feedType ft then
                        FeedTypeDialog feedType

                    else
                        model.dialog

                _ ->
                    model.dialog

        feedSet =
            model.feedSet

        feedSetDefinition =
            model.feedSetDefinition

        mdl =
            { model
                | feedSet =
                    { feedSet | feeds = feeds }
                , feedSetDefinition =
                    { feedSetDefinition
                        | feedTypes = List.map .feedType feeds
                    }
                , dialog = dialog
            }
    in
    case findFeed feedType mdl.feedSet of
        Nothing ->
            mdl |> withNoCmd

        Just feed ->
            let
                ( mdl2, cmd2 ) =
                    reloadFeed feed mdl

                cmd3 =
                    case feed.feedType of
                        NotificationFeed _ ->
                            resetSelectOption nodeIds.notificationTypeSelect

                        _ ->
                            Cmd.none
            in
            mdl2
                |> withCmds
                    [ maybePutFeedSetDefinition mdl mdl.feedSetDefinition
                    , cmd2
                    , cmd3
                    ]


foldStatuses : (a -> Feed -> Status -> ( a, Bool )) -> a -> List Feed -> a
foldStatuses folder initialA initialFeeds =
    let
        foldWrapped : a -> Feed -> Maybe WrappedStatus -> ( a, Bool )
        foldWrapped a feed maybeWrapped =
            case maybeWrapped of
                Nothing ->
                    ( a, False )

                Just (WrappedStatus wrapped) ->
                    folder a feed wrapped

        statusesLoop : a -> Feed -> List Status -> ( a, Bool )
        statusesLoop a feed statuses =
            case statuses of
                [] ->
                    ( a, False )

                status :: rest ->
                    let
                        ( a2, done2 ) =
                            folder a feed status
                    in
                    if done2 then
                        ( a2, True )

                    else
                        let
                            ( a3, done3 ) =
                                foldWrapped a2 feed status.reblog
                        in
                        if done3 then
                            ( a3, True )

                        else
                            let
                                ( a4, done4 ) =
                                    foldWrapped a3 feed status.quote
                            in
                            if done4 then
                                ( a4, True )

                            else
                                statusesLoop a4 feed rest

        notificationsLoop : a -> Feed -> List Notification -> ( a, Bool )
        notificationsLoop a feed notifications =
            case notifications of
                [] ->
                    ( a, False )

                notification :: rest ->
                    case notification.status of
                        Nothing ->
                            notificationsLoop a feed rest

                        Just status ->
                            let
                                ( a2, done ) =
                                    folder a feed status
                            in
                            if done then
                                ( a2, True )

                            else
                                notificationsLoop a2 feed rest

        elementsLoop : a -> Feed -> FeedElements -> ( a, Bool )
        elementsLoop a feed elements =
            case elements of
                StatusElements statuses ->
                    statusesLoop a feed statuses

                NotificationElements notifications ->
                    notificationsLoop a feed notifications

                _ ->
                    ( a, False )

        feedLoop : a -> List Feed -> ( a, Bool )
        feedLoop a feeds =
            case feeds of
                [] ->
                    ( a, False )

                feed :: rest ->
                    let
                        ( a2, done ) =
                            elementsLoop a feed feed.elements
                    in
                    if done then
                        ( a2, True )

                    else
                        feedLoop a2 rest
    in
    feedLoop initialA initialFeeds
        |> Tuple.first


{-| Update bodyEnv.now, if there's a poll in Feed whose time display would change.
-}
maybeUpdateBodyEnvNow : Posix -> Model -> Model
maybeUpdateBodyEnvNow now model =
    let
        folder : ( Model, Maybe String ) -> Feed -> Status -> ( ( Model, Maybe String ), Bool )
        folder ( mdl, mfid ) feed status =
            let
                feedType =
                    feed.feedType

                feedId =
                    Types.feedID feedType
            in
            if
                (mfid == Just feedId)
                    || (feedType == ThreadExplorerFeed)
                    || (feedType == AccountDialogFeed)
            then
                ( ( mdl, mfid ), False )

            else
                case status.poll of
                    Nothing ->
                        ( ( mdl, mfid ), False )

                    Just poll ->
                        let
                            feedEnv =
                                getFeedEnv feed.feedType mdl
                        in
                        if not <| pollTimeChanges poll feedEnv.bodyEnv.now then
                            ( ( mdl, mfid ), False )

                        else
                            let
                                bodyEnv =
                                    feedEnv.bodyEnv

                                newFeedEnv =
                                    { feedEnv
                                        | bodyEnv =
                                            { bodyEnv | now = now }
                                    }
                            in
                            ( ( { mdl
                                    | feedEnvs =
                                        Dict.insert feedId newFeedEnv mdl.feedEnvs
                                }
                              , Just feedId
                              )
                            , False
                            )

        renderEnv =
            model.renderEnv

        here =
            renderEnv.here

        pollTimeChanges poll bodyEnvNow =
            let
                expires_at =
                    poll.expires_at
            in
            pollTimeUntil here bodyEnvNow expires_at
                /= pollTimeUntil here now expires_at

        ( mdl2, _ ) =
            foldStatuses folder ( model, Nothing ) model.feedSet.feeds
    in
    mdl2


{-| Never rendered. Just used for updating.
-}
simpleButton : Button () ColumnsUIMsg
simpleButton =
    Button.simpleButton ( 10, 10 ) ()


containsAcct : String -> String -> Bool
containsAcct acct source =
    if source == "" then
        False

    else
        let
            a =
                "@" ++ acct

            aLen =
                String.length a
        in
        case String.indexes a source of
            [] ->
                False

            indexes ->
                let
                    chars =
                        String.toList source |> Array.fromList

                    isWhiteSpaceChar : Int -> Bool
                    isWhiteSpaceChar index =
                        case Array.get index chars of
                            Nothing ->
                                True

                            Just char ->
                                isWhiteSpace char

                    loop : List Int -> Bool
                    loop tail =
                        case tail of
                            [] ->
                                False

                            index :: rest ->
                                if isWhiteSpaceChar (index - 1) then
                                    if isWhiteSpaceChar (index + aLen) then
                                        True

                                    else
                                        loop rest

                                else
                                    loop rest
                in
                loop indexes


statusMentionsString : String -> Status -> String -> String
statusMentionsString me status source =
    let
        addMention acct res =
            if acct == me || containsAcct acct source then
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
            case postState.replyType of
                EditPost _ ->
                    postState

                _ ->
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
                        statusMentionsString me replyTo ""
                in
                { postState
                    | text = mentionsString
                    , mentionsString = mentionsString
                }


updatePostDialogPoll : Model -> (PollDefinition -> PollDefinition) -> ( Model, Cmd Msg )
updatePostDialogPoll model updater =
    case model.dialog of
        PostDialog ->
            case model.postState.pollDefinition of
                Nothing ->
                    model |> withNoCmd

                Just def ->
                    let
                        state =
                            model.postState
                    in
                    { model
                        | postState =
                            { state
                                | pollDefinition =
                                    Just <| updater def
                            }
                    }
                        |> withNoCmd

        _ ->
            model |> withNoCmd


receivePollVotes : String -> Result Error Response -> Model -> ( Model, Cmd Msg )
receivePollVotes statusId result model =
    let
        feeds =
            allFeeds model

        folder : (FeedBodyEnv -> FeedBodyEnv) -> Feed -> Dict String FeedEnv -> Dict String FeedEnv
        folder modifier feed feedEnvs =
            let
                feedId =
                    Types.feedID feed.feedType
            in
            case Dict.get feedId feedEnvs of
                Nothing ->
                    feedEnvs

                Just feedEnv ->
                    let
                        bodyEnv =
                            modifier feedEnv.bodyEnv
                    in
                    if bodyEnv == feedEnv.bodyEnv then
                        feedEnvs

                    else
                        Dict.insert feedId
                            { feedEnv
                                | bodyEnv =
                                    bodyEnv
                            }
                            feedEnvs

        mdl =
            { model
                | feedEnvs =
                    List.foldl
                        (folder
                            (\bodyEnv ->
                                { bodyEnv
                                    | pollsSubmitted =
                                        Set.remove statusId bodyEnv.pollsSubmitted
                                }
                            )
                        )
                        model.feedEnvs
                        feeds
            }
    in
    case result of
        Err err ->
            { mdl | msg = Just <| Debug.toString err }
                |> withNoCmd

        Ok response ->
            case response.entity of
                PollEntity poll ->
                    let
                        mdl2 =
                            { mdl
                                | pollSelections =
                                    Dict.remove statusId mdl.pollSelections
                                , feedEnvs =
                                    List.foldl
                                        (folder
                                            (\bodyEnv ->
                                                { bodyEnv
                                                    | pollSelections =
                                                        Dict.remove statusId
                                                            bodyEnv.pollSelections
                                                }
                                            )
                                        )
                                        mdl.feedEnvs
                                        feeds
                            }
                    in
                    modifyColumnsStatus statusId
                        (\s -> { s | poll = Just poll })
                        mdl2
                        |> withNoCmd

                entity ->
                    let
                        e =
                            Debug.log "Bad entity in receivePollVotes" entity
                    in
                    { mdl | msg = Just "Bad entity in receivePollVotes" }
                        |> withNoCmd


isPollSubmitted : String -> FeedBodyEnv -> Bool
isPollSubmitted statusId bodyEnv =
    Set.member statusId bodyEnv.pollsSubmitted


setPollSubmitted : String -> Bool -> FeedType -> Model -> Model
setPollSubmitted statusId isSubmitted feedType model =
    let
        feedEnv =
            getFeedEnv feedType model

        bodyEnv =
            feedEnv.bodyEnv
    in
    { model
        | feedEnvs =
            Dict.insert (Types.feedID feedType)
                { feedEnv
                    | bodyEnv =
                        { bodyEnv
                            | pollsSubmitted =
                                if isSubmitted then
                                    Set.insert statusId bodyEnv.pollsSubmitted

                                else
                                    Set.remove statusId bodyEnv.pollsSubmitted
                        }
                }
                model.feedEnvs
    }


allFeedTypes : Model -> List FeedType
allFeedTypes model =
    let
        accountDialogTypes =
            case model.dialog of
                AccountDialog _ (Just (StatusesContent _)) ->
                    [ AccountDialogFeed ]

                _ ->
                    []

        threadExplorerTypes =
            case model.popupExplorer of
                ThreadPopupExplorer _ ->
                    [ ThreadExplorerFeed ]

                _ ->
                    []
    in
    accountDialogTypes
        ++ threadExplorerTypes
        ++ List.map .feedType model.feedSet.feeds


allFeeds : Model -> List Feed
allFeeds model =
    let
        emptyFeed =
            { feedType = AccountDialogFeed
            , elements = StatusElements []
            , newElements = 0
            , undisplayedElements = NoUndisplayed
            , error = Nothing
            }

        accountDialogFeeds =
            case model.dialog of
                AccountDialog account (Just (StatusesContent content)) ->
                    [ { emptyFeed
                        | feedType = AccountDialogFeed
                        , elements = StatusElements content.statuses
                      }
                    ]

                _ ->
                    []

        threadExplorerFeeds =
            case model.popupExplorer of
                ThreadPopupExplorer content ->
                    [ { emptyFeed
                        | feedType = ThreadExplorerFeed
                        , elements =
                            List.foldl (\ss l -> ss.displayed ++ l)
                                []
                                content.ribbon
                                |> StatusElements
                      }
                    ]

                _ ->
                    []
    in
    accountDialogFeeds ++ threadExplorerFeeds ++ model.feedSet.feeds


submitPollVotes : String -> String -> Model -> ( Model, Cmd Msg )
submitPollVotes statusId pollId model =
    case Maybe.withDefault [] <| Dict.get statusId model.pollSelections of
        [] ->
            { model | msg = Just <| "No poll selection for statusId: " ++ statusId }
                |> withNoCmd

        selections ->
            let
                folder : Model -> Feed -> Status -> ( Model, Bool )
                folder mdl feed status =
                    if statusId /= status.id then
                        ( mdl, False )

                    else
                        ( setPollSubmitted statusId True feed.feedType model
                        , False
                        )

                mdl2 =
                    foldStatuses folder model <| allFeeds model
            in
            mdl2
                |> withCmd
                    (serverRequest
                        (\id result -> ColumnsUIMsg <| ReceivePollVotes id result)
                        statusId
                        (PollsRequest <|
                            Request.PostVotes
                                { id = pollId
                                , choices = selections
                                }
                        )
                        mdl2
                    )


selectPollOption : String -> Int -> Bool -> Model -> ( Model, Cmd Msg )
selectPollOption statusId idx isMultiple model =
    let
        selections =
            Maybe.withDefault [] <| Dict.get statusId model.pollSelections

        newSelections =
            if isMultiple then
                if List.member idx selections then
                    LE.filterNot ((==) idx) selections

                else
                    idx :: selections

            else
                [ idx ]

        mdl2 =
            { model
                | pollSelections =
                    Dict.insert statusId newSelections model.pollSelections
            }

        folder : Model -> Feed -> Status -> ( Model, Bool )
        folder mdl3 feed status =
            if statusId /= status.id then
                ( mdl3, False )

            else
                let
                    feedType =
                        feed.feedType

                    feedEnv =
                        getFeedEnv feedType mdl3

                    bodyEnv =
                        feedEnv.bodyEnv

                    pollSelections =
                        bodyEnv.pollSelections
                in
                if Dict.get statusId pollSelections == Just newSelections then
                    ( mdl3, False )

                else
                    let
                        newFeedEnv =
                            { feedEnv
                                | bodyEnv =
                                    { bodyEnv
                                        | pollSelections =
                                            Dict.insert statusId
                                                newSelections
                                                pollSelections
                                    }
                            }
                    in
                    ( { mdl3
                        | feedEnvs =
                            Dict.insert (Types.feedID feedType)
                                newFeedEnv
                                mdl3.feedEnvs
                      }
                    , False
                    )
    in
    foldStatuses folder mdl2 (allFeeds mdl2)
        |> withNoCmd


updateFeedBodyPollSelections : Feed -> Model -> Model
updateFeedBodyPollSelections feed model =
    let
        feedId =
            Types.feedID feed.feedType
    in
    case Dict.get feedId model.feedEnvs of
        Nothing ->
            model

        Just feedEnv ->
            let
                pollSelections =
                    model.pollSelections

                folder : Dict String (List Int) -> Feed -> Status -> ( Dict String (List Int), Bool )
                folder dict _ status =
                    if status.poll == Nothing then
                        ( dict, False )

                    else
                        case Dict.get status.id pollSelections of
                            Nothing ->
                                ( dict, False )

                            Just selections ->
                                ( Dict.insert status.id selections dict, False )

                bodyPollSelections =
                    foldStatuses folder Dict.empty [ feed ]

                bodyEnv =
                    feedEnv.bodyEnv
            in
            if bodyEnv.pollSelections == bodyPollSelections then
                model

            else
                let
                    newFeedEnv =
                        { feedEnv
                            | bodyEnv =
                                { bodyEnv
                                    | pollSelections = bodyPollSelections
                                }
                        }
                in
                { model
                    | feedEnvs =
                        Dict.insert feedId newFeedEnv model.feedEnvs
                }


fetchRelationships : List Account -> Model -> ( Model, Cmd Msg )
fetchRelationships accounts model =
    let
        relationships =
            model.relationships

        accountId =
            case model.account of
                Nothing ->
                    ""

                Just acct ->
                    acct.id

        folder { id } res =
            if id == accountId then
                res

            else
                case Dict.get id relationships of
                    Nothing ->
                        id :: res

                    Just _ ->
                        res

        toFetch =
            List.foldr folder [] accounts
    in
    sendRequest
        (AccountsRequest <|
            Request.GetRelationships { ids = toFetch }
        )
        model


stopVideosForAttachment : Int -> List Attachment -> Cmd Msg
stopVideosForAttachment index attachments =
    case LE.getAt (Debug.log "stopVideosForAttachment" index) attachments of
        Nothing ->
            Cmd.none

        Just { type_ } ->
            if
                (type_ == GifvAttachment)
                    || (type_ == VideoAttachment)
            then
                stopVideos ()

            else
                Cmd.none


getStatusesRequestFromUserFeedFlags : String -> Maybe Paging -> UserFeedFlags -> Request
getStatusesRequestFromUserFeedFlags id paging flags =
    let
        { only_media, pinned, replies, reblogs } =
            flags
    in
    AccountsRequest <|
        Request.GetStatuses
            { id = id
            , only_media = only_media
            , pinned = pinned
            , exclude_replies = not replies
            , paging = paging
            , exclude_reblogs = not reblogs
            }


sendGetFollowers : String -> Maybe Int -> Model -> ( Model, Cmd Msg )
sendGetFollowers id limit model =
    sendRequest
        (AccountsRequest <|
            Request.GetFollowers
                { id = id
                , limit = limit
                }
        )
        model


sendGetFollowing : String -> Maybe Int -> Model -> ( Model, Cmd Msg )
sendGetFollowing id limit model =
    sendRequest
        (AccountsRequest <|
            Request.GetFollowing
                { id = id
                , limit = limit
                }
        )
        model


clearPostStateReplyTo : PostState -> PostState
clearPostStateReplyTo postState =
    let
        text =
            if postState.text == postState.mentionsString then
                ""

            else
                postState.text
    in
    { postState
        | replyTo = Nothing
        , replyType = NoReply
        , visibility = postState.setVisibility
        , text = text
        , mentionsString = ""
    }


appStateUpdated : Model -> Updates -> ( Model, Cmd Msg )
appStateUpdated model { saveCount, keyCounts, updates } =
    let
        appState =
            model.appState
    in
    { model
        | appState =
            { appState
                | saveCount = Debug.log "appStateUpdated, saveCount" saveCount
                , keyCounts = Debug.log "  keyCounts" keyCounts
            }
        , appStateUpdating = True
    }
        |> applyUpdates updates


applyUpdates : Dict String (Maybe String) -> Model -> ( Model, Cmd Msg )
applyUpdates updates model =
    let
        folder : String -> Maybe String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
        folder key value ( mdl, cmd ) =
            case String.split "." key of
                prefix :: rest ->
                    case Dict.get prefix pkToUpdater of
                        Nothing ->
                            let
                                msg =
                                    Debug.log "No pkToUpdater for" prefix
                            in
                            mdl |> withCmd cmd

                        Just updater ->
                            let
                                ( mdl2, cmd2 ) =
                                    updater (String.join "." rest) value mdl
                            in
                            mdl2 |> withCmds [ cmd, cmd2 ]

                [] ->
                    -- Can't happen
                    mdl |> withNoCmd
    in
    Dict.foldl folder ( model, Cmd.none ) updates


{-| TODO: Test that the DynamoDB secrets actually work.

Put up an alert dialog if so, or the DynamoDBDialog with error message if not.

-}
commitDynamoDBDialog : Model -> ( Model, Cmd Msg )
commitDynamoDBDialog model =
    let
        account =
            model.appStateAccount

        ( mdl, _ ) =
            dismissDialog model
    in
    if
        (account.accessKey == "")
            || (account.secretKey == "")
    then
        mdl |> withCmd (putDynamoDBAccount Nothing)

    else
        { mdl | appState = AppState.mergeAccount account mdl.appState }
            |> withCmd (putDynamoDBAccount <| Just account)


yesImSure : AreYouSureReason -> Status -> Model -> ( Model, Cmd Msg )
yesImSure reason status model =
    sendRequest
        (case reason of
            AreYouSureBlock relationship ->
                let
                    blocked =
                        case relationship of
                            Just { blocking } ->
                                blocking

                            _ ->
                                False
                in
                if blocked then
                    BlocksRequest <|
                        Request.PostUnblock { id = status.account.id }

                else
                    BlocksRequest <|
                        Request.PostBlock { id = status.account.id }

            AreYouSureMute relationship ->
                let
                    muted =
                        case relationship of
                            Just { muting } ->
                                muting

                            _ ->
                                False
                in
                if muted then
                    MutesRequest <|
                        Request.PostAccountUnmute { id = status.account.id }

                else
                    MutesRequest <|
                        Request.PostAccountMute
                            { id = status.account.id
                            , notifications = model.muteNotifications
                            }

            AreYouSureDeleteStatus ->
                StatusesRequest <|
                    Request.DeleteStatus { id = status.id }
        )
        { model | dialog = NoDialog }


showUndisplayed : FeedType -> Model -> ( Model, Cmd Msg )
showUndisplayed feedType model =
    case findFeed feedType model.feedSet of
        Nothing ->
            model |> withNoCmd

        Just feed ->
            let
                feedId =
                    Types.feedID feedType
            in
            showUndisplayedFeed feed model
                |> withCmd
                    (Dom.setViewportOf feedId 0 0
                        |> Task.attempt (\_ -> Noop)
                    )


showUndisplayedFeed : Feed -> Model -> Model
showUndisplayedFeed feed model =
    let
        feedSet =
            model.feedSet

        ( newElements, count, didit ) =
            case feed.undisplayedElements of
                Undisplayed (StatusElements newStatuses) ->
                    case feed.elements of
                        StatusElements statuses ->
                            ( List.append newStatuses statuses
                                |> List.take maxFeedLength
                                |> StatusElements
                            , List.length newStatuses
                            , True
                            )

                        elements ->
                            ( elements, feed.newElements, False )

                Undisplayed (NotificationElements newNotifications) ->
                    case feed.elements of
                        NotificationElements notifications ->
                            ( List.append newNotifications notifications
                                |> List.take maxFeedLength
                                |> NotificationElements
                            , List.length newNotifications
                            , True
                            )

                        elements ->
                            ( elements, feed.newElements, False )

                _ ->
                    ( feed.elements, feed.newElements, False )

        newFeed =
            { feed
                | elements = newElements
                , newElements = count
                , undisplayedElements =
                    if didit then
                        NoUndisplayed

                    else
                        feed.undisplayedElements
            }
    in
    { model | feedSet = replaceFeed newFeed feedSet }


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
        |> updateNewElementsLeftRight
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


makeScrolledStatus status =
    { status = status
    , visited = Set.empty
    , displayed = [ status ]
    , scroll = Nothing
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
                    let
                        server =
                            model.renderEnv.loginServer
                    in
                    if serverHasFeature server featureNames.partialContext model.renderEnv then
                        sendGetStatusPartialContext AncestorsContext status.id mdl

                    else
                        let
                            request =
                                StatusesRequest <|
                                    Request.GetStatusContext { id = status.id }
                        in
                        sendGeneralRequest (ColumnsSendMsg << ReceiveStatusContext request)
                            request
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


{-| This implements the scroll keys/scroll-pill for the Thread Explorer Popup.,
-}
scrollThreadExplorer : ThreadExplorerState -> Bool -> ScrollDirection -> Model -> ( Model, Cmd Msg )
scrollThreadExplorer state allTheWay direction model =
    if allTheWay then
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
                            ( insertPostSearch account.acct
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

                CommandChoice _ _ ->
                    ( model, Types.defaultUserFeedType, False )

        ( mdl3, cmds ) =
            case choice of
                CommandChoice command status ->
                    case model.popup of
                        CommandsPopup _ ->
                            let
                                ( mdl4, cmd ) =
                                    commandChoice command status model
                            in
                            ( mdl4, [ cmd ] )

                        _ ->
                            ( mdl2, [] )

                _ ->
                    case model.popup of
                        PostTextPopup _ ->
                            ( mdl2
                            , [ Task.attempt (\_ -> Noop) <|
                                    -- Probably extraneous, since mobileFocus does this again.
                                    Dom.focus nodeIds.postDialogText
                              , mobileFocus nodeIds.postDialogText
                              ]
                            )

                        _ ->
                            ( mdl2, [] )
    in
    if addTheFeed then
        case mdl3.dialog of
            EditColumnsDialog ->
                addFeedType (fillinFeedType feedType mdl3) mdl3

            FeedTypeDialog ftdType ->
                case ftdType of
                    NotificationFeed params ->
                        case mdl3.accountInput of
                            Just { id } ->
                                let
                                    newFeedType =
                                        NotificationFeed
                                            { params
                                                | accountId = Just id
                                            }
                                in
                                updateFeedColumn newFeedType mdl3

                            _ ->
                                mdl3 |> withNoCmd

                    _ ->
                        mdl3 |> withNoCmd

            _ ->
                mdl3 |> withNoCmd

    else
        mdl3 |> withCmds cmds


commandChoice : Command -> Status -> Model -> ( Model, Cmd Msg )
commandChoice command status model =
    let
        mdl =
            { model
                | popup = NoPopup
                , popupElement = Nothing
                , popupChoices = []
            }

        setAreYouSureDialog : AreYouSureReason -> Model
        setAreYouSureDialog reason =
            { mdl
                | dialog = AreYouSureDialog reason status
                , muteNotifications =
                    case reason of
                        AreYouSureMute _ ->
                            True

                        _ ->
                            mdl.muteNotifications
            }
    in
    case command of
        -- my post
        MuteConversationCommand ->
            let
                req =
                    if status.muted then
                        Request.PostStatusUnmute

                    else
                        Request.PostStatusMute
            in
            sendRequest
                (MutesRequest <|
                    req { id = status.id }
                )
                mdl

        PinOnProfileCommand ->
            let
                req =
                    if status.pinned then
                        Request.PostUnpinStatus

                    else
                        Request.PostPinStatus
            in
            sendRequest
                (StatusesRequest <|
                    req { id = status.id }
                )
                mdl

        DeleteStatusCommand ->
            setAreYouSureDialog AreYouSureDeleteStatus
                |> withNoCmd

        EditStatusCommand stat ->
            let
                attachments =
                    stat.media_attachments

                postState =
                    -- Ignoring polls for now. I don't think Rebased does the
                    -- right thing for them, anyway.
                    { initialPostState
                        | content_type = model.postState.content_type
                        , sensitive = status.sensitive
                        , visibility = status.visibility
                        , media_ids = List.map .id attachments
                        , fileNames =
                            List.map
                                (\a -> Maybe.withDefault "" a.description)
                                attachments
                        , fileUrls =
                            List.map
                                (\a ->
                                    case a.preview_url of
                                        Nothing ->
                                            a.url

                                        Just url ->
                                            url
                                )
                                attachments
                    }

                mdl2 =
                    { mdl | postState = postState }
            in
            mdl2
                |> withCmd
                    (serverRequest
                        (\s r -> ColumnsUIMsg <| ReceiveStatusSource s r)
                        stat
                        (StatusesRequest <| Request.GetStatusSource { id = stat.id })
                        mdl2
                    )

        DeleteAndRedraftCommand ->
            mdl
                |> withCmd (alertDialogCmd "Delete & Redraft not yet available.")

        -- other user's post
        MentionCommand ->
            clearPostState mdl
                |> withCmd
                    (Task.perform ColumnsUIMsg <|
                        Task.succeed (PostWithMention status.account.username)
                    )

        MuteCommand relationship ->
            if
                case relationship of
                    Just req ->
                        req.muting

                    Nothing ->
                        False
            then
                yesImSure (AreYouSureMute relationship) status mdl

            else
                setAreYouSureDialog (AreYouSureMute relationship)
                    |> withNoCmd

        BlockCommand relationship ->
            if
                case relationship of
                    Just req ->
                        req.blocking

                    Nothing ->
                        False
            then
                yesImSure (AreYouSureBlock relationship) status mdl

            else
                setAreYouSureDialog (AreYouSureBlock relationship)
                    |> withNoCmd

        ReportCommand ->
            mdl
                |> withCmd (alertDialogCmd "Reporting not yet available.")

        _ ->
            mdl |> withNoCmd


clearPostState : Model -> Model
clearPostState model =
    { model
        | postState =
            { initialPostState
                | content_type = model.postState.content_type
            }
    }


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
        let
            doSearch request =
                let
                    ( mdl, searchCmd ) =
                        sendRequest request model

                    ( mdl2, cmd ) =
                        if model.searchActive then
                            { model | nextSearch = searchCmd } |> withNoCmd

                        else
                            { mdl
                                | searchActive = True
                                , nextSearch = Cmd.none
                            }
                                |> withCmd searchCmd
                in
                { mdl2
                    | popup = PostTextPopup search
                    , postTriggerCoordinatesCount =
                        model.postTriggerCoordinatesCount + 1
                }
                    |> withCmd cmd
        in
        case search.popupType of
            PostPopupColon ->
                searchPostPopupColon search model

            PostPopupAtsign ->
                (AccountsRequest <|
                    Request.GetSearchAccounts
                        { q = search.string
                        , limit = Nothing
                        , resolve = True
                        , following = False
                        }
                )
                    |> doSearch

            _ ->
                (SearchRequest <|
                    Request.GetSearch
                        { q = search.string
                        , resolve = True
                        , limit = Nothing
                        , offset = Nothing
                        , following = False
                        }
                )
                    |> doSearch


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
        let
            mdl =
                { model
                    | popup = NoPopup
                    , popupElement = Nothing
                }
        in
        case mdl.dialog of
            FeedTypeDialog (NotificationFeed params) ->
                if params.accountId == Nothing then
                    mdl |> withNoCmd

                else
                    let
                        newFeedType =
                            NotificationFeed { params | accountId = Nothing }
                    in
                    updateFeedColumn newFeedType mdl

            _ ->
                mdl |> withNoCmd

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
        | msg = Nothing
        , dialog = NoDialog
        , attachmentView = Nothing
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

        Just tokenApi ->
            let
                renderEnv =
                    model.renderEnv

                mdl =
                    { model
                        | renderEnv =
                            { renderEnv
                                | loginServer = Just server

                                -- Temporary Model, so doesn't matter
                                , accountId = ""
                            }
                        , token = tokenApi.token
                    }

                request =
                    GroupsRequest <|
                        Request.GetGroups { tab = Request.AdminGroups }

                ( _, cmd ) =
                    sendRequest request mdl
            in
            { model
                | featureProbeRequests =
                    ( server, request )
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
            case model.attachmentView of
                Just attachmentView ->
                    scrollAttachmentDialog allTheWay direction attachmentView mdl

                _ ->
                    scrollPageInternal allTheWay direction mdl

        ThreadPopupExplorer state ->
            scrollThreadExplorer state allTheWay direction mdl


showEllipsisPopup : String -> Status -> Model -> ( Model, Cmd Msg )
showEllipsisPopup ellipsisId status model =
    let
        accountId =
            case model.account of
                Just account ->
                    account.id

                Nothing ->
                    ""

        isMyPost =
            status.account.id == accountId
    in
    { model
        | popup = CommandsPopup ellipsisId
        , popupElement = Nothing -- TODO (unique ID for each status rendering)
        , popupChoices =
            if isMyPost then
                myEllipsisChoices status model

            else
                otherGuyEllipsisChoices status model
    }
        |> withCmds
            [ Task.attempt (ColumnsUIMsg << ReceivePopupElement)
                (Dom.getElement ellipsisId)
            , if isMyPost then
                Cmd.none

              else
                sendRequest
                    (AccountsRequest <|
                        Request.GetRelationships { ids = [ status.account.id ] }
                    )
                    model
                    |> Tuple.second
            ]


myEllipsisChoices : Status -> Model -> List PopupChoice
myEllipsisChoices status model =
    let
        server =
            model.renderEnv.loginServer
    in
    List.map (\command -> CommandChoice command status) <|
        List.concat
            [ [ MuteConversationCommand
              , SeparatorCommand
              , PinOnProfileCommand
              , SeparatorCommand
              ]
            , if serverHasFeature server featureNames.editing model.renderEnv then
                [ EditStatusCommand status ]

              else
                []
            , [ DeleteStatusCommand
              , DeleteAndRedraftCommand
              , SeparatorCommand
              , CancelCommand
              ]
            ]


otherGuyEllipsisChoices : Status -> Model -> List PopupChoice
otherGuyEllipsisChoices status model =
    List.map (\command -> CommandChoice command status)
        [ MuteConversationCommand
        , SeparatorCommand
        , MentionCommand
        , SeparatorCommand
        , MuteCommand Nothing
        , BlockCommand Nothing
        , ReportCommand
        , SeparatorCommand
        , CancelCommand
        ]


scrollAttachmentDialog : Bool -> ScrollDirection -> AttachmentView -> Model -> ( Model, Cmd Msg )
scrollAttachmentDialog allTheWay direction attachmentView model =
    let
        attachments =
            attachmentView.attachments

        attachmentCnt =
            List.length attachments

        attachmentIndex =
            attachmentView.index

        newIndex =
            if allTheWay then
                case direction of
                    ScrollLeft ->
                        0

                    ScrollRight ->
                        attachmentCnt - 1

            else
                case direction of
                    ScrollLeft ->
                        max 0 (attachmentIndex - 1)

                    ScrollRight ->
                        min (attachmentIndex + 1) (attachmentCnt - 1)
    in
    { model
        | attachmentView =
            Just { attachmentView | index = newIndex }
    }
        |> withCmd (stopVideosForAttachment newIndex attachments)


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


computeScrollColumns : ScrollInfo -> ScrollColumns
computeScrollColumns { scrollLeft, col0Left, columnWidth, maxColumn, windowWidth } =
    let
        left =
            toFloat (scrollLeft - col0Left)
                / toFloat columnWidth

        right =
            toFloat (scrollLeft + windowWidth - col0Left)
                / toFloat columnWidth
    in
    { left =
        if (left - toFloat (truncate left)) < 0.25 then
            max 0 <| truncate left

        else
            max 0 <| (truncate left + 1)
    , right =
        if (right - toFloat (truncate right)) < 0.75 then
            min maxColumn <| truncate right - 1

        else
            min maxColumn <| truncate right
    }


scrollPageInternal : Bool -> ScrollDirection -> Model -> ( Model, Cmd Msg )
scrollPageInternal allTheWay direction model =
    let
        renderEnv =
            model.renderEnv

        ( windowWidth, _ ) =
            renderEnv.windowSize
    in
    let
        { scrollLeft, col0Left, columnWidth } =
            columnScrollInfo model

        columnCnt =
            model.feedSet.feeds |> List.length

        width =
            col0Left + columnCnt * columnWidth

        rawNewScroll =
            if allTheWay then
                case direction of
                    ScrollLeft ->
                        col0Left

                    ScrollRight ->
                        width + 2 - windowWidth

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
                                width + 2 - windowWidth

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


scrollToNew : ScrollDirection -> FeedType -> Model -> Cmd Msg
scrollToNew direction feedType model =
    case direction of
        ScrollLeft ->
            scrollLeftToNew feedType model

        ScrollRight ->
            scrollRightToNew feedType model


feedHasNew : Feed -> Bool
feedHasNew feed =
    (feed.newElements > 0)
        || (case feed.undisplayedElements of
                Undisplayed elements ->
                    Types.feedElementsCount elements > 0

                _ ->
                    False
           )


scrollLeftToNew : FeedType -> Model -> Cmd Msg
scrollLeftToNew feedType model =
    let
        ft =
            feedType

        findIndex : Int -> Maybe Int -> List Feed -> Int
        findIndex idx last feeds =
            case feeds of
                [] ->
                    -- Can't happen
                    0

                feed :: tail ->
                    if feedType == feed.feedType then
                        case last of
                            Just i ->
                                i

                            Nothing ->
                                idx

                    else if feedHasNew feed then
                        findIndex (idx + 1) (Just idx) tail

                    else
                        findIndex (idx + 1) last tail
    in
    scrollToIndexOnRight (findIndex 0 Nothing model.feedSet.feeds) model


scrollToIndexOnRight : Int -> Model -> Cmd Msg
scrollToIndexOnRight idx model =
    let
        { scrollLeft, col0Left, columnWidth, maxColumn, windowWidth } =
            columnScrollInfo model

        scrollPos =
            col0Left
                + (columnWidth * (idx + 1))
                - windowWidth

        newScroll =
            if scrollPos < 0 then
                col0Left

            else
                scrollPos
    in
    if newScroll /= scrollLeft then
        Dom.setViewportOf "body" (toFloat newScroll) 0
            |> Task.attempt (\_ -> Noop)

    else
        Cmd.none


scrollRightToNew : FeedType -> Model -> Cmd Msg
scrollRightToNew feedType model =
    let
        findIndex : Int -> Maybe Int -> List Feed -> Int
        findIndex idx found feeds =
            case feeds of
                [] ->
                    -- Can't happen
                    0

                feed :: tail ->
                    case found of
                        Just foundIdx ->
                            if feedHasNew feed then
                                idx

                            else
                                findIndex (idx + 1) found tail

                        Nothing ->
                            if feedType == feed.feedType then
                                findIndex (idx + 1) (Just idx) tail

                            else
                                findIndex (idx + 1) Nothing tail
    in
    scrollToIndexOnLeft (findIndex 0 Nothing model.feedSet.feeds) model


scrollToIndexOnLeft : Int -> Model -> Cmd Msg
scrollToIndexOnLeft idx model =
    let
        { scrollLeft, col0Left, columnWidth, maxColumn, windowWidth } =
            columnScrollInfo model

        scrollPos =
            col0Left + (columnWidth * idx)

        rightDelta =
            maxColumn + 1 - idx

        maxPos =
            scrollPos + (rightDelta * columnWidth)

        newScroll =
            if maxPos < windowWidth then
                col0Left + ((maxColumn + 1) * columnWidth) - windowWidth

            else
                scrollPos
    in
    if newScroll /= scrollLeft then
        Dom.setViewportOf "body" (toFloat newScroll) 0
            |> Task.attempt (\_ -> Noop)

    else
        Cmd.none


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

        updateBodyEnvs mdl =
            List.foldl updateFeedBodyPollSelections mdl mdl.feedSet.feeds
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
                    if model.postState.posting then
                        NoDialog

                    else
                        -- Don't hide if user clicked "Clear"
                        PostDialog

                d ->
                    d
        , postState =
            { initialPostState
                | content_type =
                    model.postState.content_type
            }
    }
        |> (if showReceivedPost then
                updateBodyEnvs

            else
                identity
           )


fillinFeedType : FeedType -> Model -> FeedType
fillinFeedType feedType model =
    case feedType of
        UserFeed _ ->
            makeUserFeed model.userNameInput model.userColumnFlags

        PublicFeed _ ->
            let
                flags =
                    if model.publicColumnFlags == Types.defaultPublicFeedFlags then
                        Nothing

                    else
                        Just model.publicColumnFlags
            in
            PublicFeed { flags = flags }

        NotificationFeed _ ->
            NotificationFeed model.notificationColumnParams

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
                , undisplayedElements = NeverUndisplayed
                , error = Nothing
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
            , dialog = NoDialog
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
                    let
                        foo =
                            Debug.log "Can't find (loginServer, accountIdDict)"
                                ( loginServer, model.accountIdDict )
                    in
                    accountsRequest ()

                Just acctIds ->
                    let
                        nameAtServer =
                            usernameAtServer username server model.renderEnv
                    in
                    case LE.find (.username >> (==) nameAtServer) acctIds of
                        Just acctId ->
                            getStatusesRequest acctId.id paging params

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
                            getStatusesRequest id paging params

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


getStatusesRequest : String -> Maybe Paging -> UserFeedParams -> Request
getStatusesRequest id paging params =
    let
        userFeedFlags =
            case params.flags of
                Nothing ->
                    Types.defaultUserFeedFlags

                Just flags ->
                    flags
    in
    getStatusesRequestFromUserFeedFlags id paging userFeedFlags


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
                                        let
                                            { local, only_media } =
                                                Types.defaultPublicFeedFlags
                                        in
                                        { local = local
                                        , only_media = only_media
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

        ReceiveStatusContext request result ->
            receiveStatusContext request result model


sendGetStatusPartialContext : PartialContext -> String -> Model -> ( Model, Cmd Msg )
sendGetStatusPartialContext which statusid model =
    let
        request =
            StatusesRequest <|
                Request.GetStatusPartialContext
                    { which = which, id = statusid, offset = Nothing }
    in
    sendGeneralRequest (ColumnsSendMsg << ReceiveStatusContext request)
        request
        model


receiveStatusContext : Request -> Result Error Response -> Model -> ( Model, Cmd Msg )
receiveStatusContext request result model =
    case request of
        StatusesRequest (Request.GetStatusContext { id }) ->
            case result of
                Ok _ ->
                    receiveResponse request result model

                Err _ ->
                    sendGetStatusPartialContext AncestorsContext id model

        StatusesRequest (Request.GetStatusPartialContext { which, id }) ->
            case result of
                Err _ ->
                    receiveResponse request
                        result
                        { model | awaitingContext = Nothing }

                Ok response ->
                    case which of
                        AncestorsContext ->
                            case response.entity of
                                StatusListEntity statuses ->
                                    sendGetStatusPartialContext DescendantsContext
                                        id
                                        { model
                                            | awaitingContext =
                                                Just
                                                    { ancestors = statuses
                                                    , descendants = []
                                                    }
                                        }

                                _ ->
                                    model |> withNoCmd

                        DescendantsContext ->
                            case response.entity of
                                StatusListEntity statuses ->
                                    case model.awaitingContext of
                                        Nothing ->
                                            model |> withNoCmd

                                        Just context ->
                                            let
                                                server =
                                                    model.renderEnv.loginServer

                                                featureName =
                                                    featureNames.partialContext

                                                mdl =
                                                    if
                                                        serverHasFeature server
                                                            featureName
                                                            model.renderEnv
                                                    then
                                                        model

                                                    else
                                                        setServerHasFeature
                                                            (Debug.log "Server does partialContext"
                                                                server
                                                            )
                                                            featureName
                                                            True
                                                            model
                                            in
                                            updateThreadExplorer id
                                                (ContextEntity
                                                    { context
                                                        | descendants = statuses
                                                    }
                                                )
                                                { mdl | awaitingContext = Nothing }
                                                |> withNoCmd

                                _ ->
                                    model |> withNoCmd

        _ ->
            -- TODO
            model |> withNoCmd


fillinMissingReplyToAccountIds : Model -> Model
fillinMissingReplyToAccountIds model =
    let
        references =
            model.references

        fillin1 : String -> ( ReferenceDict, Set String ) -> ( ReferenceDict, Set String )
        fillin1 id ( feedRefs, missing ) =
            case Dict.get id references of
                Nothing ->
                    ( feedRefs, missing )

                Just ref ->
                    ( Dict.insert id ref feedRefs, Set.remove id missing )

        fillin : String -> FeedEnv -> FeedEnv
        fillin k env =
            let
                bodyEnv =
                    env.bodyEnv

                ( refs, miss ) =
                    Set.foldl fillin1
                        ( bodyEnv.references, bodyEnv.missingReplyToAccountIds )
                        bodyEnv.missingReplyToAccountIds
            in
            if
                (refs == bodyEnv.references)
                    && (miss == bodyEnv.missingReplyToAccountIds)
            then
                -- Preserve EQ
                env

            else
                { env
                    | bodyEnv =
                        { bodyEnv
                            | references = refs
                            , missingReplyToAccountIds = miss
                        }
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
                    Set.union env.bodyEnv.missingReplyToAccountIds miss
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
        Just msg ->
            let
                feedSet =
                    model.feedSet
            in
            case findFeed feedType feedSet of
                Nothing ->
                    mdl |> withCmd cmd

                Just feed ->
                    { mdl
                        | msg = Nothing
                        , feedSet =
                            replaceFeed { feed | error = Just msg } feedSet
                    }
                        |> withCmd cmd

        Nothing ->
            case mdl.entity of
                Nothing ->
                    mdl |> withCmd cmd

                Just e ->
                    let
                        ( elements, references, relationshipsCmd ) =
                            case e of
                                StatusListEntity statuses ->
                                    ( Just <| StatusElements statuses
                                    , addStatusesReferences statuses
                                        model.references
                                    , Cmd.none
                                    )

                                NotificationListEntity notifications ->
                                    ( Just <| NotificationElements notifications
                                    , addNotificationsReferences notifications
                                        model.references
                                    , getRelationshipsCmd notifications mdl
                                    )

                                AccountListEntity accounts ->
                                    ( Just <| AccountElements accounts
                                    , model.references
                                    , Cmd.none
                                    )

                                _ ->
                                    ( Nothing, model.references, Cmd.none )

                        feedSet =
                            mdl.feedSet

                        feedEnvs =
                            model.feedEnvs

                        feedEnv =
                            case Dict.get feedId model.feedEnvs of
                                Nothing ->
                                    makeFeedEnv feedId

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

                                                ( mdl5, cmd5 ) =
                                                    webSocketConnect feedType mdl

                                                ( mdl6, cmd6, timestamp ) =
                                                    let
                                                        ( maybeTs, ( mdl7, cmd7 ) ) =
                                                            updateFeedTimestamp
                                                                feedId
                                                                receiveType
                                                                elem
                                                                mdl5
                                                    in
                                                    ( mdl7
                                                    , cmd7
                                                    , maybeTs
                                                    )
                                            in
                                            ( LE.updateIf
                                                (\feed ->
                                                    feedType == feed.feedType
                                                )
                                                (\feed ->
                                                    updateReceivedFeed
                                                        receiveType
                                                        elem
                                                        timestamp
                                                        { feed | error = Nothing }
                                                )
                                                mdl6.feedSet.feeds
                                            , mdl6 |> withCmds [ cmd5, cmd6 ]
                                            , updateFeedEnvReferences
                                                receiveType
                                                elem
                                                mdl5.references
                                                feedEnv
                                            )

                        missing =
                            feedEnv2.bodyEnv.missingReplyToAccountIds

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

                        mdl8 =
                            case findFeed feedType model.feedSet of
                                Nothing ->
                                    mdl4

                                Just feed ->
                                    updateFeedBodyPollSelections feed mdl4
                    in
                    { mdl8
                        | feedSet =
                            { feedSet | feeds = feeds }
                        , references = references
                    }
                        |> updateNewElementsLeftRight
                        |> withCmds
                            [ cmd
                            , cmd2
                            , relationshipsCmd
                            ]


getRelationshipsCmd : List Notification -> Model -> Cmd Msg
getRelationshipsCmd notifications model =
    case findFeed Types.defaultNotificationFeedType model.feedSet of
        Nothing ->
            Cmd.none

        Just feed ->
            case Dict.get (Types.feedID feed.feedType) model.feedEnvs of
                Nothing ->
                    Cmd.none

                Just feedEnv ->
                    let
                        bodyEnv =
                            feedEnv.bodyEnv

                        relationships =
                            bodyEnv.relationships

                        folder : Notification -> Set String -> Set String
                        folder notification res =
                            let
                                nid =
                                    notification.account.id
                            in
                            if
                                (notification.type_ /= FollowNotification)
                                    && Dict.member nid relationships
                            then
                                res

                            else
                                Set.insert nid res

                        ids =
                            List.foldl folder Set.empty notifications
                    in
                    if Set.isEmpty ids then
                        Cmd.none

                    else
                        Task.perform (ExplorerUIMsg << SendRequest) <|
                            Task.succeed
                                (AccountsRequest <|
                                    Request.GetRelationships
                                        { ids = Set.toList ids }
                                )



-- The returned (Maybe String) is the OLD timestamp.


updateFeedTimestamp : String -> ReceiveFeedType -> FeedElements -> Model -> ( Maybe String, ( Model, Cmd Msg ) )
updateFeedTimestamp timestampId receiveType elem model =
    case receiveType of
        ReceiveMoreFeed ->
            ( Nothing, model |> withNoCmd )

        _ ->
            let
                id =
                    case elem of
                        StatusElements statuses ->
                            case List.head statuses of
                                Nothing ->
                                    ""

                                Just status ->
                                    status.id

                        NotificationElements notifications ->
                            case List.head notifications of
                                Nothing ->
                                    ""

                                Just notification ->
                                    notification.id

                        _ ->
                            -- Don't handle ConversationElements here yet
                            ""
            in
            if id == "" then
                ( Nothing, model |> withNoCmd )

            else
                case model.renderEnv.loginServer of
                    Nothing ->
                        ( Nothing, model |> withNoCmd )

                    Just server ->
                        let
                            timestamps =
                                model.timestamps

                            ( dict, oldTs ) =
                                case Dict.get server timestamps of
                                    Nothing ->
                                        ( Dict.fromList [ ( timestampId, id ) ]
                                        , Nothing
                                        )

                                    Just d ->
                                        ( Dict.insert timestampId id d
                                        , Dict.get timestampId d
                                        )
                        in
                        ( oldTs
                        , { model
                            | timestamps =
                                Dict.insert server dict timestamps
                          }
                            |> withCmd
                                (putTimestamps server <| Just dict)
                        )


feedTypeToStreamType : FeedType -> Maybe StreamType
feedTypeToStreamType feedType =
    case feedType of
        HomeFeed ->
            Just UserStream

        ProFeed _ ->
            Just ProStream

        PublicFeed _ ->
            Just PublicStream

        HashtagFeed hash ->
            Just <| LocalHashtagStream hash

        ListFeed list ->
            Just <| ListStream list

        GroupFeed groupId ->
            Just <| GroupStream groupId

        _ ->
            Nothing


webSocketConnect : FeedType -> Model -> ( Model, Cmd Msg )
webSocketConnect feedType model =
    model |> withNoCmd


webSocketConnectNotYet : FeedType -> Model -> ( Model, Cmd Msg )
webSocketConnectNotYet feedType model =
    let
        wsFeedType =
            case feedType of
                NotificationFeed _ ->
                    HomeFeed

                _ ->
                    feedType

        feedId =
            Types.feedID wsFeedType

        webSocketFeeds =
            model.webSocketFeeds
    in
    if Set.member feedId webSocketFeeds then
        ( model, Cmd.none )

    else
        case feedTypeToStreamType wsFeedType of
            Nothing ->
                ( model, Cmd.none )

            Just streamType ->
                case model.token of
                    Nothing ->
                        ( model, Cmd.none )

                    Just _ ->
                        case model.streaming_api of
                            Nothing ->
                                ( model, Cmd.none )

                            Just api ->
                                let
                                    url =
                                        Mastodon.WebSocket.streamUrl api
                                            model.token
                                            streamType
                                in
                                ( addWebSocketFeed feedId model
                                , WebSocket.makeOpenWithKey feedId
                                    (Debug.log "Open WebSocket" url)
                                    |> webSocketSend
                                )


updateNewElementsLeftRight : Model -> Model
updateNewElementsLeftRight model =
    let
        feeds =
            model.feedSet.feeds

        leftColumn =
            model.scrollColumns.left

        rightColumn =
            model.scrollColumns.right

        undisplayedPairs =
            List.map computeUndisplayed feeds

        undisplayedCounts =
            List.map Tuple.first undisplayedPairs

        totalUndisplayed =
            List.foldr (+) 0 undisplayedCounts

        news =
            List.map .newElements feeds

        totalNews =
            totalUndisplayed + List.foldr (+) 0 news

        totalCols =
            List.foldr
                (\( n, m ) tot ->
                    if n > 0 || m > 0 then
                        tot + 1

                    else
                        tot
                )
                0
                (List.map2 Tuple.pair news undisplayedCounts)

        computeUndisplayed feed =
            case feed.undisplayedElements of
                Undisplayed elements ->
                    ( Types.feedElementsCount elements, 1 )

                _ ->
                    ( 0, 0 )

        loop : Int -> Int -> Int -> List Int -> List Feed -> Dict String FeedEnv -> Dict String FeedEnv
        loop colidx left cols newsTail feedsTail feedEnvs =
            case newsTail of
                [] ->
                    feedEnvs

                total :: newsRest ->
                    case feedsTail of
                        [] ->
                            feedEnvs

                        feed :: feedsRest ->
                            let
                                feedId =
                                    Types.feedID feed.feedType

                                ( undisplayed, undisplayedCount ) =
                                    computeUndisplayed feed

                                newElements =
                                    feed.newElements + undisplayed

                                newCols =
                                    if feed.newElements > 0 then
                                        1

                                    else
                                        undisplayedCount
                            in
                            loop (colidx + 1)
                                (left + newElements)
                                (cols + newCols)
                                newsRest
                                feedsRest
                                (case Dict.get feedId feedEnvs of
                                    Nothing ->
                                        feedEnvs

                                    Just feedEnv ->
                                        let
                                            right =
                                                totalNews - left - newElements

                                            rightCols =
                                                totalCols - cols - newCols

                                            ( storedLeft, storedCols ) =
                                                if colidx == leftColumn then
                                                    ( left, cols )

                                                else
                                                    ( 0, 0 )

                                            ( storedRight, storedRightCols ) =
                                                if colidx == rightColumn then
                                                    ( right, rightCols )

                                                else
                                                    ( 0, 0 )
                                        in
                                        if
                                            (feedEnv.newElementsLeft == storedLeft)
                                                && (feedEnv.newColumnsLeft == storedCols)
                                                && (feedEnv.newElementsRight == storedRight)
                                                && (feedEnv.newColumnsRight == storedRightCols)
                                        then
                                            feedEnvs

                                        else
                                            Dict.insert feedId
                                                { feedEnv
                                                    | newElementsLeft = storedLeft
                                                    , newColumnsLeft = storedCols
                                                    , newElementsRight = storedRight
                                                    , newColumnsRight = storedRightCols
                                                }
                                                feedEnvs
                                )

        feedEnvs2 =
            loop 0 0 0 news feeds model.feedEnvs
    in
    { model | feedEnvs = feedEnvs2 }


updateFeedEnvReferences : ReceiveFeedType -> FeedElements -> ReferenceDict -> FeedEnv -> FeedEnv
updateFeedEnvReferences receiveType feedElements references feedEnv =
    let
        bodyEnv =
            feedEnv.bodyEnv

        referencesAndMissing =
            case receiveType of
                ReceiveWholeFeed ->
                    ( Dict.empty, Set.empty )

                _ ->
                    ( bodyEnv.references, bodyEnv.missingReplyToAccountIds )

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
    if
        (bodyEnv.references == feedReferences)
            && (bodyEnv.missingReplyToAccountIds == missingReplyToAccountIds)
    then
        feedEnv

    else
        { feedEnv
            | bodyEnv =
                { bodyEnv
                    | references = feedReferences
                    , missingReplyToAccountIds = missingReplyToAccountIds
                }
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


maxFeedLength : Int
maxFeedLength =
    100


updateReceivedFeed : ReceiveFeedType -> FeedElements -> Maybe String -> Feed -> Feed
updateReceivedFeed receiveType elements timestamp feed =
    let
        ( elements2, newElements ) =
            case receiveType of
                ReceiveWholeFeed ->
                    ( elements
                    , timestampNewElements timestamp
                        elements
                      <|
                        Types.feedElementsCount elements
                    )

                ReceiveMoreFeed ->
                    ( appendFeedElements feed.elements elements feed.elements
                        |> Tuple.first
                    , feed.newElements
                    )

                ReceiveNewFeed ->
                    let
                        ( elements3, cnt ) =
                            appendFeedElementsTruncated (Just maxFeedLength)
                                True
                                elements
                                feed.elements
                                feed.elements
                    in
                    ( elements3, timestampNewElements timestamp elements3 cnt )
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


timestampNewElements : Maybe String -> FeedElements -> Int -> Int
timestampNewElements timestamp elements default =
    case timestamp of
        Nothing ->
            default

        Just ts ->
            case elements of
                StatusElements statuses ->
                    case LE.findIndex (\s -> s.id == ts) statuses of
                        Nothing ->
                            default

                        Just idx ->
                            idx

                NotificationElements notifications ->
                    case LE.findIndex (\s -> s.id == ts) notifications of
                        Nothing ->
                            default

                        Just idx ->
                            idx

                _ ->
                    default


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
                    , newcnt
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

        SetTargetLanguage targetLanguage ->
            let
                renderEnv =
                    model.renderEnv
            in
            { model
                | renderEnv =
                    { renderEnv
                        | targetLanguage =
                            if targetLanguage == "" then
                                Nothing

                            else
                                Just targetLanguage
                    }
            }
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

        SetContentType content_type ->
            { model | content_type = content_type }
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

        SendRequest request ->
            sendRequest request model


{-| Process Requests sent from the columns page.

These send requests over the wire to instances.

-}
explorerSendMsg : ExplorerSendMsg -> Model -> ( Model, Cmd Msg )
explorerSendMsg msg model =
    case msg of
        ReceiveResponse request result ->
            receiveResponse request result model

        ReceivePostResponse request result ->
            let
                postState =
                    model.postState

                dialog =
                    case model.dialog of
                        PostDialog ->
                            case result of
                                Err _ ->
                                    PostDialog

                                _ ->
                                    NoDialog

                        _ ->
                            model.dialog
            in
            receiveResponse request
                result
                { model
                    | postState = { postState | posting = False }
                    , dialog = dialog
                }

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
            sendGetFollowers (getAccountId model)
                (String.toInt model.pagingInput.limit)
                model

        SendGetFollowing ->
            sendGetFollowing (getAccountId model)
                (String.toInt model.pagingInput.limit)
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

        SendGetStatusAncestors ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusPartialContext
                        { which = Request.AncestorsContext
                        , id = model.statusId
                        , offset = Nothing
                        }
                )
                model

        SendGetStatusDescendants ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusPartialContext
                        { which = Request.DescendantsContext
                        , id = model.statusId
                        , offset = Nothing
                        }
                )
                model

        SendGetStatusSource ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusSource
                        { id = model.statusId
                        }
                )
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

        SendPostTranslate ->
            let
                targetLanguage =
                    defaultedTargetLanguage model.renderEnv
            in
            sendRequest
                (StatusesRequest <|
                    Request.PostTranslate
                        { id = model.statusId
                        , target_language = targetLanguage
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
                --{ model | dialog = NoDialog }
                model

        SendPostUnreblogStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostUnreblogStatus { id = model.statusId }
                )
                --{ model | dialog = NoDialog }
                model

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
                (StatusesRequest
                    (Request.PostStatus <| postedStatus model)
                )
                { model | dialog = NoDialog }

        SendPutStatus ->
            let
                statusId =
                    model.statusId
            in
            if statusId == "" then
                { model | msg = Just "'status id' may not be blank." }
                    |> withNoCmd

            else
                sendRequest
                    (StatusesRequest <|
                        Request.PutStatus
                            { id = statusId, status = editedStatus model }
                    )
                    { model | dialog = NoDialog }

        SendGetStatusHistory ->
            let
                statusId =
                    model.statusId
            in
            if statusId == "" then
                { model | msg = Just "'status id' may not be blank." }
                    |> withNoCmd

            else
                sendRequest
                    (StatusesRequest <|
                        Request.GetStatusHistory
                            { id = statusId }
                    )
                    model

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


postedStatus : Model -> PostedStatus
postedStatus model =
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
    , content_type = nothingIfBlank model.content_type
    , scheduled_at = nothingIfBlank model.scheduled_at
    , language = nothingIfBlank model.language
    , idempotencyKey = nothingIfBlank model.idempotencyKey
    }


editedStatus : Model -> EditedStatus
editedStatus model =
    { status = nothingIfBlank model.status
    , in_reply_to_id = nothingIfBlank model.in_reply_to_id
    , quote_of_id = nothingIfBlank model.quote_of_id
    , media_ids = Just <| splitMediaIds model.media_ids
    , sensitive = model.media_sensitive
    , spoiler_text = nothingIfBlank model.spoiler_text
    , visibility = model.visibility
    , content_type = Just "text/plain"
    , poll = pollDefinition model
    , scheduled_at = nothingIfBlank model.scheduled_at
    }


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
                                if flds == model.fields then
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
        getModelAccountId model


getModelAccountId : Model -> String
getModelAccountId model =
    case model.account of
        Nothing ->
            ""

        Just { id } ->
            id


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
                    model.renderEnv
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
                                    , undisplayedElements = NoUndisplayed
                                    , error = Nothing
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
                                |> updateFeedBodyPollSelections proFeed

                        _ ->
                            mdl


getAccountDialogAccount : String -> Model -> ( Model, Cmd Msg )
getAccountDialogAccount id model =
    case model.dialog of
        AccountDialog account _ ->
            if
                (account.id == getModelAccountId model)
                    || (account.id == id)
            then
                sendRequest
                    (AccountsRequest <|
                        Request.GetAccount { id = account.id }
                    )
                    model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


processReceivedAccount : Account -> Model -> ( Model, Cmd Msg )
processReceivedAccount account model =
    let
        accountId =
            Types.accountToAccountId account

        modelAccountId =
            getModelAccountId model

        isMyAccount =
            modelAccountId == account.id

        newModelAccount =
            if isMyAccount then
                Just account

            else
                model.account

        ( mdl, cmd ) =
            case model.dialog of
                AccountDialog acct content ->
                    let
                        newDialog =
                            if account.id /= acct.id then
                                model.dialog

                            else
                                AccountDialog account content

                        ( mdl2, cmd2 ) =
                            if account.id /= acct.id then
                                ( model, Cmd.none )

                            else
                                case content of
                                    Just (FollowingContent _) ->
                                        if isMyAccount then
                                            sendGetFollowing account.id
                                                (Just accountDialogRelationsCount)
                                                model

                                        else
                                            ( model, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )
                    in
                    ( { mdl2 | dialog = newDialog }
                    , cmd2
                    )

                d ->
                    ( model, Cmd.none )

        ( mdl3, cmd3 ) =
            case model.renderEnv.loginServer of
                Nothing ->
                    ( mdl, cmd )

                Just server ->
                    mergeAccountId accountId server mdl
    in
    { mdl3
        | account = newModelAccount
        , references =
            addAccountReference account mdl.references
    }
        |> withCmds [ cmd, cmd3 ]


decodeInstancePostFormats : Value -> Maybe (List String)
decodeInstancePostFormats value =
    case
        JD.decodeValue
            (JD.at [ "pleroma", "metadata", "post_formats" ] <| JD.list JD.string)
            value
    of
        Err _ ->
            Nothing

        Ok formats ->
            Just formats


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


stringToVersion : String -> Maybe Version
stringToVersion s =
    case String.split " " s of
        [] ->
            Nothing

        v :: _ ->
            case String.split "." v of
                maj :: (min :: (pat :: _)) ->
                    case String.toInt maj of
                        Nothing ->
                            Nothing

                        Just major ->
                            case String.toInt min of
                                Nothing ->
                                    Nothing

                                Just minor ->
                                    case String.toInt pat of
                                        Nothing ->
                                            Nothing

                                        Just patch ->
                                            Just <| Version major minor patch

                _ ->
                    Nothing


decodeInstanceFeatures : Value -> Maybe (List ( String, Bool ))
decodeInstanceFeatures value =
    case
        JD.decodeValue
            (JD.at [ "pleroma", "metadata", "features" ] <| JD.list JD.string)
            value
    of
        Ok features ->
            Just
                [ ( featureNames.editing
                  , List.member "editing" features
                  )
                , ( featureNames.quote_posting
                  , List.member "quote_posting" features
                  )
                , ( featureNames.translation
                  , List.member "translation" features
                  )
                ]

        Err _ ->
            let
                version =
                    case JD.decodeValue (JD.field "version" JD.string) value of
                        Err _ ->
                            Nothing

                        Ok s ->
                            stringToVersion s
            in
            case version of
                Nothing ->
                    Nothing

                Just { major, minor } ->
                    if
                        (major >= 4)
                            || (major == 3 && minor >= 5)
                    then
                        Just
                            [ ( featureNames.editing, True ) ]

                    else
                        Nothing


applyResponseSideEffects : Response -> Model -> Model
applyResponseSideEffects response model1 =
    let
        model =
            case response.entity of
                RelationshipListEntity relationships ->
                    { model1
                        | relationships =
                            List.foldr (\r res -> Dict.insert r.id r res)
                                model1.relationships
                                relationships
                    }

                RelationshipEntity relationship ->
                    { model1
                        | relationships =
                            Dict.insert relationship.id
                                relationship
                                model1.relationships
                    }

                _ ->
                    model1

        renderEnv =
            model.renderEnv
    in
    case response.request of
        InstanceRequest Request.GetInstance ->
            case renderEnv.loginServer of
                Nothing ->
                    model

                Just loginServer ->
                    case response.entity of
                        InstanceEntity instance ->
                            let
                                maybeFormats =
                                    decodeInstancePostFormats instance.v

                                postFormats =
                                    renderEnv.postFormats

                                mdl =
                                    if
                                        Dict.get loginServer postFormats
                                            == maybeFormats
                                    then
                                        model

                                    else
                                        { model
                                            | renderEnv =
                                                { renderEnv
                                                    | postFormats =
                                                        Debug.log "model.postFormats" <|
                                                            case maybeFormats of
                                                                Nothing ->
                                                                    Dict.remove loginServer postFormats

                                                                Just formats ->
                                                                    Dict.insert loginServer
                                                                        formats
                                                                        postFormats
                                                }
                                        }
                            in
                            case
                                Debug.log "instanceFeatures" <|
                                    decodeInstanceFeatures instance.v
                            of
                                Nothing ->
                                    mdl

                                Just features ->
                                    List.foldl
                                        (\( feature, bool ) mdl2 ->
                                            setServerHasFeature (Just loginServer)
                                                feature
                                                bool
                                                mdl2
                                        )
                                        mdl
                                        features

                        _ ->
                            model

        AccountsRequest Request.GetVerifyCredentials ->
            case response.entity of
                AccountEntity account ->
                    updatePatchCredentialsInputs model

                _ ->
                    model

        AccountsRequest (Request.GetAccountByUsername _) ->
            let
                mdl =
                    case renderEnv.loginServer of
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

        AccountsRequest (Request.GetAccount _) ->
            case response.entity of
                AccountEntity account ->
                    let
                        ( mdl, cmd ) =
                            processReceivedAccount account model
                    in
                    { mdl | sideEffectCmd = cmd }

                _ ->
                    model

        AccountsRequest (Request.PostFollow { id }) ->
            let
                ( mdl, cmd ) =
                    getAccountDialogAccount id model
            in
            { mdl
                | isAccountFollowed =
                    if id == model.accountId then
                        True

                    else
                        model.isAccountFollowed
                , sideEffectCmd =
                    cmd
            }

        AccountsRequest (Request.PostUnfollow { id }) ->
            let
                ( mdl, cmd ) =
                    getAccountDialogAccount id model
            in
            { mdl
                | isAccountFollowed =
                    if id == model.accountId then
                        False

                    else
                        model.isAccountFollowed
                , sideEffectCmd =
                    cmd
            }

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

        AccountsRequest (Request.GetRelationships _) ->
            case response.entity of
                RelationshipListEntity relationships ->
                    let
                        mdl =
                            updateFeedBodyRelationships relationships model
                    in
                    { mdl
                        | popupChoices =
                            case relationships of
                                [ relationship ] ->
                                    List.map
                                        (\choice ->
                                            case choice of
                                                CommandChoice command status ->
                                                    case command of
                                                        MuteCommand _ ->
                                                            CommandChoice
                                                                (MuteCommand <| Just relationship)
                                                                status

                                                        BlockCommand _ ->
                                                            CommandChoice
                                                                (BlockCommand <| Just relationship)
                                                                status

                                                        _ ->
                                                            choice

                                                _ ->
                                                    choice
                                        )
                                        model.popupChoices

                                _ ->
                                    model.popupChoices
                    }

                _ ->
                    model

        AccountsRequest (Request.GetFollowing { id }) ->
            case response.entity of
                AccountListEntity accounts ->
                    case model.dialog of
                        AccountDialog account (Just (FollowingContent _)) ->
                            let
                                ( mdl, cmd ) =
                                    setAccountDialog model
                                        (AccountDialog account <|
                                            Just (FollowingContent accounts)
                                        )
                            in
                            { mdl
                                | sideEffectCmd =
                                    case model.account of
                                        Nothing ->
                                            cmd

                                        Just acct ->
                                            if acct.id == id then
                                                cmd

                                            else
                                                Cmd.batch
                                                    [ cmd
                                                    , Task.perform ColumnsUIMsg <|
                                                        Task.succeed
                                                            (FetchRelationships accounts)
                                                    ]
                            }

                        _ ->
                            model

                _ ->
                    model

        AccountsRequest (Request.GetFollowers _) ->
            case response.entity of
                AccountListEntity accounts ->
                    case model.dialog of
                        AccountDialog account (Just (FollowersContent _)) ->
                            let
                                ( mdl, cmd ) =
                                    setAccountDialog model
                                        (AccountDialog account <|
                                            Just (FollowersContent accounts)
                                        )
                            in
                            { mdl
                                | sideEffectCmd =
                                    Cmd.batch
                                        [ cmd
                                        , Task.perform ColumnsUIMsg <|
                                            Task.succeed
                                                (FetchRelationships accounts)
                                        ]
                            }

                        _ ->
                            model

                _ ->
                    model

        AccountsRequest (Request.GetStatuses { paging }) ->
            let
                mdl =
                    statusSmartPaging response.entity paging model
            in
            case response.entity of
                StatusListEntity statuses ->
                    case mdl.dialog of
                        AccountDialog account maybeContent ->
                            case maybeContent of
                                Nothing ->
                                    mdl

                                Just (StatusesContent contentStatuses) ->
                                    let
                                        ( mdl2, cmd2 ) =
                                            setAccountDialog model
                                                (AccountDialog account <|
                                                    Just
                                                        (StatusesContent
                                                            { contentStatuses
                                                                | statuses = statuses
                                                            }
                                                        )
                                                )
                                    in
                                    { mdl2 | sideEffectCmd = cmd2 }

                                _ ->
                                    mdl

                        _ ->
                            mdl

                _ ->
                    mdl

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

                        choices =
                            case mdl.popup of
                                UserNamePopup ->
                                    List.map AccountChoice results

                                PostTextPopup search ->
                                    case search.popupType of
                                        PostPopupAtsign ->
                                            List.map AccountChoice results

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

        CustomEmojisRequest Request.GetCustomEmojis ->
            case response.entity of
                EmojiListEntity emojis ->
                    let
                        names =
                            List.map .shortcode (List.take 20 emojis)

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
                (Debug.log "Server supports groups" renderEnv.loginServer)
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

        MutesRequest (Request.PostStatusMute _) ->
            case response.entity of
                StatusEntity status ->
                    modifyColumnsStatus status.id (\_ -> status) model

                _ ->
                    model

        MutesRequest (Request.PostStatusUnmute _) ->
            case response.entity of
                StatusEntity status ->
                    modifyColumnsStatus status.id (\_ -> status) model

                _ ->
                    model

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

        StatusesRequest (Request.PostPinStatus _) ->
            case response.entity of
                StatusEntity status ->
                    modifyColumnsStatus status.id (\_ -> status) model

                _ ->
                    model

        StatusesRequest (Request.PostUnpinStatus _) ->
            case response.entity of
                StatusEntity status ->
                    modifyColumnsStatus status.id (\_ -> status) model

                _ ->
                    model

        StatusesRequest (Request.GetStatusHistory { id }) ->
            case model.dialog of
                StatusHistoryDialog status _ ->
                    case response.entity of
                        HistoryStatusListEntity history ->
                            { model
                                | dialog =
                                    StatusHistoryDialog status history
                            }

                        _ ->
                            model

                _ ->
                    model

        StatusesRequest (Request.PutStatus _) ->
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
                        , pollOptions = [ "", "" ]
                    }
                        |> adjustColumnsForPost status
                        |> updateColumnsStatus response.entity

                _ ->
                    model

        StatusesRequest (Request.PostTranslate { id }) ->
            case response.entity of
                TranslationEntity translation ->
                    { model
                        | renderEnv =
                            { renderEnv
                                | translationDict =
                                    Dict.insert id translation renderEnv.translationDict
                            }
                    }

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

        StatusesRequest (Request.DeleteStatus { id }) ->
            deleteColumnsStatus id model

        FavouritesRequest (Request.PostFavourite _) ->
            updateColumnsStatus response.entity model

        FavouritesRequest (Request.PostUnfavourite _) ->
            updateColumnsStatus response.entity model

        _ ->
            model


updateFeedBodyRelationships : List Relationship -> Model -> Model
updateFeedBodyRelationships relationships model =
    case
        findFeed Types.defaultNotificationFeedType
            model.feedSet
    of
        Nothing ->
            model

        Just feed ->
            let
                feedId =
                    Types.feedID feed.feedType
            in
            case
                Dict.get feedId
                    model.feedEnvs
            of
                Nothing ->
                    model

                Just feedEnv ->
                    let
                        bodyEnv =
                            feedEnv.bodyEnv

                        folder : Relationship -> Dict String Relationship -> Dict String Relationship
                        folder relationship res =
                            Dict.insert relationship.id
                                relationship
                                res

                        newFeedEnv =
                            { feedEnv
                                | bodyEnv =
                                    { bodyEnv
                                        | relationships =
                                            List.foldl folder
                                                bodyEnv.relationships
                                                relationships
                                    }
                            }
                    in
                    { model
                        | feedEnvs =
                            Dict.insert feedId
                                newFeedEnv
                                model.feedEnvs
                    }


{-| Just received an update to a StatusEntity.

That happens when a reply is posted (on refetch of the replied-to
post), or the user clicks on a posts's reblog or favorite button.
Or when a Poll is refreshed.

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

        dialog =
            case model.dialog of
                AccountDialog account maybeContent ->
                    case maybeContent of
                        Just (StatusesContent adStatuses) ->
                            let
                                ( statuses, changed ) =
                                    modifyStatuses id modifier adStatuses.statuses
                            in
                            if not changed then
                                model.dialog

                            else
                                AccountDialog account
                                    (Just <|
                                        StatusesContent
                                            { adStatuses | statuses = statuses }
                                    )

                        _ ->
                            model.dialog

                d ->
                    d
    in
    { model
        | feedSet =
            { feedSet
                | feeds =
                    List.map (modifyFeedStatus id modifier) feedSet.feeds
            }
        , popupExplorer = popupExplorer
        , dialog = dialog
    }


deleteColumnsStatus : String -> Model -> Model
deleteColumnsStatus id model =
    -- Not quite right. References inside statuses are not removed.
    -- Let them report it as a bug.
    let
        popupExplorer =
            case model.popupExplorer of
                ThreadPopupExplorer state ->
                    let
                        deleteFromDisplayed scrolledStatus =
                            { scrolledStatus
                                | displayed =
                                    LE.filterNot (.id >> (==) id)
                                        scrolledStatus.displayed
                            }

                        ribbon =
                            LE.filterNot (.status >> .id >> (==) id)
                                state.ribbon
                    in
                    { state
                        | ribbon =
                            List.map deleteFromDisplayed ribbon
                    }
                        |> ThreadPopupExplorer

                explorer ->
                    explorer

        dialog =
            case model.dialog of
                AccountDialog account maybeContent ->
                    case maybeContent of
                        Just (StatusesContent adStatuses) ->
                            AccountDialog account
                                (Just <|
                                    StatusesContent
                                        { adStatuses
                                            | statuses =
                                                LE.filterNot (.id >> (==) id)
                                                    adStatuses.statuses
                                        }
                                )

                        _ ->
                            model.dialog

                d ->
                    d

        deleteFromFeedElements : FeedElements -> ( FeedElements, Bool )
        deleteFromFeedElements feedElements =
            case feedElements of
                StatusElements statuses ->
                    let
                        newStatuses =
                            LE.filterNot (.id >> (==) id) statuses
                    in
                    if List.length statuses == List.length newStatuses then
                        ( feedElements, False )

                    else
                        ( StatusElements newStatuses, True )

                _ ->
                    ( feedElements, False )

        deleteFromFeed : Feed -> Feed
        deleteFromFeed feed =
            let
                { elements, undisplayedElements } =
                    feed

                ( newElements, changed ) =
                    deleteFromFeedElements elements

                ( newUndisplayedElements, udChanged ) =
                    case undisplayedElements of
                        Undisplayed udElements ->
                            let
                                ( newUdElements, newUdChanged ) =
                                    deleteFromFeedElements udElements
                            in
                            if newUdChanged then
                                ( Undisplayed newUdElements, True )

                            else
                                ( undisplayedElements, False )

                        _ ->
                            ( undisplayedElements, False )
            in
            if changed || udChanged then
                { feed
                    | elements = newElements
                    , undisplayedElements = newUndisplayedElements
                }

            else
                feed

        feedSet =
            model.feedSet
    in
    { model
        | feedSet =
            { feedSet
                | feeds =
                    List.map deleteFromFeed feedSet.feeds
            }
        , popupExplorer = popupExplorer
        , dialog = dialog
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
            case serverKnowsFeature maybeServer featureNames.quote model.renderEnv of
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


sendPostRequest : Request -> Model -> ( Model, Cmd Msg )
sendPostRequest request =
    sendGeneralRequest (ExplorerSendMsg << ReceivePostResponse request) request


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


saveAuthorization : String -> Authorization -> Model -> ( Maybe String, Model, Cmd Msg )
saveAuthorization server authorization model =
    let
        tokens =
            model.tokens

        tokenApi =
            case Dict.get server tokens of
                Nothing ->
                    { emptyTokenApi | token = Just authorization.token }

                Just api ->
                    { api | token = Just authorization.token }

        ( newTokenApi, maybeStreaming_api, cmd ) =
            case tokenApi.api of
                UnknownApi ->
                    case model.lastInstance of
                        Nothing ->
                            ( tokenApi, Nothing, getInstance model )

                        Just lastInstance ->
                            if lastInstance.server == server then
                                case lastInstance.instance.urls of
                                    Just { streaming_api } ->
                                        ( { tokenApi
                                            | api = UrlApi streaming_api
                                          }
                                        , Just streaming_api
                                        , Cmd.none
                                        )

                                    Nothing ->
                                        ( tokenApi
                                        , Nothing
                                        , getInstance model
                                        )

                            else
                                ( tokenApi, Nothing, getInstance model )

                _ ->
                    ( tokenApi, Nothing, Cmd.none )

        mdl =
            { model
                | tokens =
                    Dict.insert server newTokenApi tokens
            }
    in
    ( maybeStreaming_api
    , mdl
    , Cmd.batch
        [ putToken server <| Just newTokenApi
        , cmd
        , fetchFeatures server mdl
        ]
    )



---
--- Persistence
---


put : String -> Maybe Value -> Cmd Msg
put key value =
    Cmd.batch
        [ localStorageSend (LocalStorage.put (Debug.log "put" key) value)
        , Task.perform ColumnsUIMsg <| Task.succeed <| DynamoDBSave key value
        ]


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


putToken : String -> Maybe TokenApi -> Cmd Msg
putToken server tokenApi =
    put (tokenStorageKey server) <|
        case tokenApi of
            Nothing ->
                Nothing

            Just tok ->
                Just <| encodeTokenApi tok


appStorageKey : String -> String
appStorageKey server =
    pk.app ++ "." ++ server


appStorageKeyServer : String -> String
appStorageKeyServer key =
    String.dropLeft (String.length pk.app + 1) key


getApp : String -> Cmd Msg
getApp server =
    getLabeled pk.app <| appStorageKey server


putApp : String -> Maybe App -> Cmd Msg
putApp server maybeApp =
    put (appStorageKey server) <|
        case maybeApp of
            Nothing ->
                Nothing

            Just app ->
                Just <| ED.encodeApp app


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


getMaxTootChars : String -> Cmd Msg
getMaxTootChars server =
    get <| pk.maxTootChars ++ "." ++ server


putMaxTootChars : String -> Maybe String -> Cmd Msg
putMaxTootChars server maxTootChars =
    let
        v =
            case maxTootChars of
                Nothing ->
                    Nothing

                Just s ->
                    Just <| JE.string s
    in
    put (pk.maxTootChars ++ "." ++ server) v


getTimestamps : String -> Cmd Msg
getTimestamps server =
    getLabeled pk.timestamps <| pk.timestamps ++ "." ++ server


putTimestamps : String -> Maybe (Dict String String) -> Cmd Msg
putTimestamps server statusids =
    let
        v =
            case statusids of
                Nothing ->
                    Nothing

                Just sids ->
                    Just <| JE.dict identity JE.string sids
    in
    put (pk.timestamps ++ "." ++ server) v


putDynamoDBAccount : Maybe DynamoDB.Types.Account -> Cmd Msg
putDynamoDBAccount account =
    let
        v =
            case account of
                Nothing ->
                    Nothing

                Just acct ->
                    Just <| DynamoDB.encodeAccount acct
    in
    put pk.dynamoDBAccount v


getTimestamp : String -> Cmd Msg
getTimestamp feedid =
    getLabeled pk.timestamp <| pk.timestamp ++ "." ++ feedid


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
    let
        appState =
            AppState.makeAppState emptyAppStateAccount
    in
    { model = "model"
    , token = "token"
    , feedSetDefinition = "feedSetDefinition"
    , accountIds = "accountIds"
    , maxTootChars = "maxTootChars"
    , timestamp = "timestamp"
    , timestamps = "timestamps"
    , app = "app"

    -- This one is not saved to DynamoDB
    , dynamoDBAccount = "dynamoDBAccount"

    -- These are here to remind me not to use them
    -- They have "DynamoDB." in front of them, so should be safe.
    , appStateSaveCount = appState.saveCountKey
    , appStateKeyCounts = appState.keyCountsKey
    }


pkToUpdater : Dict String (String -> Maybe String -> Model -> ( Model, Cmd Msg ))
pkToUpdater =
    Dict.fromList
        [ ( pk.model, updateModel )
        , ( pk.token, updateToken )
        , ( pk.feedSetDefinition, updateFeedSetDefinition )
        , ( pk.accountIds, updateAccountIds )
        , ( pk.maxTootChars, updateMaxTootChars )
        , ( pk.timestamp, updateTimestamp )
        , ( pk.timestamps, updateTimestamps )
        , ( pk.app, updateApp )
        ]


debugAndAppend : String -> x -> Maybe String
debugAndAppend label x =
    Just <|
        let
            x2 =
                Debug.log label x
        in
        if label == "" then
            Debug.toString x ++ "."

        else
            label ++ ": " ++ Debug.toString x


updateModel : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateModel key value model =
    case value of
        Nothing ->
            { model | msg = debugAndAppend "" "WTF! model deleted in DynamoDB" }
                |> withNoCmd

        Just v ->
            case JD.decodeString savedModelDecoder v of
                Err err ->
                    { model
                        | msg = debugAndAppend "Error decoding model" err
                    }
                        |> withNoCmd

                Ok savedModel ->
                    let
                        mdl =
                            savedModelToModel savedModel model
                    in
                    mdl
                        |> withCmd
                            (put pk.model (Just <| encodeSavedModel savedModel))


updateToken : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateToken server value model =
    let
        result =
            case value of
                Nothing ->
                    Ok Nothing

                Just v ->
                    JD.decodeString tokenApiDecoder v
                        |> Result.map Just
    in
    case result of
        Err err ->
            { model
                | msg =
                    debugAndAppend ("Error decoding " ++ tokenStorageKey server) err
            }
                |> withNoCmd

        Ok maybeTokenApi ->
            case maybeTokenApi of
                Nothing ->
                    -- Token deleted.
                    if Just server == model.renderEnv.loginServer then
                        model
                            |> withCmd
                                (Task.perform GlobalMsg <| Task.succeed Logout)

                    else
                        { model | tokens = Dict.remove server model.tokens }
                            |> withNoCmd

                Just tokenApi ->
                    { model | tokens = Dict.insert server tokenApi model.tokens }
                        |> withNoCmd


updateFeedSetDefinition : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateFeedSetDefinition key value model =
    model |> withNoCmd


updateAccountIds : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateAccountIds key value model =
    model |> withNoCmd


updateMaxTootChars : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateMaxTootChars key value model =
    model |> withNoCmd


updateTimestamp : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateTimestamp key value model =
    model |> withNoCmd


updateTimestamps : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateTimestamps key value model =
    model |> withNoCmd


updateApp : String -> Maybe String -> Model -> ( Model, Cmd Msg )
updateApp key value model =
    model |> withNoCmd


pkTimestampLength : Int
pkTimestampLength =
    String.length pk.timestamp


pkTimestampsLength : Int
pkTimestampsLength =
    String.length pk.timestamps

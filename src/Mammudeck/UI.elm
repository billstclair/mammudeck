----------------------------------------------------------------
--
-- UI.elm
-- User interface for Mammudeck.
-- Copyright (c) 2019-2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------


module Mammudeck.UI exposing
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
    , serverKnowsFeature
    , setServerHasFeature
    , threadExplorerStatusId
    , usernameAtServer
    , view
    )

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import CustomElement.BodyColors as BodyColors
import CustomElement.RenderNotify as RenderNotify
import CustomElement.TextAreaTracker as TextAreaTracker exposing (Coordinates)
import CustomElement.WatchColorScheme as WatchColorScheme exposing (ColorScheme(..))
import CustomElement.WriteClipboard as WriteClipboard
import Dialog
import Dict exposing (Dict)
import DropZone
import DynamoDB.AppState as AppState exposing (AppState, Updates)
import File exposing (File)
import FormatNumber
import FormatNumber.Locales
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
        , label
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
        , video
        )
import Html.Attributes
    exposing
        ( alt
        , autocomplete
        , autofocus
        , autoplay
        , checked
        , class
        , cols
        , colspan
        , controls
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
import Html.Parser exposing (Node(..))
import Html.Parser.Util as Util
import Http
import Iso8601
import Json.Decode as JD exposing (Decoder)
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
        , makeFeedEnv
        , markdownContentType
        , modelToSavedModel
        , plainTextContentType
        , savedModelDecoder
        , savedModelToModel
        , selectedRequestToString
        )
import Mammudeck.TimeUntil as TimeUntil exposing (timeUntil)
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
        , NotificationFeedParams
        , PublicFeedFlags
          --, Renderer
        , ScrollNotification
        , UndisplayedElements(..)
        , UserFeedFlags
        , UserFeedParams
        , makeUserFeed
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
        , HistoryStatus
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
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , FieldUpdate
        , Paging
        , RawRequest
        , Request(..)
        , Response
        , WhichGroups
        , emptyPaging
        )
import PopupPicker exposing (PopupPicker)
import Regex
import Set exposing (Set)
import String.Extra as SE
import Svg exposing (Svg, svg)
import Svg.Attributes as Svga
import Svg.Button as Button exposing (Button, TriangularButtonDirection(..))
import Task exposing (Task)
import Time exposing (Month, Posix, Zone)
import Time.Extra as TE exposing (Interval(..))
import Time.Format as Format
import Time.Format.Config.Configs as Configs
import Url exposing (Url)


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
    , borderColor : String
    , darkGrayColor : String
    }


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
        , borderColor = "#555"
        , darkGrayColor = "#888"
        }
    , light =
        { backgroundColor = "white"
        , inputBackground = "white"
        , color = "black"
        , popupChoiceClass = "popup-choice-light"
        , highlightStatusColor = "#fed8b1"
        , repliedToStatusColor = "#eee"
        , visitedStatusColor = "#ececec"
        , borderColor = "#ddd"
        , darkGrayColor = "#777"
        }
    }


getStyle : RenderEnv -> StyleProperties
getStyle renderEnv =
    case renderEnv.style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light

        SystemStyle ->
            case renderEnv.colorScheme of
                Nothing ->
                    styles.light

                Just LightColorScheme ->
                    styles.light

                Just DarkColorScheme ->
                    styles.dark


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
    , onClick : Maybe Msg
    }


imageLink : ImageSpec -> Html Msg
imageLink spec =
    a
        ([ href spec.linkUrl ]
            ++ (case spec.onClick of
                    Nothing ->
                        [ blankTarget ]

                    Just onc ->
                        [ onClick onc ]
               )
        )
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
        , style "border-radius" "9999px"
        , style "border-width" "1px"
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
            getStyle model.renderEnv
    in
    { title = "Mammudeck"
    , body =
        [ BodyColors.bodyColors
            [ BodyColors.color color
            , BodyColors.backgroundColor backgroundColor
            ]
            []
        , WatchColorScheme.watchColorScheme
            [ WatchColorScheme.onChange (GlobalMsg << SetColorScheme) ]
            []
        , renderPopupExplorer model
        , case model.attachmentView of
            Nothing ->
                text ""

            Just attachmentView ->
                attachmentDialog attachmentView model
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
            getStyle model.renderEnv
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


styleToLabel : Style -> String
styleToLabel style =
    case style of
        DarkStyle ->
            "Dark"

        LightStyle ->
            "Light"

        SystemStyle ->
            "System"


labelToStyle : String -> Maybe Style
labelToStyle label =
    case label of
        "Dark" ->
            Just DarkStyle

        "Light" ->
            Just LightStyle

        "System" ->
            Just SystemStyle

        _ ->
            Nothing


styleOption : Style -> Style -> Html Msg
styleOption currentStyle style =
    let
        label =
            styleToLabel style
    in
    option
        [ value label
        , selected <| style == currentStyle
        ]
        [ text label ]


styleSelect : Style -> Html Msg
styleSelect currentStyle =
    select [ onInput (ColumnsUIMsg << SetStyle) ]
        [ option [ value "" ] [ text "-- select style --" ]
        , styleOption currentStyle SystemStyle
        , styleOption currentStyle LightStyle
        , styleOption currentStyle DarkStyle
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
        , if renderEnv.loginServer == Nothing then
            p []
                [ text "Enter a 'server' name and click 'Login'."
                ]

          else
            primaryServerLine model
        , loginSelectedUI False model
        , p [ style "color" "red" ]
            [ Maybe.withDefault "" model.msg |> text ]
        , Markdown.toHtml []
            """
Mammudeck is a TweetDeck-like columnar interface to Mastodon/Pleroma. It is a work in progress. Keep an eye on the "Columns" page for new features. Use the "API Explorer" page to do low-level API hacking.

[Wikipedia says](https://en.wikipedia.org/wiki/Mastodon) that "Mastodons... are any species of extinct proboscideans in the genus Mammut (family Mammutidae), distantly related to elephants..." I removed the ending "t" from "Mammut" and added "deck" to get "Mammudeck".

Help is avaiable in the [Help](#help.intro) dialog.

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

Mammudeck is a labor of love, but I wouldn't at all mind earning some income from it. That can only happen if you, my customers, support me. If you use it, and like it, please donate at [paypal.me/billstclair](https://www.paypal.me/billstclair).
            """
        , p [ style "text-align" "center" ]
            [ b "style: "
            , styleSelect renderEnv.style
            , br
            , link "@billstclair@impeccable.social"
                "https://impeccable.social/billstclair"
            , br
            , text <| "Copyright " ++ special.copyright ++ " 2019-2022, Bill St. Clair"
            , br
            , imageLink
                { imageUrl = "images/elm-logo-125x125.png"
                , linkUrl = "https://elm-lang.org/"
                , altText = "Elm Inside"
                , borderColor = Nothing
                , h = "32px"
                , onClick = Nothing
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
                , onClick = Nothing
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
            getStyle renderEnv
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
            [ text "style"
            , br
            , styleSelect renderEnv.style
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
            [ button (ColumnsUIMsg ShowDocsDialog) "help" ]
        , p []
            [ button (ColumnsUIMsg ShowEditColumnsDialog) "edit" ]
        , p []
            [ button (ColumnsUIMsg ShowSettingsDialog) "settings" ]
        , p []
            [ button (ColumnsUIMsg ShowDynamoDBDialog) "dynamoDB" ]
        , p []
            [ button (ColumnsUIMsg ShowSaveRestoreDialog) "save" ]
        , p []
            [ button (ColumnsUIMsg ShowKeyboardShortcutsDialog) "keyboard" ]
        , p []
            [ button (ColumnsUIMsg ReloadAllColumns) "reload" ]
        , p []
            [ button (ColumnsUIMsg ShowAllUndisplayed) "display all" ]
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
            getStyle renderEnv
    in
    [ p []
        [ primaryServerLine model ]
    , p []
        [ loginSelectedUI False model ]
    , p [] [ pageSelector True (renderEnv.loginServer /= Nothing) ColumnsPage ]
    , p [] [ b "Actions:" ]
    , p []
        [ button (ColumnsUIMsg ShowDocsDialog)
            "Help Dialog"
        , br
        , button (ColumnsUIMsg ShowEditColumnsDialog)
            "Edit Columns Dialog"
        , br
        , button (ColumnsUIMsg ShowDynamoDBDialog)
            "DynamoDB Dialog"
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
        , button (ColumnsUIMsg ShowAllUndisplayed)
            "Show All Undisplayed"
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
                [ text "style"
                , br
                , styleSelect renderEnv.style
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
        [ button (ColumnsUIMsg ReloadFromServer) "Reload from Server"
        , br
        , checkBox (ColumnsUIMsg ToggleShowAccountDialogId)
            renderEnv.showIds
            "Show ids"
        , br
        , br
        , text "Reload the page after doing this:"
        , br
        , button (ColumnsUIMsg ClearFeatures) "Clear saved server features"
        , br
        , br
        , text "Will ask you to confirm before clearing EVERYTHING:"
        , br
        , button (GlobalMsg ClearAllDialog) "Clear all persistent state!"
        ]
    ]


statusHistoryDialog : Status -> List HistoryStatus -> Model -> Html Msg
statusHistoryDialog status history model =
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
        , title = "Status History"
        , content =
            statusHistoryContent status history model
        , actionBar =
            [ button (ColumnsUIMsg CommitDynamoDBDialog) "OK"
            ]
        }
        True


statusHistoryContent : Status -> List HistoryStatus -> Model -> List (Html Msg)
statusHistoryContent status history model =
    let
        renderEnv =
            model.renderEnv

        cnt =
            List.length history

        { borderColor } =
            getStyle renderEnv

        renderHistory : Int -> HistoryStatus -> Html Msg
        renderHistory index hs =
            let
                idxString =
                    String.fromInt (cnt - index) ++ ": "

                dateString =
                    formatIso8601 renderEnv.here hs.created_at
            in
            div
                [ style "width" "100%"
                , style "border" <| "2px solid " ++ borderColor
                , style "padding" "0 3px"
                ]
            <|
                span [ style "font-size" "90%" ]
                    [ b idxString
                    , text dateString
                    , br
                    ]
                    :: statusBody renderEnv (Just status) hs.content Nothing
    in
    [ div [ id nodeIds.statusHistoryDialog ] <|
        floatingDismissDialogButton model (Just ( 0, -50 ))
            :: List.indexedMap renderHistory (List.reverse history)
    ]


dynamoDBDialog : Model -> Html Msg
dynamoDBDialog model =
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
        , title = "DynamoDB Persistence"
        , content =
            dynamoDBDialogContent model
        , actionBar =
            [ button (ColumnsUIMsg CommitDynamoDBDialog) "OK"
            , button (ColumnsUIMsg DismissDialog) "Cancel"
            ]
        }
        True


dynamoDBDialogContent : Model -> List (Html Msg)
dynamoDBDialogContent model =
    let
        renderEnv =
            model.renderEnv

        { inputBackground, color } =
            getStyle renderEnv
    in
    [ p []
        [ text "This information is stored in your brower's LocalStorage database."
        ]
    , p []
        [ text "To disable DynamoDB storage, and remove the information from LocalStorage, clear "
        , b "tableName"
        , text ", "
        , b "accessKey"
        , text " or "
        , b "secretKey"
        , text "."
        ]
    , p []
        [ AppState.renderAccount
            (\a -> ColumnsUIMsg <| SetAppStateAccount a)
            model.appStateAccount
        ]
    ]


fullUsernameAtServer : Bool -> String -> String -> RenderEnv -> String
fullUsernameAtServer alwaysIncludeServer username server renderEnv =
    if
        (server == "")
            || (not alwaysIncludeServer
                    && (Just server == renderEnv.loginServer)
               )
    then
        username

    else
        username ++ "@" ++ server


usernameAtServer : String -> String -> RenderEnv -> String
usernameAtServer =
    fullUsernameAtServer False


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


changeFeedTypeParamsMsg : FeedType -> Maybe Msg
changeFeedTypeParamsMsg feedType =
    let
        msg =
            Just (ColumnsUIMsg <| ShowFeedTypePopup feedType)
    in
    case feedType of
        UserFeed _ ->
            msg

        PublicFeed _ ->
            msg

        NotificationFeed _ ->
            msg

        _ ->
            Nothing


headerFeedId : String -> String
headerFeedId feedId =
    feedId ++ " [header]"


undisplayedElementsColor : String
undisplayedElementsColor =
    "red"


renderFeed : Bool -> RenderEnv -> FeedEnv -> Feed -> Html Msg
renderFeed isFeedLoading renderEnv feedEnv feed =
    let
        bodyEnv =
            feedEnv.bodyEnv

        feedType =
            feed.feedType

        { color, borderColor } =
            getStyle renderEnv

        ( _, h ) =
            renderEnv.windowSize

        innerHeight =
            --Debug.log "  innerHeight" <|
            case feedEnv.headerHeight of
                Nothing ->
                    "calc(100% - " ++ defaultColumnHeaderHeight ++ ")"

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
                    case bodyEnv.group of
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

                UserFeed params ->
                    a
                        [ href "#"
                        , onClick (ColumnsUIMsg <| ShowUserDialog params)
                        ]
                        [ feedTitle feedType ]

                _ ->
                    feedTitle feedType
    in
    div
        [ style "height" <| px (h - 20)
        , style "width" <| px renderEnv.columnWidth
        , style "border" <| "1px solid " ++ borderColor
        ]
        [ div
            [ style "border" <| "1px solid " ++ borderColor
            , style "text-align" "center"
            , style "color" color
            , id <| headerFeedId feedId
            ]
            [ if isFeedLoading then
                feedLoadingEmojiSpan True True

              else
                case feed.undisplayedElements of
                    NoUndisplayed ->
                        Html.i
                            [ style "font-size" smallTextFontSize
                            , class "icon-spin4"
                            ]
                            []

                    NeverUndisplayed ->
                        Html.i
                            [ onClick <| ColumnsUIMsg (RefreshFeed feedType)
                            , style "cursor" "pointer"
                            , style "font-size" smallTextFontSize
                            , class "icon-spin3"
                            ]
                            []

                    Undisplayed elements ->
                        let
                            count =
                                Types.feedElementsCount elements
                        in
                        span
                            [ onClick (ColumnsUIMsg <| ShowUndisplayed feedType)
                            , style "cursor" "pointer"
                            , style "color" undisplayedElementsColor
                            , Html.Attributes.title "Show undisplayed."
                            ]
                            [ text <|
                                special.nbsp
                                    ++ "+"
                                    ++ String.fromInt count
                                    ++ special.nbsp
                            ]
            , text " "
            , title
            , case changeFeedTypeParamsMsg feedType of
                Nothing ->
                    text ""

                Just msg ->
                    span []
                        [ text " "
                        , Html.i
                            [ onClick msg
                            , style "cursor" "pointer"
                            , style "font-size" smallTextFontSize
                            , class "icon-down-open"
                            ]
                            []
                        ]
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
                    case bodyEnv.group of
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
            , renderNewElementsRow feed.newElements feedType feedEnv

            --, renderUndisplayedElementsRow feed.undisplayedElements feedType feedEnv
            ]
        , div
            [ style "overflow-y" "auto"
            , style "overflow-x" "hidden"
            , id feedId
            , style "height" innerHeight
            ]
          <|
            [ Lazy.lazy6 renderFeedElements
                feed.error
                feed.newElements
                feedType
                renderEnv
                bodyEnv
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


renderUndisplayedElementsRow : UndisplayedElements -> FeedType -> FeedEnv -> Html Msg
renderUndisplayedElementsRow undisplayedElements feedType feedEnv =
    case undisplayedElements of
        Undisplayed elements ->
            let
                count =
                    Types.feedElementsCount elements
            in
            span
                [ style "text-align" "center" ]
                [ br
                , span
                    [ style "color" "green"
                    , style "cursor" "pointer"
                    , Html.Attributes.title "Show undisplayed."
                    , onClick (ColumnsUIMsg <| ShowUndisplayed feedType)
                    ]
                    [ text <|
                        special.nbsp
                            ++ String.fromInt count
                            ++ special.nbsp
                            ++ "undisplayed"
                            ++ special.nbsp
                    ]
                ]

        _ ->
            text ""


renderNewElementsRow : Int -> FeedType -> FeedEnv -> Html Msg
renderNewElementsRow newElements feedType feedEnv =
    let
        newElementsLeft =
            feedEnv.newElementsLeft

        newColumnsLeft =
            feedEnv.newColumnsLeft

        newElementsRight =
            feedEnv.newElementsRight

        newColumnsRight =
            feedEnv.newColumnsRight
    in
    if
        (newElements <= 0)
            && (newElementsLeft <= 0)
            && (newElementsRight <= 0)
    then
        text ""

    else
        span
            [ style "text-align" "center" ]
            [ br
            , if newElementsLeft <= 0 then
                text ""

              else
                a
                    [ href "#"
                    , Html.Attributes.title "Scroll left to new post."
                    , onClick (ColumnsUIMsg <| ScrollToNew ScrollLeft feedType)
                    ]
                    [ text <|
                        special.nbsp
                            ++ String.fromInt newElementsLeft
                            ++ "/"
                            ++ String.fromInt newColumnsLeft
                            ++ special.nbsp
                            ++ "<"
                            ++ special.nbsp
                    ]
            , if newElements <= 0 then
                text <| special.nbsp ++ special.nbsp

              else
                span
                    [ style "color" "red"
                    , style "cursor" "pointer"
                    , Html.Attributes.title "Mark read."
                    , onClick (ColumnsUIMsg <| MarkFeedRead feedType)
                    ]
                    [ text <|
                        special.nbsp
                            ++ String.fromInt newElements
                            ++ special.nbsp
                            ++ "new"
                            ++ special.nbsp
                    ]
            , if newElementsRight <= 0 then
                text ""

              else
                a
                    [ href "#"
                    , Html.Attributes.title "Scroll left to new post."
                    , onClick (ColumnsUIMsg <| ScrollToNew ScrollRight feedType)
                    ]
                    [ text <|
                        special.nbsp
                            ++ ">"
                            ++ special.nbsp
                            ++ String.fromInt newElementsRight
                            ++ "/"
                            ++ String.fromInt newColumnsRight
                            ++ special.nbsp
                    ]
            ]


renderFeedElements : Maybe String -> Int -> FeedType -> RenderEnv -> FeedBodyEnv -> FeedElements -> Html Msg
renderFeedElements error newElements feedType renderEnv bodyEnv elements =
    let
        feedId =
            Types.feedID <| Debug.log "  renderFeedElements" feedType

        errorElements =
            case error of
                Nothing ->
                    []

                Just msg ->
                    let
                        { borderColor } =
                            getStyle renderEnv
                    in
                    [ div [ style "border" <| "1px solid " ++ borderColor ]
                        [ p [ style "color" "red" ]
                            [ text msg ]
                        ]
                    ]
    in
    case elements of
        StatusElements statuses ->
            div [] <|
                List.concat
                    [ errorElements
                    , List.indexedMap
                        (renderStatusWithNewMarker feedType renderEnv bodyEnv newElements)
                        statuses
                    ]

        NotificationElements notifications ->
            let
                ( gangedNotifications, newElements2 ) =
                    gangNotifications newElements notifications
            in
            div [] <|
                List.concat
                    [ errorElements
                    , List.indexedMap
                        (renderGangedNotificationWithNewMarker feedType
                            renderEnv
                            bodyEnv
                            newElements2
                        )
                        gangedNotifications
                    ]

        _ ->
            text ""


renderGangedNotificationWithNewMarker : FeedType -> RenderEnv -> FeedBodyEnv -> Int -> Int -> GangedNotification -> Html Msg
renderGangedNotificationWithNewMarker feedType renderEnv bodyEnv newElements index gangedNotification =
    let
        feedId =
            Types.feedID feedType
    in
    if newElements > 0 && newElements == index then
        div []
            [ renderNewMarker feedType renderEnv
            , renderGangedNotification feedId renderEnv bodyEnv index gangedNotification
            ]

    else
        renderGangedNotification feedId renderEnv bodyEnv index gangedNotification


renderGangedNotification : String -> RenderEnv -> FeedBodyEnv -> Int -> GangedNotification -> Html Msg
renderGangedNotification ellipsisPrefix renderEnv bodyEnv index gangedNotification =
    let
        notification =
            gangedNotification.notification
    in
    case gangedNotification.accounts of
        account :: others ->
            if others == [] then
                renderNotification renderEnv bodyEnv ellipsisPrefix index notification

            else
                renderMultiNotification renderEnv
                    bodyEnv
                    account
                    others
                    ellipsisPrefix
                    index
                    notification

        _ ->
            renderNotification renderEnv bodyEnv ellipsisPrefix index notification


renderMultiNotification : RenderEnv -> FeedBodyEnv -> Account -> List Account -> String -> Int -> Notification -> Html Msg
renderMultiNotification renderEnv bodyEnv account others ellipsisPrefix index notification =
    let
        { color, borderColor } =
            getStyle renderEnv

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
        [ style "border" <| "1px solid " ++ borderColor
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
                        , linkUrl = "#"
                        , altText = "Show account dialog for @" ++ other.acct
                        , borderColor =
                            if other.is_pro then
                                Just "gold"

                            else
                                Nothing
                        , h = "1.5em"
                        , onClick =
                            Just <| (ColumnsUIMsg <| ShowAccountDialog other)
                        }
                )
                (account :: others)
                |> List.intersperse (text " ")
                |> span []
            ]
        , renderNotificationBody renderEnv bodyEnv notification.id ellipsisPrefix index notification
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
                                    && (if car.type_ == Pleroma_EmojiReactionNotification then
                                            car.emoji == gn.notification.emoji

                                        else
                                            True
                                       )
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

        FollowRequestNotification ->
            span [] [ displayHtml, text " requested to follow you" ]

        UpdateNotification ->
            span [] [ displayHtml, text " edited a status" ]

        Admin_SignupNotification ->
            span [] [ displayHtml, text " signed up" ]

        Admin_ReportNotification ->
            span [] [ displayHtml, text " made a report" ]

        Pleroma_EmojiReactionNotification ->
            span []
                [ displayHtml
                , text " reacted to "
                , postName
                , case notification.emoji of
                    Nothing ->
                        text ""

                    Just emoji ->
                        span []
                            [ text ": "
                            , text emoji
                            ]
                ]

        UnknownNotification name ->
            span [] [ displayHtml, text <| " unknown notification type: " ++ name ]


headerFontSize : String
headerFontSize =
    "90%"


headerFontSizeStyle : Attribute msg
headerFontSizeStyle =
    style "font-size" headerFontSize


renderNotification : RenderEnv -> FeedBodyEnv -> String -> Int -> Notification -> Html Msg
renderNotification renderEnv bodyEnv ellipsisPrefix index notification =
    let
        description =
            notificationDescription notification renderEnv

        { color, borderColor } =
            getStyle renderEnv
    in
    div [ style "border" <| "1px solid " ++ borderColor ]
        [ div []
            [ div [ headerFontSizeStyle ]
                [ renderAccount renderEnv
                    notification.account
                    description
                    False
                    (Just notification.created_at)
                    Nothing
                ]
            , renderNotificationBody renderEnv bodyEnv notification.id ellipsisPrefix index notification
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

        loop : List String -> List EmojiOrTextString -> List EmojiOrTextString
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

                [ last ] ->
                    List.reverse <|
                        case res of
                            (TextString x) :: more ->
                                TextString (x ++ ":" ++ last) :: more

                            _ ->
                                TextString last :: res

                first :: rest ->
                    if validEmojiName first then
                        case rest of
                            car :: cdr ->
                                loop cdr <|
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


replaceEmojiReferences : RenderEnv -> Maybe Status -> List Html.Parser.Node -> List Html.Parser.Node
replaceEmojiReferences renderEnv maybeStatus nodes =
    let
        size =
            18 * renderEnv.fontSizePct // 100

        sizeStr =
            String.fromInt size ++ "px"

        emojis =
            case maybeStatus of
                Nothing ->
                    renderEnv.emojis

                Just status ->
                    List.foldl (\e es -> Dict.insert e.shortcode e es)
                        renderEnv.emojis
                        status.emojis

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
                                List.map (emojiStringToImg sizeStr emojis)
                                    emojisAndStrings

                Html.Parser.Element tag attrs subnodes ->
                    Html.Parser.Element tag attrs <|
                        List.map updater subnodes

                _ ->
                    node
    in
    List.map updater nodes


replaceMentionLinks : RenderEnv -> Maybe Status -> List Html.Parser.Node -> List Html.Parser.Node
replaceMentionLinks renderEnv maybeStatus nodes =
    case maybeStatus of
        Nothing ->
            nodes

        Just status ->
            if status.mentions == [] then
                nodes

            else
                let
                    mentionFolder : Mention -> Dict String Mention -> Dict String Mention
                    mentionFolder mention dict =
                        Dict.insert mention.url mention dict

                    urlToMention =
                        List.foldr mentionFolder Dict.empty status.mentions

                    mapper : Node -> Node
                    mapper node =
                        case node of
                            Element e attributes subNodes ->
                                if e /= "a" then
                                    Element e attributes <| List.map mapper subNodes

                                else
                                    case LE.find (\( href, _ ) -> href == "href") attributes of
                                        Nothing ->
                                            Element e attributes <| List.map mapper subNodes

                                        Just ( href, url ) ->
                                            case Dict.get url urlToMention of
                                                Nothing ->
                                                    node

                                                Just mention ->
                                                    Element e
                                                        (( "title", "Show account dialog for @" ++ mention.acct )
                                                            :: LE.setIf ((==) ( href, url ))
                                                                ( href, "#mentionDialog." ++ mention.id ++ "." ++ mention.url )
                                                                attributes
                                                        )
                                                        subNodes

                            _ ->
                                node
                in
                List.map mapper nodes


statusBody : RenderEnv -> Maybe Status -> String -> Maybe String -> List (Html Msg)
statusBody renderEnv maybeStatus html maybeMarkdown =
    case maybeStatus of
        Nothing ->
            statusBodyInternal renderEnv maybeStatus html maybeMarkdown

        Just status ->
            case Dict.get status.id renderEnv.translationDict of
                Nothing ->
                    statusBodyInternal renderEnv maybeStatus html maybeMarkdown

                Just translation ->
                    statusBodyInternal renderEnv
                        maybeStatus
                        translation.content
                        maybeMarkdown


statusBodyInternal : RenderEnv -> Maybe Status -> String -> Maybe String -> List (Html Msg)
statusBodyInternal renderEnv maybeStatus html maybeMarkdown =
    case Html.Parser.run html of
        Ok nodes ->
            replaceEmojiReferences renderEnv maybeStatus nodes
                |> replaceMentionLinks renderEnv maybeStatus
                |> Util.toVirtualDom

        Err _ ->
            [ case maybeMarkdown of
                Just markdown ->
                    text markdown

                Nothing ->
                    text html
            ]


smallTextFontSize : String
smallTextFontSize =
    "80%"


renderNotificationBody : RenderEnv -> FeedBodyEnv -> String -> String -> Int -> Notification -> Html Msg
renderNotificationBody renderEnv bodyEnv notificationId ellipsisPrefix index notification =
    let
        { color, borderColor } =
            getStyle renderEnv
    in
    case notification.status of
        Nothing ->
            let
                linkInfo =
                    selectedRequestToLinkInfo NotificationsSelected
                        notificationId
            in
            maybeRenderStatusIdLink notificationId linkInfo renderEnv

        Just status ->
            let
                body =
                    statusBody renderEnv
                        (Just status)
                        status.content
                        status.plain_markdown

                timeString =
                    formatIso8601 renderEnv.here status.created_at

                postLink =
                    renderStatusUrl timeString status
            in
            div []
                [ hr borderColor
                , div
                    [ class "content"
                    , style "color" color
                    ]
                  <|
                    List.concat
                        [ [ p [ style "font-size" smallTextFontSize ] [ postLink ] ]
                        , body
                        ]
                , renderPoll renderEnv bodyEnv index status
                , renderMediaAttachments renderEnv status
                , renderStatusActions renderEnv
                    notificationId
                    NotificationsSelected
                    ellipsisPrefix
                    status
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


showMentionDialogMsg : Mention -> Msg
showMentionDialogMsg mention =
    ColumnsUIMsg <| ShowMentionDialog mention


showAccountDialogMsg : Account -> Msg
showAccountDialogMsg account =
    ColumnsUIMsg <| ShowAccountDialog account


renderAccount : RenderEnv -> Account -> Html Msg -> Bool -> Maybe Datetime -> Maybe Status -> Html Msg
renderAccount renderEnv account description useLink datetime maybeStatus =
    let
        { fontSizePct, here } =
            renderEnv

        { color } =
            getStyle renderEnv

        ( url, maybeOnClick, linkTitle ) =
            if useLink then
                ( account.url, Nothing, "Open host page for @" ++ account.acct )

            else
                ( "#"
                , Just <| showAccountDialogMsg account
                , "Show account dialog for @" ++ account.acct
                )
    in
    table []
        [ tr []
            [ td []
                [ imageLink
                    { imageUrl = account.avatar
                    , linkUrl = "#"
                    , altText = "Show avatar for @" ++ account.acct
                    , borderColor =
                        if account.is_pro then
                            Just "gold"

                        else
                            Nothing
                    , h = "3em"
                    , onClick = Just (ColumnsUIMsg <| ShowImage account.avatar)
                    }
                ]
            , td [ style "color" color ]
                [ description
                , br
                , if useLink then
                    a
                        [ href url
                        , title <| "Open in new tab: " ++ url
                        , blankTarget
                        ]
                        [ text ("@" ++ account.acct)
                        , Html.i [ class "icon-link-ext" ] []
                        ]

                  else
                    Html.a
                        [ href "#"
                        , title <| "Show account dialog for @" ++ account.acct
                        , onClick <| showAccountDialogMsg account
                        ]
                        [ text ("@" ++ account.acct) ]
                , if account.is_verified && fontSizePct > 0 then
                    blueCheck fontSizePct

                  else
                    text ""
                , case datetime of
                    Nothing ->
                        text ""

                    Just dt ->
                        span []
                            [ br
                            , let
                                timeString =
                                    formatIso8601 renderEnv.here dt
                              in
                              case maybeStatus of
                                Nothing ->
                                    text timeString

                                Just status ->
                                    renderStatusUrl timeString status
                            ]
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
        { color, borderColor } =
            getStyle renderEnv
    in
    div
        [ style "border" <| "1px solid " ++ borderColor ]
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
        { borderColor } =
            getStyle renderEnv

        border =
            "1px solid " ++ borderColor
    in
    div
        [ style "border" border
        ]
        [ Html.hr
            [ style "color" "red"
            , style "background-color" "red"
            , style "border-color" "red"
            , style "height" "10px"
            , style "margin" "0"
            , style "cursor" "pointer"
            , title "Mark read."
            , onClick (ColumnsUIMsg <| MarkFeedRead feedType)
            ]
            []
        ]


renderStatusWithNewMarker : FeedType -> RenderEnv -> FeedBodyEnv -> Int -> Int -> Status -> Html Msg
renderStatusWithNewMarker feedType renderEnv bodyEnv newElements index status =
    let
        feedId =
            Types.feedID feedType
    in
    if newElements > 0 && newElements == index then
        div []
            [ renderNewMarker feedType renderEnv
            , renderStatus feedType renderEnv bodyEnv feedId index status
            ]

    else
        renderStatus feedType renderEnv bodyEnv feedId index status


renderStatus : FeedType -> RenderEnv -> FeedBodyEnv -> String -> Int -> Status -> Html Msg
renderStatus =
    renderStatusWithId Nothing


renderStatusWithId : Maybe String -> FeedType -> RenderEnv -> FeedBodyEnv -> String -> Int -> Status -> Html Msg
renderStatusWithId maybeNodeid feedType renderEnv bodyEnv ellipsisPrefix index statusIn =
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
                            case Dict.get account_id bodyEnv.references of
                                Nothing ->
                                    Just ( ( account_id, account_id ), "", Nothing )

                                Just reference ->
                                    case reference of
                                        ReferencedAccount acct ->
                                            Just
                                                ( ( acct.acct, account_id )
                                                , acct.url
                                                , Just acct
                                                )

                                        ReferencedMention mention ->
                                            Just
                                                ( ( mention.acct, account_id )
                                                , mention.url
                                                , Nothing
                                                )

        { color, borderColor } =
            getStyle renderEnv

        displayNameHtml =
            renderDisplayName account.display_name renderEnv

        body =
            statusBody renderEnv (Just status) status.content status.plain_markdown

        editedTime =
            case status.edited_at of
                Nothing ->
                    ""

                Just edited_at ->
                    "Edited " ++ formatIso8601 renderEnv.here edited_at
    in
    div
        (List.append
            [ style "border" <| "1px solid " ++ borderColor ]
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
                            [ a
                                [ href ""
                                , onClick <| showAccountDialogMsg acct
                                , title <| "Show account dialog for @" ++ acct.acct
                                ]
                                [ renderDisplayName acct.display_name
                                    renderEnv
                                ]
                            , text " reblogged:"
                            ]
                , case replyToInfo of
                    Nothing ->
                        text ""

                    Just ( ( acct, id ), url, maybeAccount ) ->
                        div
                            [ class "status-el media-body"
                            , class "reply-to-and-account-name"
                            , style "margin-top" "0.2em"
                            ]
                            [ Html.i [ class "icon-reply" ]
                                []
                            , text " Reply to "
                            , a
                                [ href "#"
                                , style "margin-left" "0.4em"
                                , onClick <|
                                    case maybeAccount of
                                        Just a ->
                                            showAccountDialogMsg a

                                        Nothing ->
                                            showMentionDialogMsg <|
                                                { url = url
                                                , username = acct
                                                , acct = acct
                                                , id = id
                                                }
                                , title <| "Show account dialog for @" ++ acct
                                ]
                                [ text <| "@" ++ acct ]
                            ]
                , renderAccount renderEnv
                    account
                    displayNameHtml
                    False
                    (Just status.created_at)
                    (Just status)
                ]
            , case status.group of
                Nothing ->
                    text ""

                Just group ->
                    case bodyEnv.group of
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
            , hr borderColor
            , if editedTime == "" then
                text ""

              else
                p [ style "font-size" "90%" ]
                    [ a
                        [ href "#"
                        , onClick (ColumnsUIMsg <| ShowStatusHistoryDialog status)
                        , title "Show status history dialog"
                        ]
                        [ text editedTime ]
                    , br
                    ]
            , if
                not <|
                    serverHasFeature renderEnv.loginServer
                        featureNames.translation
                        renderEnv
              then
                text ""

              else
                let
                    targetLanguage =
                        defaultedTargetLanguage renderEnv

                    statusLanguage =
                        defaultedStatusLanguage status renderEnv

                    maybeTranslation =
                        Dict.get status.id renderEnv.translationDict
                in
                case maybeTranslation of
                    Nothing ->
                        if targetLanguage == statusLanguage then
                            text ""

                        else
                            p []
                                [ a
                                    [ href "#"
                                    , onClick
                                        (ColumnsUIMsg <|
                                            TranslateStatus status.id targetLanguage
                                        )
                                    ]
                                    [ text "Translate from \""
                                    , text statusLanguage
                                    , text "\" to \""
                                    , text targetLanguage
                                    , text "\""
                                    ]
                                ]

                    Just translation ->
                        p []
                            [ text "Translated from \""
                            , text <|
                                String.toLower translation.detected_source_language
                            , text "\" by "
                            , text translation.provider
                            , br
                            , a
                                [ href "#"
                                , onClick (ColumnsUIMsg <| UntranslateStatus status.id)
                                ]
                                [ text "Remove translation" ]
                            ]
            , div
                [ class "content"
                , style "color" color
                ]
                body
            , renderPoll renderEnv bodyEnv index status
            , renderMediaAttachments renderEnv status
            , renderStatusQuote feedType renderEnv bodyEnv ellipsisPrefix index status
            , renderStatusCard renderEnv status
            , renderStatusActions renderEnv
                statusIn.id
                StatusesSelected
                ellipsisPrefix
                status
            ]
        ]


isPollOptionChecked : String -> Int -> FeedBodyEnv -> Bool
isPollOptionChecked statusId idx { pollSelections } =
    case Dict.get statusId pollSelections of
        Nothing ->
            False

        Just indices ->
            List.member idx indices


pollOptionValue : String -> Int -> FeedBodyEnv -> Int
pollOptionValue statusId idx { pollSelections } =
    case Dict.get statusId pollSelections of
        Just [ value ] ->
            value

        _ ->
            -1


isAPollOptionSelected : String -> FeedBodyEnv -> Bool
isAPollOptionSelected statusId { pollSelections } =
    case Dict.get statusId pollSelections of
        Nothing ->
            False

        Just [] ->
            False

        _ ->
            True


renderPoll : RenderEnv -> FeedBodyEnv -> Int -> Status -> Html Msg
renderPoll renderEnv bodyEnv index status =
    let
        renderStyle =
            getStyle renderEnv

        color =
            renderStyle.color

        expiredColor =
            "red"

        statusId =
            status.id
    in
    case status.poll of
        Nothing ->
            text ""

        Just { id, expires_at, expired, multiple, votes_count, voters_count, options, voted, own_votes } ->
            let
                myPoll =
                    status.account.id == renderEnv.accountId

                voting =
                    Set.member statusId bodyEnv.pollsSubmitted

                renderOption idx option =
                    tr [] <|
                        List.concat
                            [ if myPoll then
                                [ td [] [ text option.title ] ]

                              else
                                [ if not expired && not voted then
                                    if multiple then
                                        td []
                                            [ checkBox
                                                (ColumnsUIMsg <|
                                                    SelectPollOption statusId
                                                        idx
                                                        True
                                                )
                                                (isPollOptionChecked statusId
                                                    idx
                                                    bodyEnv
                                                )
                                                ""
                                            ]

                                    else
                                        td []
                                            [ radioButton
                                                { buttonValue = idx
                                                , radioValue =
                                                    pollOptionValue statusId
                                                        idx
                                                        bodyEnv

                                                -- This isn't unique. It needs a path to this poll, through containing statuses.
                                                , radioName = bodyEnv.feedId ++ "." ++ id ++ "." ++ String.fromInt index
                                                , setter =
                                                    ColumnsUIMsg <|
                                                        SelectPollOption statusId
                                                            idx
                                                            False
                                                , label = ""
                                                }
                                            ]

                                  else
                                    td []
                                        [ if List.member idx own_votes then
                                            if multiple then
                                                text "x"

                                            else
                                                text special.checkmark

                                          else
                                            text <| String.repeat 3 special.nbsp
                                        ]
                                , td [] [ text option.title ]
                                ]
                            , [ td [ style "text-align" "right" ]
                                    [ text <|
                                        String.fromInt <|
                                            if option.votes_count == 0 then
                                                0

                                            else
                                                round
                                                    (100
                                                        * toFloat option.votes_count
                                                        / toFloat votes_count
                                                    )
                                    , text "%"
                                    ]
                              ]
                            ]

                ( voteLabel, voteCount ) =
                    case voters_count of
                        Nothing ->
                            ( " total votes", votes_count )

                        Just vcount ->
                            if vcount == 0 && votes_count /= 0 then
                                ( " total votes", votes_count )

                            else
                                ( " voters", vcount )

                maybeUntil =
                    pollTimeUntil renderEnv.here bodyEnv.now expires_at
            in
            div []
                [ br
                , table [ class "prettytable" ] <|
                    List.indexedMap renderOption options
                , if myPoll || expired then
                    text ""

                  else
                    let
                        enabled =
                            not voted
                                && not voting
                                && isAPollOptionSelected statusId bodyEnv

                        label =
                            if voted then
                                "You voted!"

                            else if voting then
                                "Vote submitted"

                            else
                                "Submit Vote"
                    in
                    p []
                        [ enabledButton enabled
                            (ColumnsUIMsg <| SubmitPollVotes statusId id)
                            label
                        ]
                , p
                    [ style "color" <|
                        if expired then
                            expiredColor

                        else
                            color
                    ]
                    [ a
                        [ href "#"
                        , onClick <| ColumnsUIMsg (RefreshStatus status)
                        ]
                        [ text "refresh" ]
                    , text <| " " ++ special.middleDot ++ " "
                    , text <| String.fromInt voteCount
                    , text voteLabel
                    , br
                    , let
                        expireSpan =
                            case expires_at of
                                Nothing ->
                                    span []

                                Just expireString ->
                                    span [ title expireString ]
                      in
                      expireSpan
                        [ if expired then
                            case maybeUntil of
                                Nothing ->
                                    text "Poll ended"

                                Just ( until, _ ) ->
                                    text <| "Poll ended  " ++ until

                          else
                            case maybeUntil of
                                Nothing ->
                                    text "Unknown expiration time"

                                Just ( until, timeExpired ) ->
                                    if timeExpired then
                                        span []
                                            [ text <| "Poll ended " ++ until
                                            , br
                                            , text "Refresh to see results"
                                            ]

                                    else
                                        text <| "Poll ends " ++ until
                        ]
                    ]
                ]


renderStatusQuote : FeedType -> RenderEnv -> FeedBodyEnv -> String -> Int -> Status -> Html Msg
renderStatusQuote feedType renderEnv bodyEnv ellipsisPrefix index status =
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
                                feedType
                                { renderEnv
                                    | columnWidth =
                                        renderEnv.columnWidth - 16
                                }
                                bodyEnv
                                (ellipsisPrefix ++ ".quote")
                                index
                                wrappedStatus
                            ]
                        ]


renderStatusCard : RenderEnv -> Status -> Html Msg
renderStatusCard renderEnv status =
    case status.card of
        Nothing ->
            text ""

        Just card ->
            let
                { borderColor } =
                    getStyle renderEnv
            in
            div
                [ style "margin" "4px" ]
                [ div
                    [ style "border" <| "1px solid " ++ borderColor
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
            List.indexedMap (renderAttachment renderEnv status)
                status.media_attachments


statusButton : List (Attribute Msg) -> String -> Msg -> Html Msg
statusButton attributes theText msg =
    div [ onClick msg ]
        [ Html.i attributes []
        , text theText
        ]


selectedRequestToLinkInfo : SelectedRequest -> String -> LinkInfo
selectedRequestToLinkInfo selectedRequest id =
    let
        ( label, setId, msg ) =
            case selectedRequest of
                AccountsSelected ->
                    ( "account"
                    , \model -> { model | accountId = id }
                    , SendGetAccount
                    )

                StatusesSelected ->
                    ( "status"
                    , \model -> { model | statusId = id }
                    , SendGetStatus
                    )

                NotificationsSelected ->
                    ( "notification"
                    , \model -> { model | notificationId = id }
                    , SendGetNotification
                    )

                _ ->
                    ( "<error>"
                    , identity
                    , SendNothing
                    )
    in
    { id = id
    , selectedRequest = selectedRequest
    , label = label
    , setId = setId
    , message = msg
    }


renderStatusActions : RenderEnv -> String -> SelectedRequest -> String -> Status -> Html Msg
renderStatusActions renderEnv statusId selectedRequest ellipsisPrefix status =
    let
        { color } =
            getStyle renderEnv

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

        visibility =
            status.visibility

        rebloggedTitle =
            if reblogged then
                "unRepeat"

            else
                case visibility of
                    PublicVisibility ->
                        "Repeat"

                    UnlistedVisibility ->
                        "Repeat"

                    PrivateVisibility ->
                        "Visibile only to followers"

                    DirectVisibility ->
                        "Direct message, visible only to mentions"

                    _ ->
                        "Private group visibility"

        toggleStatusRepeat =
            ColumnsUIMsg <| ToggleStatusRepeat status

        ( retweetedClass, reblogsMsg ) =
            if reblogged then
                ( " button-icon rt-active icon-retweet retweeted"
                , toggleStatusRepeat
                )

            else
                case visibility of
                    PublicVisibility ->
                        ( " button-icon rt-active icon-retweet retweeted-empty"
                        , toggleStatusRepeat
                        )

                    UnlistedVisibility ->
                        ( " button-icon rt-active icon-retweet retweeted-empty"
                        , toggleStatusRepeat
                        )

                    PrivateVisibility ->
                        ( " icon-lock"
                        , Noop
                        )

                    _ ->
                        ( " icon-mail-alt"
                        , Noop
                        )

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

        linkInfo =
            selectedRequestToLinkInfo selectedRequest statusId
    in
    div []
        [ maybeRenderStatusIdLink statusId linkInfo renderEnv
        , div
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
                , class retweetedClass
                ]
                reblogsString
                reblogsMsg
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
            , let
                ellipsisId =
                    ellipsisPrefix ++ "." ++ status.id
              in
              statusButton
                [ class "icon-ellipsis"
                , id ellipsisId
                ]
                ""
                (ColumnsUIMsg <| StatusEllipsisPopup ellipsisId status)
            ]
        ]


maybeRenderStatusIdLink : String -> LinkInfo -> RenderEnv -> Html Msg
maybeRenderStatusIdLink statusId linkInfo renderEnv =
    if not renderEnv.showIds then
        text ""

    else
        div []
            [ b linkInfo.label
            , b " id: "
            , a
                [ href "#"
                , onClick <| ProcessLinkInfo linkInfo
                ]
                [ text statusId ]
            , br
            ]


hrpct : String -> Int -> Html msg
hrpct color pct =
    Html.hr
        [ style "width" <| String.fromInt pct ++ "%"
        , style "margin" "auto"
        , style "background-color" color
        , style "color" color
        , style "border-color" color
        ]
        []


hr : String -> Html msg
hr color =
    hrpct color 95


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


renderAttachment : RenderEnv -> Status -> Int -> Attachment -> Html Msg
renderAttachment renderEnv status index attachment =
    let
        showAttachmentMsg =
            ColumnsUIMsg (ShowAttachmentDialog index status)

        standardDiv imageType =
            let
                popupLink =
                    a
                        [ href "#"
                        , onClick showAttachmentMsg
                        ]
                        [ text "popup" ]

                maxws =
                    renderEnv.columnWidth - 5 |> String.fromInt

                maxes =
                    Just ( maxws, "" )
            in
            div []
                [ span [ style "font-size" smallTextFontSize ]
                    [ text "["
                    , a
                        [ href attachment.url
                        ]
                        [ text imageType ]
                    , text "]"
                    ]
                , case attachment.preview_url of
                    Nothing ->
                        span []
                            [ br
                            , popupLink
                            ]

                    Just preview_url ->
                        span []
                            [ br
                            , a
                                [ href "#"
                                , onClick showAttachmentMsg
                                ]
                                [ case attachmentElement True True False maxes attachment of
                                    Nothing ->
                                        popupLink

                                    Just html ->
                                        html
                                ]
                            ]
                ]
    in
    case attachment.type_ of
        ImageAttachment ->
            let
                preview_url =
                    case attachment.preview_url of
                        Just url ->
                            case attachment.meta of
                                Just (ImageMeta { small }) ->
                                    case small of
                                        Just { width } ->
                                            case width of
                                                Just w ->
                                                    if w < renderEnv.columnWidth then
                                                        attachment.url

                                                    else
                                                        url

                                                Nothing ->
                                                    url

                                        _ ->
                                            url

                                _ ->
                                    url

                        Nothing ->
                            attachment.url
            in
            div []
                [ span [ style "font-size" smallTextFontSize ]
                    [ text "["
                    , a
                        [ href attachment.url ]
                        [ text "image" ]
                    , text "]"
                    ]
                , br
                , a
                    [ href "#"
                    , onClick showAttachmentMsg
                    ]
                    [ img
                        [ src preview_url
                        , alt "image"
                        , style "width" "100%"
                        ]
                        []
                    ]
                ]

        GifvAttachment ->
            standardDiv "gif"

        VideoAttachment ->
            standardDiv "video"

        _ ->
            text ""


px : Int -> String
px int =
    String.fromInt int ++ "px"


pxBang : Int -> String
pxBang int =
    px int ++ " !important"


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
                4 * w - 2

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
                "icon-spin4"
                ShowAllUndisplayed
                "Show All Undisplayed [u]"
            , squareButton ( l, 3 * w - 3 )
                "icon-pencil"
                (ShowPostDialog Nothing)
                "Show Post Dialog [p]"
            , triangleButton LeftButton
                ( 1, 3 * w - 3 )
                (ScrollPage ScrollLeft)
                "Scroll One Page Left"
            , triangleButton RightButton
                ( l + w - 2, 3 * w - 3 )
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
            makeFeedEnv feedId


renderColumns : Model -> Html Msg
renderColumns model =
    let
        renderEnv =
            model.renderEnv

        { color, borderColor } =
            getStyle renderEnv

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
                if model.dialog == PostDialog then
                    text ""

                else
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
                                , style "border" <| "2px solid black"
                                , style "border-radius" "25px"
                                , style "background" scrollPillBackground
                                , style "margin-top" "-1px"
                                , style "cursor" "pointer"
                                , style "color" "black"
                                , Html.Attributes.title "Login to a different server."
                                , onClick (ColumnsUIMsg ShowServerDialog)
                                ]
                                [ text server ]

                --, br
                --, text (model.bodyScroll.scrollLeft |> String.fromFloat)
                ]
        , table
            [ style "border-collapse" "collapse"
            , style "border-color" borderColor
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

                                feedEnv =
                                    getFeedEnv feed.feedType model
                            in
                            td
                                [ style "vertical-align" "top"
                                , style "padding" "0"
                                ]
                                [ Lazy.lazy4 renderFeed
                                    isFeedLoading
                                    renderEnv
                                    feedEnv
                                    feed
                                ]
                        )
                        feeds
                    ]
            ]
        ]


pollTimeUntil : Zone -> Posix -> Maybe Datetime -> Maybe ( String, Bool )
pollTimeUntil here now expires_at =
    case expires_at of
        Nothing ->
            Nothing

        Just dateTime ->
            case Iso8601.toTime dateTime of
                Err _ ->
                    Nothing

                Ok time ->
                    let
                        secondDiff =
                            TE.diff Second here now time
                    in
                    Just
                        ( if TE.diff Hour here now time /= 0 then
                            timeUntil here 2 now time

                          else if TE.diff Minute here now time /= 0 then
                            timeUntil here 1 now time

                          else if secondDiff == 0 then
                            ""

                          else if secondDiff > 0 && secondDiff <= 30 then
                            timeUntil here 1 now time

                          else
                            TimeUntil.addInOrAgo now time <|
                                "less than a minute"
                        , secondDiff == 0 || TimeUntil.diffTimes now time < 0
                        )


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


renderExplorer : Model -> Html Msg
renderExplorer model =
    let
        { backgroundColor, color } =
            getStyle model.renderEnv
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
                [ text <| "Copyright " ++ special.copyright ++ " 2019-2022, Bill St. Clair"
                , br
                , link "@billstclair@impeccable.social"
                    "https://impeccable.social/billstclair"
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
        , style "white-space" "nowrap"
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


isFilterInputValid : FilterInput -> Bool
isFilterInputValid { phrase, context, expires_in } =
    (phrase /= "")
        && (context /= [])
        && ((expires_in == "")
                || (Nothing /= String.toInt expires_in)
           )


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
        , sendButton SendGetStatusCard model
        , text " "
        , sendButton SendGetStatusContext model
        , br
        , text "ancestors & descendants are only for Truth Social. Use context elsewhere."
        , br
        , sendButton SendGetStatusAncestors model
        , text " "
        , sendButton SendGetStatusDescendants model
        , br
        , sendButton SendGetStatusSource model
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
        , if
            serverHasFeature model.renderEnv.loginServer
                featureNames.translation
                model.renderEnv
          then
            text "Server supports translation, when <Status>.language is non-blank."

          else
            text "Server does not support translation."
        , br
        , textInput "target language: "
            2
            (ExplorerUIMsg << SetTargetLanguage)
            (case model.renderEnv.targetLanguage of
                Nothing ->
                    ""

                Just language ->
                    language
            )
        , text " "
        , sendButton SendPostTranslate model
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
        , let
            buttonName =
                sendButtonName model.useElmButtonNames
                    SendPostStatus
          in
          if not model.showPostStatus then
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
                , textInput "content_type: "
                    20
                    (ExplorerUIMsg << SetContentType)
                    model.content_type
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
                , visibilityRadio (Just PrivateGroupVisibility) model
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
                , sendButton SendPostStatus model
                , br
                , if
                    serverHasFeature model.renderEnv.loginServer
                        featureNames.editing
                        model.renderEnv
                  then
                    text ""

                  else
                    span []
                        [ text "Editing is not known to work on this instance."
                        , br
                        , text "You'll probably get an error."
                        , br
                        ]
                , textInput "status id: "
                    25
                    (ExplorerUIMsg << SetStatusId)
                    model.statusId
                , text " "
                , sendButton SendPutStatus model
                , text " "
                , sendButton SendGetStatusHistory model
                , br
                , button (ExplorerUIMsg ToggleShowPostStatus) <| "Hide '" ++ buttonName ++ "'"
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

                PrivateGroupVisibility ->
                    "private_group "


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
        , b "Max Toot Chars: "
        , input
            [ size 5
            , onInput (GlobalMsg << SetMaxTootChars)
            , value <| Maybe.withDefault "300" model.maxTootCharsString
            ]
            []
        , br
        , renderLoginButton model
        , if showSetServerButton then
            span []
                [ text " "
                , button (GlobalMsg SetLoginServer) "Set Server"
                , br
                , input
                    [ size 30
                    , onInput (GlobalMsg << SetTokenText)
                    , placeholder "Bearer ..."
                    , value model.tokenText
                    ]
                    []
                , text " "
                , enabledButton ((model.tokenText /= "") && (model.server /= ""))
                    (GlobalMsg SetTokenFromText)
                    "Set Token"
                ]

          else
            text ""
        ]


renderLoginButton : Model -> Html Msg
renderLoginButton model =
    enabledButton (model.server /= "")
        (GlobalMsg Login)
        "Login"


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
    , accountDialogSelect = "accountDialogSelect"
    , accountDialogStatus = "accountDialogStatus"
    , accountDialog = "accountDialog"
    , notificationTypeSelect = "notificationTypeSelect"
    , statusHistoryDialog = "statusHistoryDialog"
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
            getStyle renderEnv
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
                , class (getStyle renderEnv |> .popupChoiceClass)
                ]
                [ imageLink
                    { imageUrl = account.avatar
                    , linkUrl = "#"
                    , altText = account.acct
                    , borderColor =
                        if account.is_pro then
                            Just "gold"

                        else
                            Nothing
                    , h = "1.5em"
                    , onClick = Just (ColumnsUIMsg <| ShowImage account.avatar)
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
                , class (getStyle renderEnv |> .popupChoiceClass)
                ]
                [ imageFromSpec
                    { imageUrl = group.cover_image_url
                    , linkUrl = ""
                    , altText = group.title
                    , borderColor = Nothing
                    , h = "1.5em"
                    , onClick = Nothing
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
                , class (getStyle renderEnv |> .popupChoiceClass)
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

        CommandChoice command status ->
            case command of
                SeparatorCommand ->
                    Html.hr [ style "margin" "0" ] []

                _ ->
                    span
                        [ class (getStyle renderEnv |> .popupChoiceClass)
                        ]
                        [ text <| commandText command status ]


commandText : Command -> Status -> String
commandText command status =
    case command of
        -- This user
        MuteConversationCommand ->
            if status.muted then
                "Unmute Conversation"

            else
                "Mute Conversation"

        PinOnProfileCommand ->
            if status.pinned then
                "Unpin from Profile"

            else
                "Pin on Profile"

        EditStatusCommand _ ->
            "Edit"

        DeleteStatusCommand ->
            "Delete"

        DeleteAndRedraftCommand ->
            "Delete & Redraft"

        -- other user
        MentionCommand ->
            "Mention @" ++ status.account.username

        MuteCommand relationship ->
            let
                which =
                    case relationship of
                        Just rel ->
                            if rel.muting then
                                "Unmute"

                            else
                                "Mute"

                        Nothing ->
                            "Mute"
            in
            which ++ " @" ++ status.account.username

        BlockCommand relationship ->
            let
                which =
                    case relationship of
                        Just rel ->
                            if rel.blocking then
                                "Unblock"

                            else
                                "Block"

                        Nothing ->
                            "Block"
            in
            which ++ " @" ++ status.account.username

        ReportCommand ->
            "Report @" ++ status.account.username

        CancelCommand ->
            "Cancel"

        _ ->
            "Shouldn't happen"


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

                                CommandsPopup _ ->
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


renderThreadExplorer : ThreadExplorerState -> Model -> Html Msg
renderThreadExplorer state model =
    let
        feedType =
            ThreadExplorerFeed

        renderEnv =
            model.renderEnv

        globalFeedEnv =
            getFeedEnv ThreadExplorerFeed model

        globalBodyEnv =
            globalFeedEnv.bodyEnv

        bodyEnv =
            { globalBodyEnv
                | references = model.references
                , now = model.now
            }

        feedEnv =
            { globalFeedEnv | bodyEnv = bodyEnv }

        { backgroundColor, color, highlightStatusColor, repliedToStatusColor, visitedStatusColor } =
            getStyle renderEnv

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
            case state.headerHeight of
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
                renderAStatus idx s replyChain =
                    let
                        nodeid =
                            threadExplorerStatusId idx

                        ellipsisId =
                            "threadExplorer." ++ String.fromInt idx
                    in
                    if s.id == status.id then
                        div [ style "background-color" highlightStatusColor ]
                            [ renderStatusWithId (Just nodeid)
                                feedType
                                renderEnv2
                                bodyEnv
                                ellipsisId
                                idx
                                s
                            ]

                    else if not replyChain && s.replies_count > 0 then
                        div
                            [ style "background-color" <|
                                if Set.member s.id visited then
                                    visitedStatusColor

                                else
                                    repliedToStatusColor
                            ]
                            [ renderStatusWithId (Just nodeid)
                                feedType
                                renderEnv2
                                bodyEnv
                                ellipsisId
                                idx
                                s
                            ]

                    else
                        renderStatusWithId (Just nodeid)
                            feedType
                            renderEnv2
                            bodyEnv
                            ellipsisId
                            idx
                            s

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
                                    renderAStatus idx s sReplyChain
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
                , fsStyle renderEnv2
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

        DynamoDBDialog ->
            dynamoDBDialog model

        StatusHistoryDialog status history ->
            statusHistoryDialog status history model

        KeyboardShortcutsDialog ->
            keyboardShortcutsDialog model

        SaveRestoreDialog ->
            saveRestoreDialog model

        ReportDialog status ->
            reportDialog status model

        DocsDialog ->
            docsDialog model

        AccountDialog account content ->
            accountDialog account content model

        FeedTypeDialog feedType ->
            feedTypeDialog feedType model

        AreYouSureDialog reason status ->
            areYouSureDialog reason status model

        AlertDialog content ->
            alertDialog content model

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


reportDialog : Status -> Model -> Html Msg
reportDialog status model =
    alertDialog "Report Dialog not yet supported." model


dialogRender : RenderEnv -> Dialog.Config msg -> Dialog.Visible -> Html msg
dialogRender renderEnv config visible =
    let
        { backgroundColor, color } =
            getStyle renderEnv
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


areYouSureDialog : AreYouSureReason -> Status -> Model -> Html Msg
areYouSureDialog reason status model =
    let
        which =
            case reason of
                AreYouSureBlock relationship ->
                    if
                        case relationship of
                            Just { blocking } ->
                                blocking

                            _ ->
                                False
                    then
                        "unblock"

                    else
                        "block"

                AreYouSureMute relationship ->
                    if
                        case relationship of
                            Just { muting } ->
                                muting

                            _ ->
                                False
                    then
                        "unmute"

                    else
                        "mute"

                AreYouSureDeleteStatus ->
                    "delete"
    in
    dialogRender
        model.renderEnv
        { styles =
            [ ( "width", "40%" )
            , ( "font-size", fspct model.renderEnv )
            ]
        , title = "Confirm"
        , content =
            [ text <|
                "Are you sure you want to "
                    ++ which
                    ++ (if reason == AreYouSureDeleteStatus then
                            " delete this status?"

                        else
                            " @"
                                ++ status.account.username
                                ++ "?"
                       )
            , case reason of
                AreYouSureMute _ ->
                    span []
                        [ br
                        , checkBox (ExplorerUIMsg ToggleMuteNotifications)
                            model.muteNotifications
                            "Hide notifications from this user?"
                        ]

                _ ->
                    text ""
            ]
        , actionBar =
            [ Html.button
                [ onClick (ColumnsUIMsg <| YesImSure reason status) ]
                [ b <| SE.toSentenceCase which ]
            , Html.button
                [ onClick (ColumnsUIMsg DismissDialog)
                , id nodeIds.cancelButton
                ]
                [ b "Cancel" ]
            ]
        }
        (model.dialog /= NoDialog)


alertDialog : String -> Model -> Html Msg
alertDialog content model =
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

        servers =
            (case model.renderEnv.loginServer of
                Nothing ->
                    model.tokens

                Just server ->
                    Dict.remove server model.tokens
            )
                |> Dict.keys
    in
    [ div []
        [ p []
            [ primaryServerLine model ]
        , p []
            [ loginSelectedUI False model ]
        , if servers == [] then
            text ""

          else
            let
                addServer server res =
                    [ button (GlobalMsg <| LoginServer server) server
                    , text " "
                    ]
                        ++ res
            in
            p []
                [ b "Click to login:"
                , br
                , span [] <|
                    List.foldr addServer [] servers
                ]
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
    let
        renderEnv =
            model.renderEnv
    in
    case maybeServer of
        Nothing ->
            model

        Just server ->
            let
                features =
                    renderEnv.features
            in
            case Dict.get server features of
                Nothing ->
                    { model
                        | renderEnv =
                            { renderEnv
                                | features =
                                    Dict.insert server
                                        (Dict.fromList [ ( featureName, hasFeature ) ])
                                        features
                            }
                    }

                Just dict ->
                    { model
                        | renderEnv =
                            { renderEnv
                                | features =
                                    Dict.insert server
                                        (Dict.insert featureName hasFeature dict)
                                        features
                            }
                    }


serverHasFeature : Maybe String -> String -> RenderEnv -> Bool
serverHasFeature maybeServer featureName renderEnv =
    case serverKnowsFeature maybeServer featureName renderEnv of
        Nothing ->
            False

        Just has ->
            has


serverKnowsFeature : Maybe String -> String -> RenderEnv -> Maybe Bool
serverKnowsFeature maybeServer featureName renderEnv =
    case maybeServer of
        Nothing ->
            Nothing

        Just server ->
            case Dict.get server renderEnv.features of
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


featureNames =
    { groups = "groups"
    , quote = "quote"
    , proFeed = "proFeed"
    , partialContext = "partialContext"
    , editing = "editing"
    , quote_posting = "quote_posting"
    , translation = "translation"
    }


userNameInput : Model -> Html Msg
userNameInput model =
    let
        renderEnv =
            model.renderEnv
    in
    input
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

        { color } =
            getStyle renderEnv
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
            , let
                feedType =
                    NotificationFeed
                        { accountId = Nothing
                        , exclusions = []
                        }
              in
              case LE.find (feedTypeEqual feedType) feedTypes of
                Just _ ->
                    []

                Nothing ->
                    [ row
                        [ b "Notifications"
                        , br
                        , renderNotificationFeedParams
                            (ColumnsUIMsg << SetNotificationColumnParams)
                            model.notificationColumnParams
                            model
                        ]
                        (ColumnsUIMsg <|
                            AddFeedColumn feedType
                        )
                    ]
            , let
                feedType =
                    PublicFeed { flags = Nothing }
              in
              case LE.find (feedTypeEqual feedType) feedTypes of
                Just _ ->
                    []

                Nothing ->
                    [ row
                        [ b "Public"
                        , br
                        , renderPublicFeedFlags
                            (ColumnsUIMsg << SetPublicColumnFlags)
                            model.publicColumnFlags
                        ]
                        (ColumnsUIMsg <|
                            AddFeedColumn feedType
                        )
                    ]
            , if
                not <|
                    serverHasFeature renderEnv.loginServer
                        featureNames.proFeed
                        renderEnv
              then
                []

              else
                let
                    feedType =
                        ProFeed { flags = Nothing }
                in
                case LE.find (feedTypeEqual feedType) feedTypes of
                    Just _ ->
                        []

                    Nothing ->
                        [ row [ b "Pro" ]
                            (ColumnsUIMsg <|
                                AddFeedColumn feedType
                            )
                        ]
            , [ row
                    [ b "User: "
                    , userNameInput model
                    , br
                    , renderUserFeedFlags
                        (ColumnsUIMsg << SetUserColumnFlags)
                        model.userColumnFlags
                    ]
                    (ColumnsUIMsg <| AddFeedColumn Types.defaultUserFeedType)
              ]
            , [ if not <| serverHasFeature renderEnv.loginServer featureNames.groups renderEnv then
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
            , fontelloChar [] "icon-menu" []
            , text " to move that column."
            ]
    , hrpct color 100
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
        [ row "h" "Show Help Dialog" Nothing
        , row "p" "Show Post dialog" Nothing
        , row "r" "Reload all columns" Nothing
        , row "u" "Show all undisplayed" Nothing
        , row "," "Show Settings dialog" Nothing
        , row "s" "Show DynamoDB dialog" Nothing
        , row "t" "Toggle Dark Mode" Nothing
        , row "v" "Show Server Dialog" Nothing
        , row "c" "Show Edit Columns Dialog" Nothing
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
            getStyle model.renderEnv

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
                                , streaming_api = Nothing
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


fontelloChar : List (Attribute Msg) -> String -> List (Attribute Msg) -> Html Msg
fontelloChar divAttributes iClass iAttributes =
    div
        (class "status-el" :: divAttributes)
        [ Html.i (class iClass :: iAttributes)
            []
        ]


getGroup : String -> Model -> Maybe Group
getGroup group_id model =
    Dict.get group_id model.groupDict


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


accountDialog : Account -> Maybe AccountDialogContent -> Model -> Html Msg
accountDialog account maybeContent model =
    let
        renderEnv =
            model.renderEnv
    in
    div
        [ class "tall-dialog-title-height" ]
        [ dialogRender
            renderEnv
            { styles =
                [ ( "padding", "0px" )
                , ( "width", "35em" )
                , ( "max-width", "95%" )
                , ( "max-height", "95%" )
                , ( "font-size", fspct renderEnv )
                , ( "text-align", "center" )
                , ( "background-image", "url(\"" ++ account.header ++ "\")" )
                , ( "background-size", "100%" )
                , ( "background-repeat", "no-repeat" )
                , ( "overflow-y", "auto" )
                , ( "overflow-x", "hidden" )
                ]
            , title = ""
            , content =
                accountDialogContent account maybeContent model
            , actionBar =
                [ button (ColumnsUIMsg DismissDialog) "Ok"
                ]
            }
            True
        ]


emptyRelationship : Relationship
emptyRelationship =
    { id = ""
    , following = False
    , followed_by = False
    , blocking = False
    , muting = False
    , muting_notifications = False
    , requested = False
    , domain_blocking = False
    , showing_reblogs = False
    , endorsed = False
    , v = JE.null
    }


integerFormat : FormatNumber.Locales.Locale
integerFormat =
    let
        format =
            FormatNumber.Locales.base
    in
    { format | thousandSeparator = "," }


formatInteger : Int -> String
formatInteger int =
    FormatNumber.format integerFormat <| toFloat int


feedTypeEqual : FeedType -> FeedType -> Bool
feedTypeEqual ft1 ft2 =
    case ft1 of
        UserFeed { username, server } ->
            case ft2 of
                UserFeed params ->
                    params.username == username && params.server == server

                _ ->
                    False

        PublicFeed _ ->
            case ft2 of
                PublicFeed _ ->
                    True

                _ ->
                    False

        NotificationFeed _ ->
            case ft2 of
                NotificationFeed _ ->
                    True

                _ ->
                    False

        _ ->
            ft1 == ft2


findFeed : FeedType -> FeedSet -> Maybe Feed
findFeed feedType feedSet =
    LE.find (\feed -> feedTypeEqual feedType feed.feedType) feedSet.feeds


feedIndex : FeedType -> FeedSet -> Maybe Int
feedIndex feedType feedSet =
    LE.findIndex (\feed -> feedTypeEqual feedType feed.feedType) feedSet.feeds


accountDialogContent : Account -> Maybe AccountDialogContent -> Model -> List (Html Msg)
accountDialogContent account maybeContent model =
    let
        renderEnv =
            model.renderEnv

        displayNameHtml =
            renderDisplayName account.display_name renderEnv

        userAtServer =
            account.acct

        { color, backgroundColor } =
            getStyle renderEnv

        myAccount =
            case model.account of
                Nothing ->
                    False

                Just acct ->
                    acct.id == account.id

        ( known, { following, followed_by, requested, blocking, muting } ) =
            case Dict.get account.id model.relationships of
                Just relationship ->
                    ( True, relationship )

                Nothing ->
                    ( False, emptyRelationship )

        feedType =
            makeUserFeed userAtServer model.userColumnFlags

        ( acName, acMsg ) =
            case findFeed feedType model.feedSet of
                Just _ ->
                    ( "remove column", DeleteFeedType feedType )

                Nothing ->
                    ( "add column", AddFeedType feedType )

        optionList =
            let
                muteName =
                    if muting then
                        "unmute"

                    else
                        "mute"

                blockName =
                    if blocking then
                        "unblock"

                    else
                        "block"
            in
            [ ( acName, acMsg )
            , ( "mention", PostWithMention userAtServer )
            , ( muteName, MuteAccount muting account )
            , ( blockName, BlockAccount blocking account )
            , ( "hide dialog", DismissDialog )
            , ( "show header", AccountDialogShowHeader account )
            ]

        commandOption ( name, _ ) =
            option [ value name ]
                [ text name ]

        commandSelect =
            select
                [ onInput (ColumnsUIMsg << AccountDialogCommand optionList)
                , id nodeIds.accountDialogSelect
                ]
            <|
                option
                    [ value "", selected True ]
                    [ text "-- select a command --" ]
                    :: List.map commandOption optionList

        hideDialogRow =
            span []
                [ button (ColumnsUIMsg DismissDialog) "hide dialog"
                , text " "
                , button (ColumnsUIMsg <| AccountDialogShowHeader account)
                    "show header"
                , text " "
                , button (ColumnsUIMsg acMsg)
                    acName
                ]
    in
    [ div
        [ style "padding" "2px 0px 0px"
        , style "background-color" backgroundColor
        , style "opacity" "0.9"
        , id nodeIds.accountDialog
        ]
        [ floatingDismissDialogButton model Nothing
        , renderAccount renderEnv
            account
            displayNameHtml
            True
            Nothing
            Nothing
        , div [ style "padding" "0px 20px" ]
            [ if myAccount then
                span []
                    [ text "This is your account"
                    , br
                    , hideDialogRow
                    ]

              else
                let
                    ( followLabel, followTitle ) =
                        if requested then
                            ( "requested", "Unfollow @" ++ userAtServer )

                        else if following then
                            ( "unfollow", "Unfollow @" ++ userAtServer )

                        else if followed_by then
                            ( "follow back", "Follow @" ++ userAtServer )

                        else
                            ( "follow", "Follow @" ++ userAtServer )

                    followedLabel =
                        if not known then
                            "Fetching relationships..."

                        else if followed_by then
                            "Follows you"

                        else
                            "Does NOT follow you"
                in
                span []
                    [ text followedLabel
                    , br
                    , titledButton followTitle
                        known
                        (ColumnsUIMsg <|
                            ToggleFollowAccount (following || requested) account
                        )
                        followLabel
                    , text " "
                    , commandSelect
                    ]
            , p []
                [ let
                    msg =
                        case maybeContent of
                            Just (StatusesContent _) ->
                                Noop

                            _ ->
                                ColumnsUIMsg <| AccountDialogSetShowStatuses True
                  in
                  a
                    [ href "#"
                    , onClick msg
                    ]
                    [ text <| formatInteger account.statuses_count
                    , text " statuses"
                    ]
                , text <| special.nbsp ++ special.nbsp
                , a
                    [ href "#"
                    , onClick <| ColumnsUIMsg AccountDialogShowFollowers
                    ]
                    [ text <| formatInteger account.followers_count
                    , if account.followers_count == 1 then
                        text " follower"

                      else
                        text " followers"
                    ]
                , text <| special.nbsp ++ special.nbsp
                , a
                    [ href "#"
                    , onClick <| ColumnsUIMsg AccountDialogShowFollowing
                    ]
                    [ text "following "
                    , text <| formatInteger account.following_count
                    ]
                , br
                , span [ title <| formatIso8601 renderEnv.here account.created_at ]
                    [ text "Joined "
                    , text <| iso8601ToMonthYear renderEnv.here account.created_at
                    ]
                , br
                ]
            , p [] <| statusBody renderEnv Nothing account.note Nothing
            , case account.fields of
                [] ->
                    text ""

                fields ->
                    let
                        renderField { name, value, verified_at } =
                            let
                                ( verifiedClass, verifiedColor ) =
                                    case verified_at of
                                        Nothing ->
                                            ( "icon-star-empty"
                                            , "red"
                                            )

                                        Just _ ->
                                            ( "icon-star"
                                              -- Neon Green
                                            , "#0FFF50"
                                            )

                                valueHtmls =
                                    [ div
                                        [ style "max-width" "20em"
                                        , style "overflow-x" "hidden"
                                        ]
                                      <|
                                        case Html.Parser.run value of
                                            Ok nodes ->
                                                Util.toVirtualDom nodes

                                            Err _ ->
                                                [ text value ]
                                    ]
                            in
                            tr []
                                [ td [] [ text name ]
                                , td [] valueHtmls
                                , td []
                                    [ Html.i
                                        [ class verifiedClass
                                        , style "color" verifiedColor
                                        ]
                                        []
                                    ]
                                ]
                    in
                    div []
                        [ table
                            [ class "prettytable"
                            , style "overflow-x" "scroll"
                            , style "margin" "0 auto"
                            ]
                          <|
                            List.map renderField fields
                        , br
                        ]
            , if not renderEnv.showIds then
                text ""

              else
                let
                    linkInfo =
                        selectedRequestToLinkInfo AccountsSelected account.id
                in
                p []
                    [ b "account id: "
                    , a
                        [ href ""
                        , onClick <| ProcessLinkInfo linkInfo
                        ]
                        [ text account.id ]
                    ]
            ]
        , div []
            [ let
                ( showStatuses, label ) =
                    case maybeContent of
                        Nothing ->
                            ( True, "show statuses" )

                        Just (StatusesContent _) ->
                            ( False, "show statuses" )

                        Just (FollowingContent _) ->
                            ( False, "show following" )

                        Just (FollowersContent _) ->
                            ( False, "show followers" )
              in
              titledCheckBox "Toggle status display"
                (ColumnsUIMsg <| AccountDialogSetShowStatuses showStatuses)
                (maybeContent /= Nothing)
                label
            , case maybeContent of
                Nothing ->
                    text ""

                Just (StatusesContent { flags, statuses }) ->
                    let
                        globalFeedEnv =
                            getFeedEnv AccountDialogFeed model

                        globalBodyEnv =
                            globalFeedEnv.bodyEnv
                    in
                    div []
                        [ renderUserFeedFlags
                            (ColumnsUIMsg << AccountDialogSetFlags)
                            flags
                        , div
                            [ style "padding" "4px 0px 0px 0px"
                            , style "text-align" "left"
                            ]
                            [ renderAccountDialogStatuses renderEnv
                                { globalBodyEnv
                                    | references = model.references
                                    , now = model.now
                                }
                                statuses
                            ]
                        ]

                Just (FollowingContent accounts) ->
                    renderAccountDialogAccounts model
                        accounts
                    <|
                        if myAccount then
                            Just True

                        else
                            Nothing

                Just (FollowersContent accounts) ->
                    renderAccountDialogAccounts model accounts Nothing
            ]
        ]
    ]


floatingDismissDialogButton : Model -> Maybe ( Int, Int ) -> Html Msg
floatingDismissDialogButton model maybeOffset =
    case model.boundingBox of
        Nothing ->
            text ""

        Just { top, left, width } ->
            let
                ( offsetX, offsetY ) =
                    case maybeOffset of
                        Nothing ->
                            ( 0, 0 )

                        Just offset ->
                            offset
            in
            div
                [ style "position" "absolute"
                , style "top" <| String.fromInt (top + 10 + offsetY) ++ "px"
                , style "left" <| String.fromInt (left + width - 10 - 30 + offsetX) ++ "px"
                , onClick <| ColumnsUIMsg DismissDialog
                , title "hide dialog"
                ]
                [ Html.i
                    [ class "icon-cancel"
                    , style "font-size" "30px"
                    ]
                    []
                ]


renderAccountDialogAccounts : Model -> List Account -> Maybe Bool -> Html Msg
renderAccountDialogAccounts model accounts maybeFollowing =
    let
        renderEnv =
            model.renderEnv

        { borderColor } =
            getStyle renderEnv

        accountId =
            case model.account of
                Nothing ->
                    ""

                Just acct ->
                    acct.id

        render account =
            let
                displayNameHtml =
                    renderDisplayName account.display_name renderEnv
            in
            div [ style "border" <| "1px solid " ++ borderColor ]
                [ renderAccount renderEnv
                    account
                    displayNameHtml
                    False
                    Nothing
                    Nothing
                , if account.id == accountId then
                    text ""

                  else
                    span [ style "text-align" "right" ]
                        [ let
                            ( label, enabled, isFollowing ) =
                                if maybeFollowing == Just True then
                                    ( "unfollow", True, True )

                                else
                                    case Dict.get account.id model.relationships of
                                        Just { following } ->
                                            if following then
                                                ( "unfollow", True, True )

                                            else
                                                ( "follow", True, False )

                                        _ ->
                                            ( "fetching...", False, False )
                          in
                          div
                            [ style "margin-left" "auto"
                            , style "margin-right" "0"
                            ]
                            [ enabledButton enabled
                                (ColumnsUIMsg <|
                                    ToggleFollowAccount isFollowing account
                                )
                                label
                            ]
                        ]
                ]
    in
    div
        [ style "padding" "0px"
        , style "border" <| "1px solid" ++ borderColor
        , style "text-align" "left"
        ]
        (List.map render accounts)


accountDialogStatusId : Int -> String
accountDialogStatusId idx =
    nodeIds.accountDialogStatus ++ "-" ++ String.fromInt idx


renderAccountDialogStatuses : RenderEnv -> FeedBodyEnv -> List Status -> Html Msg
renderAccountDialogStatuses renderEnv feedBodyEnv statuses =
    let
        feedType =
            AccountDialogFeed

        renderAStatus : Int -> Status -> Html Msg
        renderAStatus idx s =
            let
                nodeid =
                    accountDialogStatusId idx

                renderEnv2 =
                    renderEnv
            in
            renderStatusWithId (Just nodeid)
                feedType
                renderEnv2
                feedBodyEnv
                nodeid
                idx
                s
    in
    List.indexedMap renderAStatus statuses
        |> div []


notificationTypeName : NotificationType -> String
notificationTypeName notificationType =
    case notificationType of
        FollowNotification ->
            "follow"

        MentionNotification ->
            "mention"

        ReblogNotification ->
            "reblog"

        FavouriteNotification ->
            "favorite"

        PollNotification ->
            "poll"

        FollowRequestNotification ->
            "follow request"

        UpdateNotification ->
            "update"

        Admin_SignupNotification ->
            "admin signup"

        Admin_ReportNotification ->
            "admin report"

        Pleroma_EmojiReactionNotification ->
            "emoji reaction"

        UnknownNotification s ->
            s


sortNotificationTypes : List NotificationType -> List NotificationType
sortNotificationTypes types =
    List.map (\nt -> ( nt, notificationTypeName nt )) types
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


renderNotificationFeedParams : (NotificationFeedParams -> Msg) -> NotificationFeedParams -> Model -> Html Msg
renderNotificationFeedParams wrapper params model =
    let
        { exclusions } =
            params

        sortedExclusions =
            sortNotificationTypes exclusions

        namedExclusions =
            List.map (\n -> ( n, notificationTypeName n )) sortedExclusions

        inclusions =
            LE.filterNot (\exclusion -> List.member exclusion exclusions)
                Types.allNotificationExclusions
                |> sortNotificationTypes
                |> List.map (\n -> ( n, notificationTypeName n ))

        isMentions =
            sortedExclusions
                == sortNotificationTypes Types.allButMentionNotificationExclusions

        isAll =
            exclusions == []

        makeMentionsMsg =
            let
                excls =
                    if isMentions then
                        []

                    else
                        Types.allButMentionNotificationExclusions
            in
            wrapper { params | exclusions = excls }

        removeExclusion name =
            case LE.find (\( _, nam ) -> nam == name) namedExclusions of
                Nothing ->
                    Noop

                Just ( not, _ ) ->
                    wrapper { params | exclusions = LE.remove not exclusions }

        exclusionOption ( _, nam ) =
            option [ value nam ]
                [ text nam ]

        addExclusion not =
            let
                excls =
                    not :: exclusions

                excls2 =
                    if
                        sortNotificationTypes excls
                            == sortNotificationTypes Types.allNotificationExclusions
                    then
                        []

                    else
                        excls
            in
            wrapper { params | exclusions = excls2 }

        inclusionItem ( not, nam ) =
            span []
                [ text nam
                , titledButton "Exclude this type"
                    True
                    (addExclusion not)
                    "x"
                , text " "
                ]
    in
    span []
        [ checkBox makeMentionsMsg
            isMentions
            "mentions"
        , text " "
        , if namedExclusions == [] then
            select []
                [ option []
                    [ text "-- all notifications shown --" ]
                ]

          else
            select
                [ onInput removeExclusion
                , id nodeIds.notificationTypeSelect
                ]
            <|
                (option
                    [ value ""
                    , selected True
                    ]
                    [ text "-- include notification type --" ]
                    :: List.map exclusionOption namedExclusions
                )
        , if isAll then
            text ""

          else
            span [] <|
                List.append
                    [ br ]
                    (List.map inclusionItem inclusions)

        -- accountId doesn't work reliably in NotificationFeed.
        -- At least not on impeccable.social or gleasonator.com.
        -- All the support for it works, but I'm removing the
        -- UI.
        --, br
        --, userNameInput model
        ]


renderPublicFeedFlags : (PublicFeedFlags -> Msg) -> PublicFeedFlags -> Html Msg
renderPublicFeedFlags wrapper flags =
    let
        makeMsg : (PublicFeedFlags -> Bool) -> (Bool -> PublicFeedFlags) -> Msg
        makeMsg reader writer =
            wrapper <| writer (not <| reader flags)
    in
    span []
        [ checkBox (makeMsg .local (\bool -> { flags | local = bool }))
            flags.local
            "local"
        , text " "
        , checkBox (makeMsg .only_media (\bool -> { flags | only_media = bool }))
            flags.only_media
            "only media"
        ]


renderUserFeedFlags : (UserFeedFlags -> Msg) -> UserFeedFlags -> Html Msg
renderUserFeedFlags wrapper flags =
    let
        makeMsg : (UserFeedFlags -> Bool) -> (Bool -> UserFeedFlags) -> Msg
        makeMsg reader writer =
            wrapper <| writer (not <| reader flags)
    in
    span []
        [ checkBox (makeMsg .pinned (\bool -> { flags | pinned = bool }))
            flags.pinned
            "pinned"
        , text " "
        , checkBox (makeMsg .replies (\bool -> { flags | replies = bool }))
            flags.replies
            "replies"
        , text " "
        , checkBox (makeMsg .reblogs (\bool -> { flags | reblogs = bool }))
            flags.reblogs
            "reblogs"
        , text " "
        , checkBox (makeMsg .only_media (\bool -> { flags | only_media = bool }))
            flags.only_media
            "only media"
        ]


type alias ScrollInfo =
    { scrollLeft : Int
    , col0Left : Int
    , columnWidth : Int
    , maxColumn : Int
    , windowWidth : Int
    }


columnScrollInfo : Model -> ScrollInfo
columnScrollInfo model =
    let
        scrollLeft =
            model.bodyScroll.scrollLeft
                |> round

        col0Left =
            if model.showLeftColumn then
                leftColumnWidth + 2

            else
                2

        columnWidth =
            model.renderEnv.columnWidth
                + 2
    in
    { scrollLeft = scrollLeft
    , col0Left = col0Left
    , columnWidth = columnWidth
    , maxColumn = List.length model.feedSet.feeds - 1
    , windowWidth = (model.renderEnv.windowSize |> Tuple.first) - 8
    }


type alias ColumnInfo =
    { left : Int
    , width : Int
    , headerHeight : Int
    , windowWidth : Int
    }


defaultColumnHeaderHeight : String
defaultColumnHeaderHeight =
    "1.4em"


getFeedColumnInfo : FeedType -> Model -> ColumnInfo
getFeedColumnInfo feedType model =
    let
        { columnWidth, windowWidth, col0Left, scrollLeft } =
            columnScrollInfo model

        feedId =
            Types.feedID feedType

        headerHeight =
            case Dict.get feedId model.feedEnvs of
                Nothing ->
                    0

                Just feedEnv ->
                    case feedEnv.headerHeight of
                        Nothing ->
                            0

                        Just h ->
                            round h

        res =
            { left = 0
            , width = columnWidth
            , headerHeight = headerHeight
            , windowWidth = windowWidth
            }
    in
    case feedIndex feedType model.feedSet of
        Nothing ->
            res

        Just index ->
            { res
                | left = col0Left + index * columnWidth - scrollLeft
            }


feedTypeDialog : FeedType -> Model -> Html Msg
feedTypeDialog feedType model =
    let
        { left, headerHeight } =
            getFeedColumnInfo feedType model

        topS =
            if headerHeight == 0 then
                "calc(5px + " ++ defaultColumnHeaderHeight ++ ")"

            else
                String.fromInt (headerHeight + 5) ++ "px"

        leftS =
            String.fromInt (max (left + 4) 0) ++ "px"

        { color, backgroundColor } =
            getStyle model.renderEnv

        content =
            case feedType of
                UserFeed params ->
                    let
                        flags =
                            case params.flags of
                                Nothing ->
                                    Types.defaultUserFeedFlags

                                Just fs ->
                                    fs

                        updater newFlags =
                            ColumnsUIMsg
                                (UpdateFeedColumn <|
                                    UserFeed { params | flags = Just newFlags }
                                )
                    in
                    renderUserFeedFlags updater flags

                PublicFeed params ->
                    let
                        flags =
                            case params.flags of
                                Nothing ->
                                    Types.defaultPublicFeedFlags

                                Just fs ->
                                    fs

                        updater newFlags =
                            ColumnsUIMsg
                                (UpdateFeedColumn <|
                                    PublicFeed { params | flags = Just newFlags }
                                )
                    in
                    renderPublicFeedFlags updater flags

                NotificationFeed params ->
                    let
                        updater newParams =
                            ColumnsUIMsg
                                (UpdateFeedColumn <|
                                    NotificationFeed newParams
                                )
                    in
                    renderNotificationFeedParams updater params model

                _ ->
                    text ""
    in
    div
        [ style "position" "fixed"
        , style "left" leftS
        , style "top" topS
        , style "z-index" dialogZIndex
        , style "border" <| "1px solid " ++ color
        , style "background-color" backgroundColor
        , style "padding" "5px"
        ]
        [ content
        , br
        , div
            [ style "float" "right" ]
            [ button (ColumnsUIMsg DismissDialog) "Hide" ]
        ]


postDialog : Model -> Html Msg
postDialog model =
    let
        postState =
            model.postState

        renderEnv =
            model.renderEnv

        hasQuoteFeature =
            serverHasFeature renderEnv.loginServer featureNames.quote renderEnv

        hasGroupsFeature =
            serverHasFeature renderEnv.loginServer featureNames.groups renderEnv
    in
    dialogRender
        renderEnv
        { styles =
            [ ( "width", "50em" )
            , ( "font-size", fspct renderEnv )
            ]
        , title =
            case postState.replyType of
                ReplyToPost ->
                    "Reply"

                QuotePost ->
                    "Quote"

                EditPost _ ->
                    "Edit"

                _ ->
                    "Post"
        , content =
            postDialogContent ( hasQuoteFeature, hasGroupsFeature )
                renderEnv
                model.account
                model.dropZone
                model.max_toot_chars
                model.msg
                postState
        , actionBar =
            [ if postState.posting then
                button (ColumnsUIMsg ClearPostState) "Clear"

              else
                let
                    ( enabled, note ) =
                        if postState.text == "" && postState.media_ids == [] then
                            ( False, "Fillin post text or add media" )

                        else if
                            List.length postState.fileNames
                                /= List.length postState.media_ids
                        then
                            ( False, "Waiting for media to upload" )

                        else
                            postDialogPollComplete postState
                in
                titledButton
                    (if enabled then
                        "Send post to server"

                     else
                        note
                    )
                    enabled
                    (ColumnsUIMsg Post)
                <|
                    case postState.replyType of
                        EditPost _ ->
                            "Put Change"

                        _ ->
                            "Post"
            , if model.showLeftColumn || model.scrollPillState.showScrollPill then
                text ""

              else
                button (ColumnsUIMsg ToggleShowScrollPill) "Show Scroll Pill"
            , button (ColumnsUIMsg DismissDialog) "Hide"
            ]
        }
        True


postDialogPollComplete : PostState -> ( Bool, String )
postDialogPollComplete postState =
    case postState.pollDefinition of
        Nothing ->
            ( True, "" )

        Just pollDef ->
            if List.filter ((==) "") pollDef.options /= [] then
                ( False, "Fillin all the poll answers" )

            else if daysHoursMinutesToSeconds postState.daysHoursMinutes == Nothing then
                ( False, "days/hours/minutes malformed" )

            else
                ( True, "" )


maximumPostAttachments : Int
maximumPostAttachments =
    4


postDialogContent : ( Bool, Bool ) -> RenderEnv -> Maybe Account -> DropZone.Model -> Int -> Maybe String -> PostState -> List (Html Msg)
postDialogContent ( hasQuoteFeature, hasGroupsFeature ) renderEnv maybeAccount dropZone max_toot_chars maybeMsg postState =
    let
        { inputBackground, color, darkGrayColor, borderColor } =
            getStyle renderEnv

        ( pollRows, pollDef ) =
            case postState.pollDefinition of
                Nothing ->
                    ( 0, defaultPollDefinition )

                Just def ->
                    ( List.length def.options + 5, def )
    in
    [ let
        replyType =
            postState.replyType
      in
      case postState.replyType of
        NoReply ->
            text ""

        QuotePost ->
            p []
                [ button (ColumnsUIMsg ClearPostStateReplyTo) "Clear Quote" ]

        EditPost _ ->
            p []
                [ button (ColumnsUIMsg ClearPostStateReplyTo) "Clear Edit" ]

        ReplyToPost ->
            case postState.replyTo of
                Nothing ->
                    text ""

                Just replyTo ->
                    let
                        timeString =
                            formatIso8601 renderEnv.here replyTo.created_at

                        preposition =
                            case replyType of
                                ReplyToPost ->
                                    "to "

                                QuotePost ->
                                    "of "

                                EditPost _ ->
                                    "to "

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
    , let
        len =
            String.length postState.text

        lenstr =
            String.fromInt len

        isMarkdownInput =
            postState.content_type == markdownContentType

        markdownRows =
            if isMarkdownInput then
                7

            else
                0

        inputRows =
            let
                r =
                    19 - pollRows
            in
            if isMarkdownInput then
                r - markdownRows

            else
                r
                    |> max 5
      in
      p []
        [ textarea
            [ id nodeIds.postDialogText
            , rows inputRows
            , style "width" "100%"
            , style "color" color
            , style "background-color" inputBackground
            , onInput (ColumnsUIMsg << SetPostText)
            , value postState.text
            ]
            []
        , if not isMarkdownInput then
            text ""

          else
            let
                screenHeight =
                    renderEnv.windowSize |> Tuple.second
            in
            span []
                [ br
                , div
                    [ style "border" <| "1px solid " ++ color
                    , style "padding" "5px"
                    , style "color" color
                    , style "background-color" inputBackground
                    ]
                    [ if postState.text == "" then
                        Html.i []
                            [ text "(Type Markdown above. Formatted version will appear here)"
                            ]

                      else
                        Markdown.toHtml
                            [ style "overflow" "auto"
                            , style "width" "100%"
                            , style "max-height"
                                (String.fromInt (screenHeight // 2) ++ "px")
                            ]
                            postState.text
                    ]
                , a [ href "https://www.markdownguide.org/" ]
                    [ text "Markdown Guide" ]
                , br
                ]
        , if pollRows == 0 then
            text ""

          else
            renderPollRows pollDef postState
        , if len > max_toot_chars then
            span [ style "color" "red" ]
                [ text lenstr ]

          else
            text lenstr
        , text " / "
        , text (String.fromInt max_toot_chars)
        , p []
            [ b "Visibility: "
            , let
                visibility =
                    postState.visibility
              in
              select [ onInput (ColumnsUIMsg << SetPostVisibility) ]
                [ option
                    [ value "public"
                    , selected <| visibility == PublicVisibility
                    , title "Post to public timelines"
                    ]
                    [ text "Public" ]
                , option
                    [ value "unlisted"
                    , selected <| visibility == UnlistedVisibility
                    , title "Do not post to public timelines"
                    ]
                    [ text "Unlisted" ]
                , option
                    [ value "private"
                    , selected <| visibility == PrivateVisibility
                    , title "Post to followers only"
                    ]
                    [ text "Followers-only" ]
                , option
                    [ value "direct"
                    , selected <| visibility == DirectVisibility
                    , title "Post to mentioned users only"
                    ]
                    [ text "Direct" ]
                ]
            , text " "
            , let
                hasImages =
                    postState.fileNames /= []

                cmd =
                    if hasImages then
                        Noop

                    else
                        ColumnsUIMsg <| PostCommand postCommand.poll

                ( iconColor, theTitle ) =
                    if postState.pollDefinition == Nothing then
                        ( color
                        , if hasImages then
                            "Remove images to add poll"

                          else
                            "Add poll"
                        )

                    else
                        ( darkGrayColor, "Remove poll" )
              in
              span
                [ onClick cmd
                , style "color" iconColor
                , title theTitle
                ]
                [ Html.i [ class "icon-chart-bar" ] [] ]
            , let
                cmd =
                    ColumnsUIMsg <| ToggleMarkdownInput

                ( iconColor, theTitle ) =
                    if isMarkdownInput then
                        ( darkGrayColor, "Input plain text" )

                    else
                        ( color, "Input Markdown" )
              in
              span
                [ onClick cmd
                , style "color" iconColor
                , title theTitle
                ]
                [ Html.i [ class "icon-pencil" ] [] ]
            ]
        , case ( renderEnv.loginServer, maybeAccount ) of
            ( Just server, Just { username } ) ->
                span []
                    [ b "Posting as: "
                    , text <| fullUsernameAtServer True username server renderEnv
                    ]

            _ ->
                span [ style "color" "red" ]
                    [ text "Not logged in. Post will fail." ]
        ]
    , case maybeMsg of
        Nothing ->
            text ""

        Just msg ->
            p [ style "color" "red" ]
                [ text msg ]
    , if pollRows > 0 then
        text ""

      else
        renderPostImages postState dropZone
    ]


renderPostImages : PostState -> DropZone.Model -> Html Msg
renderPostImages postState dropZone =
    span []
        [ p []
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


renderPollRows : PollDefinition -> PostState -> Html Msg
renderPollRows pollDef postState =
    let
        pollOption : Int -> String -> Html Msg
        pollOption idx s =
            let
                ph =
                    "Answer #" ++ String.fromInt idx
            in
            span []
                [ input
                    [ value s
                    , onInput (ColumnsUIMsg << SetPollDefinitionOption idx)
                    , size 50
                    , placeholder ph
                    ]
                    []
                , if List.length pollDef.options < 3 then
                    text ""

                  else
                    span []
                        [ text " "
                        , button (ColumnsUIMsg <| RemovePollDefinitionOption idx)
                            "X"
                        ]
                , br
                ]

        { days, hours, minutes } =
            postState.daysHoursMinutes
    in
    p [] <|
        List.concat
            [ [ br
              , b "Poll:"
              , br
              ]
            , List.indexedMap pollOption pollDef.options
            , [ -- Need to limit number of options
                button (ColumnsUIMsg AddPollDefinitionOption)
                    "Add an answer"
              , text " "
              , checkBox (ColumnsUIMsg TogglePollDefinitionMultiple)
                    pollDef.multiple
                    "Multiple"
              , br
              , b "Expires: "
              , input
                    [ value days
                    , onInput (ColumnsUIMsg << SetDaysHoursMinutes "days")
                    , size 2
                    ]
                    []
              , text " days"
              , text <| special.nbsp ++ special.nbsp
              , input
                    [ value hours
                    , onInput (ColumnsUIMsg << SetDaysHoursMinutes "hours")
                    , size 2
                    ]
                    []
              , text " hours"
              , text <| special.nbsp ++ special.nbsp
              , input
                    [ value minutes
                    , onInput (ColumnsUIMsg << SetDaysHoursMinutes "minutes")
                    , size 2
                    ]
                    []
              , text " minutes"
              ]
            ]


defaultPollDefinition : PollDefinition
defaultPollDefinition =
    { options = [ "", "" ]
    , expires_in = 60 * 60 * 24 --one day
    , multiple = False
    , hide_totals = False
    }


postCommand =
    { none = ""
    , poll = "poll"
    }


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


dialogZIndex : String
dialogZIndex =
    "20"


attachmentDialog : AttachmentView -> Model -> Html Msg
attachmentDialog attachmentView model =
    let
        postState =
            model.postState

        renderEnv =
            model.renderEnv
    in
    case attachmentDialogEmbed renderEnv attachmentView of
        Nothing ->
            text ""

        Just embed ->
            div
                [ style "position" "fixed"
                , style "left" "0"
                , style "right" "0"
                , style "top" "0"
                , style "bottom" "0"
                , style "inset" "0px"
                , style "z-index" dialogZIndex
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "display" "flex"
                , style "font-size" "200%"
                ]
                (embed :: attachmentButtons attachmentView)


attachmentButtons : AttachmentView -> List (Html Msg)
attachmentButtons attachmentView =
    let
        attachmentCnt =
            List.length attachmentView.attachments

        attachmentIndex =
            attachmentView.index

        rightEnabled =
            attachmentIndex < attachmentCnt - 1

        rightIncrement =
            if rightEnabled then
                1

            else
                0

        rightMsg =
            ColumnsUIMsg <| IncrementAttachmentIndex rightIncrement

        leftEnabled =
            attachmentIndex > 0

        leftIncrement =
            if leftEnabled then
                -1

            else
                0

        leftMsg =
            ColumnsUIMsg <| IncrementAttachmentIndex leftIncrement
    in
    List.concat
        [ if attachmentCnt <= 1 then
            []

          else
            [ div
                [ style "position" "fixed"
                , style "top" "20%"
                , style "right" "0"
                , style "height" "60%"
                , style "width" "20%"
                , style "background-color" "rgba(0,0,0,0)"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , onClick rightMsg
                ]
                [ enabledButton rightEnabled Noop ">" ]
            , div
                [ style "position" "fixed"
                , style "top" "20%"
                , style "left" "0"
                , style "height" "60%"
                , style "width" "20%"
                , style "background-color" "rgba(0,0,0,0)"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , onClick leftMsg
                ]
                [ enabledButton leftEnabled Noop "<" ]
            ]
        , [ div
                [ style "position" "fixed"
                , style "top" "20%"
                , style "left" "30%"
                , style "height" "60%"
                , style "width" "40%"
                , style "background-color" "rgba(0,0,0,0)"
                , onClick (ColumnsUIMsg DismissAttachmentView)
                ]
                []
          ]
        ]


attachmentDialogEmbed : RenderEnv -> AttachmentView -> Maybe (Html Msg)
attachmentDialogEmbed renderEnv attachmentView =
    let
        ( w, h ) =
            renderEnv.windowSize

        maxws =
            String.fromInt (w - 20)

        maxhs =
            String.fromInt (h - 20)
    in
    case LE.getAt attachmentView.index attachmentView.attachments of
        Nothing ->
            Nothing

        Just attachment ->
            attachmentElement False False True (Just ( maxws, maxhs )) attachment


attachmentElement : Bool -> Bool -> Bool -> Maybe ( String, String ) -> Attachment -> Maybe (Html Msg)
attachmentElement useId usePreviewUrl isAutoplay maxes attachment =
    let
        url =
            if usePreviewUrl then
                Maybe.withDefault attachment.url attachment.preview_url

            else
                attachment.url

        fitAttributes =
            case maxes of
                Nothing ->
                    []

                Just ( maxws, maxhs ) ->
                    magicFitAttributes maxws maxhs
    in
    let
        eid =
            if useId then
                attachmentElementId attachment

            else
                ""

        videoElement _ =
            video
                (List.concat
                    [ [ src url
                      , controls True
                      , autoplay isAutoplay
                      , id eid
                      ]
                    , fitAttributes
                    ]
                )
                []
    in
    case attachment.type_ of
        ImageAttachment ->
            img
                (src url :: fitAttributes)
                []
                |> Just

        GifvAttachment ->
            Just <| videoElement ()

        VideoAttachment ->
            Just <| videoElement ()

        _ ->
            Nothing


attachmentElementId : Attachment -> String
attachmentElementId attachment =
    "i" ++ attachment.id


{-| Pure black magic.
-}
magicFitAttributes : String -> String -> List (Attribute Msg)
magicFitAttributes maxws maxhs =
    let
        xOrNone x =
            if x == "" then
                "none"

            else
                x
    in
    [ style "object-fit" "contain"
    , style "max-width" <| xOrNone maxws
    , style "max-height" <| xOrNone maxhs
    , style "border" "2px solid black"
    , style "width" "auto"
    , style "height" "auto"
    ]


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

Click "$PutStatus" to change "status id", with "status", "in reply to id", "group id", "quote of id", "sensitive", "spoiler text", "visibility", "scheduled at", "language", "idempotency key", and "media ids". Editing statuses is currently supported only by the Rebased backend.

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

The "Set Token" button uses the text to its left as the token for the "server". It is almost always of the form "Bearer <random text>". This is for servers that do not allow tokens to be minted via login with password. You'll need to pull the token from a request from the standard web page (usually in the "Network" tab of the DevTools page, accessed by "Inspect" from the right-click menu on the web page). After doing "Set Token", you need to select the server from the drop-down and click "Login".

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
    , checkmark = stringFromCode 10003 -- \u2713
    , middleDot = stringFromCode 183 -- \u00B7
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
        , ( "GetStatusCard", SendGetStatusCard )
        , ( "GetStatusContext", SendGetStatusContext )
        , ( "GetStatusAncestors", SendGetStatusAncestors )
        , ( "GetStatusDescendants", SendGetStatusDescendants )
        , ( "GetStatusSource", SendGetStatusSource )
        , ( "GetStatusRebloggedBy", SendGetStatusRebloggedBy )
        , ( "GetStatusFavouritedBy", SendGetStatusFavouritedBy )
        , ( "SendPostTranslate", SendPostTranslate )
        , ( "DeleteStatus", SendDeleteStatus )
        , ( "PostReblogStatus", SendPostReblogStatus )
        , ( "PostUnreblogStatus", SendPostUnreblogStatus )
        , ( "PostPinStatus", SendPostPinStatus )
        , ( "PostUnpinStatus", SendPostUnpinStatus )
        , ( "PostStatus", SendPostStatus )
        , ( "PutStatus", SendPutStatus )
        , ( "GetStatusHistory", SendGetStatusHistory )
        , ( "PostMedia", SendPostMedia )
        , ( "PutMedia", SendPutMedia )
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
    , ( SendGetStatusCard, ( "GetStatusCard", "GET statuses/:id/card" ) )
    , ( SendGetStatusContext, ( "GetStatusContext", "GET statuses/:id/context" ) )
    , ( SendGetStatusAncestors, ( "GetStatusAncestors", "GET statuses/:id/context/ancestors" ) )
    , ( SendGetStatusDescendants, ( "GetStatusDecendants", "GET statuses/:id/context/descendants" ) )
    , ( SendGetStatusSource, ( "GetStatusSource", "GET statuses/:id/source" ) )
    , ( SendGetStatusRebloggedBy, ( "GetStatusRebloggedBy", "GET statuses/:id/reblogged_by" ) )
    , ( SendGetStatusFavouritedBy, ( "GetStatusFavouritedBy", "GET statuses/:id/favourited_by" ) )
    , ( SendPostTranslate, ( "SendPostTranslate", "POST statuses/:id/translate" ) )
    , ( SendDeleteStatus, ( "DeleteStatus", "DELETE statuses/:id" ) )
    , ( SendPostReblogStatus, ( "PostReblogStatus", "POST statuses/:id/reblog" ) )
    , ( SendPostUnreblogStatus, ( "PostUnreblogStatus", "POST statuses/:id/unreblog" ) )
    , ( SendPostPinStatus, ( "PostPinStatus", "POST statuses/:id/pin" ) )
    , ( SendPostUnpinStatus, ( "PostUnpinStatus", "POST statuses/:id/unpin" ) )
    , ( SendPostStatus, ( "PostStatus", "POST statuses" ) )
    , ( SendPutStatus, ( "PutStatus", "PUT statuses/:id" ) )
    , ( SendGetStatusHistory, ( "GetStatusHistory", "GET statuses/:id/history" ) )
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



---
--- User documentation
---


docsDialog : Model -> Html Msg
docsDialog model =
    let
        docSection =
            model.docSection

        renderEnv =
            model.renderEnv
    in
    dialogRender
        renderEnv
        { styles =
            [ ( "width", "50em" )
            , ( "font-size", fspct renderEnv )
            ]
        , title =
            "Help"
        , content =
            docsDialogContent renderEnv docSection
        , actionBar =
            [ if model.showLeftColumn || model.scrollPillState.showScrollPill then
                text ""

              else
                button (ColumnsUIMsg ToggleShowScrollPill) "Show Scroll Pill"
            , button (ColumnsUIMsg DismissDialog) "OK"
            ]
        }
        True


docSections : List ( DocSection, String, String )
docSections =
    [ ( DocIntro, "Intro", "intro" )
    , ( DocLogin, "Login", "login" )
    , ( DocColumns, "Columns", "columns" )
    , ( DocColumnEntry, "Column Entry", "column-entry" )
    , ( DocLeftColumn, "Left Column", "left-column" )
    , ( DocScrollPill, "Scroll Pill", "scroll-pill" )
    , ( DocSettingsDialog, "Settings", "settings" )
    , ( DocDynamoDBDialog, "DynamoDB", "dynamoDB" )
    , ( DocEditColumnsDialog, "Edit Columns", "edit-columns" )
    , ( DocSaveRestoreDialog, "Save/Restore", "save/restore" )
    , ( DocKeyboardShortcutsDialog, "Keyboard Shortcuts", "keyboard-shortcuts" )
    , ( DocPostDialog, "Post", "post" )
    , ( DocApi, "API", "api" )
    ]


nextDocSection : DocSection -> ( DocSection, String, String )
nextDocSection section =
    nextDocSectionInternal section docSections


prevDocSection : DocSection -> ( DocSection, String, String )
prevDocSection section =
    nextDocSectionInternal section <| List.reverse docSections


nextDocSectionInternal : DocSection -> List ( DocSection, String, String ) -> ( DocSection, String, String )
nextDocSectionInternal section sections =
    let
        default =
            Maybe.withDefault ( DocIntro, "Intro", "intro" ) <| List.head sections
    in
    case LE.dropWhile (\( s, _, _ ) -> s /= section) sections of
        [] ->
            default

        tail ->
            Maybe.withDefault default <| List.head (List.drop 1 tail)


docSectionOption : DocSection -> ( DocSection, String, String ) -> Html Msg
docSectionOption currentSection ( section, name, _ ) =
    option
        [ value name
        , selected <| section == currentSection
        ]
        [ text name ]


docSectionChooser : DocSection -> Html Msg
docSectionChooser section =
    select [ onInput (GlobalMsg << SetDocSection) ]
        (List.map (docSectionOption section) docSections)


docSectionSelector : DocSection -> Html Msg
docSectionSelector section =
    let
        ( prevSection, prevName, _ ) =
            prevDocSection section

        ( nextSection, nextName, _ ) =
            nextDocSection section

        nameLink name linkText =
            a
                [ href "#"
                , onClick <| (GlobalMsg << SetDocSection) name
                ]
                [ text linkText ]
    in
    span []
        [ nameLink prevName <| special.nbsp ++ "<" ++ special.nbsp
        , text " "
        , docSectionChooser section
        , text " "
        , nameLink nextName <| special.nbsp ++ ">" ++ special.nbsp
        ]


type DocContent
    = DocMarkdown String
    | DocHtml (List (Html Msg))


contentToHtml : DocContent -> List (Html Msg)
contentToHtml content =
    case content of
        DocMarkdown string ->
            [ Markdown.toHtml [] string ]

        DocHtml list ->
            list


docsDialogContent : RenderEnv -> DocSection -> List (Html Msg)
docsDialogContent renderEnv section =
    let
        ( w, h ) =
            renderEnv.windowSize

        height =
            80 * h // 100 |> String.fromInt
    in
    docSectionSelector section
        :: [ div
                [ style "overflow-y" "auto"
                , style "max-height" height
                ]
                (contentToHtml <|
                    case section of
                        DocIntro ->
                            DocMarkdown """
When you first go to [mammudeck.com](./), it brings up the [Home](?page=home) page, if you're not logged in, or the [Columns](#help.columns) page, if you are. You can use the [API Explorer](#help.api) page to do low-level API calls, whether logged in or not. Everything about your session is stored persistently (in your browser's [localStorage 🔗](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) database), so the next time you return to Mammudeck (from the same browser), everything will be as it was when you last came here, except the column content will be updated. Usually, you'll [login](#help.login) from the Home page, and spend most of your time on the [Columns](#help.columns) page.

Link in this help which go to an external page are marked with a link icon:  🔗. Since I couldn't figure out how to get Elm's markdown to open those in a new tab, if you click on one without a keyboard modifier, it will leave the Mammudeck page, and lose your column content state.
"""

                        DocLogin ->
                            DocMarkdown """
You can login to a server that supports the [Mastodon client protocol 🔗](https://docs.joinmastodon.org/client/intro/), usually a [Mastodon 🔗](https://joinmastodon.org/) or [Pleroma 🔗](https://pleroma.social/) server, but there are others. Login forms are on the [Home](?page=home) page, the [API Explorer](#help.api) page, and the [settings](#help.settings) dialog. Enter the server name (e.g. "mastodon.online"), and click the "Login" button. This will take you to the server for your username and password, then switch back to Mammudeck, on successful login. Mammudeck doesn't yet support creation of accounts, so you'll have to do that on the server before logging in from Mammudeck.

Pleroma servers tell Mammudeck how many characters are allowed in a post. Mastodon servers do not, so if you know that the server you're using allows more than the default of 300 characters, enter that as the "Max Toot Chars" before clicking the "Login" button. Posting will work if you don't update this number, but it will warn about posts that exceed this length, and attempting to make a longer post will get an error from the server.
"""

                        DocColumns ->
                            DocMarkdown """
The [Columns](#help.columns) page shows the feeds you are following. It is the default page when you're logged in. You can switch to it from the "Columns/Home/API Explorer" popup on the Home and API pages, and in the left column and [Settings](#help.settings) dialog on the Columns page. You can add columns from the [Edit Columns](#help.edit-columns) dialog.

The bulk of the Columns page is columns filled with [Column Entries](#help.column-entry).

By default, the Columns page has a [left column](#help.left-column), supporting some of the [Settings](#help.settings) dialog features, and a [scroll pill](#help.scroll-pill), a light-blue square with left and right-pointing triangles, in the lower-right-hand corner of the screen. You can disable them in the [Settings](#help.settings) dialog. The left-column is mostly for newbies, and is a waste of screen space once you learn how to use the [Settings](#help.settings) dialog. The scroll pill is mostly for devices without keyboards, e.g. phones and tablets, but some prefer clicking to typing, so will keep it visible always.

If you hide both the left column and the scroll pill on a device with no keyboard, the only way to get them back, is to reply to a post. The [Post Dialog](#help.post) has a "Show Scroll Pill" button in this case.
"""

                        DocColumnEntry ->
                            DocMarkdown """
Each column is filled with column entries, which are scrollable. If you scroll to near the bottom of the column, and there are more entries to show, they will be automatically loaded from the server. There are two types of column entry, notifications and everything else. The only real difference is that notification entries usually have a header identifying the type of notification. The circle with arrows to the left of the column title causes the column to be refreshed from the server. Below that is a list of how many new entries were loaded on the last refresh. Clicking that number clears it. There is a thick red bard below the last new entry. Each entry starts with a header identifying the poster. It contains image, name, username (beginning with @) and the date/time of the post. Clicking on a user name brings up a new tab, showing the user page on its server. Clicking on the data/time brings up a new tab showing the post on its server.
"""

                        DocLeftColumn ->
                            DocMarkdown """
The left column of the [Columns](#help.columns) page is visible by default. You can hide it in the [Settings](#help.settings) dialog.

Click the "server" button to bring up a [login](#help.login) dialog.

Check the "dark" box to change the background from light to dark.

For "font", click "X" to return to the default, "^" to make the font bigger, or "v" to make the font smaller.

For "width", click "^" to make the width of the columns larger or "v" to make it smaller. The columns size will always be an even divisor of the window width, so that some number of full columns will be visible at once (without the left column).

Click the "help" button to bring up this Help dialog.

Click the "edit" button to bring up the [Edit Columns](#help.edit-columns) dialog.

Click the "settings" button to bring up the [Settings](#help.settings) dialog.

Click the "save" button to bring up the [Save/Restore](#help.save/restore) dialog.

Click the "keyboard" button to bring up the [Keyboard Shortcuts](#help.keyboard-shortcuts) dialog.


Click the "reload" button to refresh all columns from the logged-in server.

The "display all" button currently does nothing. When I re-enable streaming column updates, it will cause undisplayed posts to be displayed.

Click the "post" button to bring up the [Post](#help.post) dialog.
"""

                        DocScrollPill ->
                            DocMarkdown """
The scroll pill is in the lower right-hand corner of the page. It is a square with triangles on the two sides, sitting on top of a server identifier. The scroll pill and the server identified may be hidden in the Settings dialog. If you click on one of the triangles, the column display will be scrolled a page in that direction. If you douhble click, it will scroll (horizontally) to the end. If you click on square, it will be replaced by four square buttons. The top button brings up the Settings dialog. The next button refreshes all the columns. The button below that currently does nothing, but it will cause hidden column entries to be displayed when I add streaming updates. The bottom button brings up the Post dialog. Typing "esc" hides the four buttons, replacing them with a single blank square.

If the server is shown under the pill, clicking on it will bring up the Server dialog.
"""

                        DocSettingsDialog ->
                            DocMarkdown """
The settings dialog can be shown by either the scroll pill, the left column, or by typing a comma (","). It contains a bunch of controls:

* If you are currently logged in to a server, it will show your @nick, the server name, and a "Logout" button.
* Below that is where you can select and login to a server. Select the server from the pop-up or type its domain name, for Mastadon servers that support other than the default 300-character post, enter the number of characters supported (Pleroma servers send that number as part of the login protocol, but Mastodon servers do not). Click the "Login" button, and enter your username and password in the resulting server dialog.
* The "Page" pop-up allows you to choose between "Columns", "Home" and "API Explorer". You'll spend most of your time on the "Columns" page. The ["API Explorer"](#help.api) page allows you to send commands to the server and see the JSON returned; it is mostly useful to developers.
* The "Help Dialog" button shows this dialog.
* The "Edit Columns Dialog" button shows the ["Edit Columns"](#help.edit-columns) dialog.
* The "Save/Restore Dialog" button shows the ["Save/Restore"](#help.save/restore) dialog.
* The "Keyboard Shortcuts Dialog" button shows the ["Keyboard Shortcuts"](#help.keyboard-shortcuts) dialog.
* The "Reload All Columns" button reloads from the server the contents of all columns.
* The "Show All Undisplayed" button currently does nothing.
* The "Post Dialog" button shows the ["Post"](#help.post) dialog.
* The "Appearance" section allows you to change the displayed font size and column width. "X" returns to the default font size. "^" and "v" increase and decrease the font size or column width. The "dark" button shows a dark background when checked.
* The "Show/Hide Left Column", "Show/Hide Scroll Pill", and "Show/Hide Server under Scroll Pill" buttons do what they say.
* The "Reload from Server" button reloads the JavaScript code from the server. It is mostly useful when you save the Mammudeck icon on your phone screen, to ensure you're running the latest version of the code.
* The "Clear saved server features" button clears some internal state. It's mostly for development debugging."
* The "Clear all persistent state!" button removes all saved state, including server tokens and column layout. It brings up a confirmation dialog before erasing.
"""

                        DocDynamoDBDialog ->
                            DocMarkdown """
DynamoDB Dialog
"""

                        DocEditColumnsDialog ->
                            DocMarkdown """
The "Edit Columns" dialog is available from the "Edit Columns" button on the ["Settings"](help.settings) dialog. It has choices at the top of the window and a list of columns at the bottom of the window. The choices are "Home", "Notifications", "Public", "User", "Hashtag", and "List". A column that is already included will not be shown. Neither will a column that isn't supported by the current server. You need to type a username, either "user", for users on the logged-in-server, or "user@server.suf", for users on other servers. As you type, Mammudeck will send queries to the server to find a list of users containing the string you've typed. Likewise with "Hashtag". Type the hashtag you want, without the leading sharp sign ("#"). Sometimes the Hashtag search doesn't work, but if it does, you'll be shown possible matches as you type.

To add a column, click on the "+" button to the right of it in the top of the window. To remove a column, click on the "x" button to the right of it in the bottom of the window. To move a column, click on the three bars button to its left, then click on the position you want to move it to.

Click "OK" or press the "esc" key to dismiss the dialog.
"""

                        DocSaveRestoreDialog ->
                            DocMarkdown """
The "Save/Restore" dialog is available from the "Save/Restore Dialog" button on the ["Settings"](help.settings) dialog. It has three sections, each of which contains the JSON for that persistent setting. You can copy them, or paste and click the "Resample" button to store.
"""

                        DocKeyboardShortcutsDialog ->
                            DocMarkdown """
The "Keyboard Shortcuts" dialog is available from the "Keyboard Shortcuts Dialog" bytton on the ["Settings"](help.settings) dialog or by typing "?". It shows all the one-letter commands. Dismiss it by pressing the "OK" button or the "esc" key.
"""

                        DocPostDialog ->
                            DocMarkdown """
The "Post" dialog is available from the "Post Dialog" button the ["Settings"](help.settings) dialog, or by typing "p".

The contents of the Post dialog are persistent. If it has information about a post being replied to, you can clear that by pressing the "Clear Reply" button.

The "Hide" button hids the dialog, without losing anything you've already typed. The "Post" button posts to the server. The dialog will close when the post has been made successfully. Until then, you can click the "Hide" button to finish the post in the background.

The "Choose File" button allows you to choose an image or video file to attach to your post. You can also drag an image and drop it in the rectangular box to the right of the "Choose File" button.

Below the text entry box is a display of the total characters in the post and the total allowed characters.
"""

                        DocApi ->
                            DocMarkdown """
The "Mastodon API Explorer" window is available from the "Page" popup on the home page or the ["Settings"](help.settings) dialog. It has its own help. Click on a section and its help will appear at the bottom of the page.
"""
                )
           ]

module ReadDroppedFilesAsText exposing (main)

{-| Drag and drop files into Elm app example.

This is the example from the proda-ai/elm-dropzone package.

To build it:

    $ cd .../mammudeck
    $ elm make src/ReadDroppedFilesAsText.elm --output site/ReadDroppedFilesAsText.html

Then either aim your browser at that file, or pick it from `elm
reactor` running in the mamudeck directory.

When you drop files on the outlined drop zone, it will print their details.

-}

import Browser
import Dict exposing (Dict)
import DropZone exposing (DropZoneMessage(..), dropZoneEventHandlers, isHovering)
import Html exposing (Html, button, div, h1, input, li, ol, p, text, ul)
import Html.Attributes exposing (id, style, type_)
import Json.Decode as D
import Json.Encode as E
import List
import String
import Task



-- MODEL


type alias File =
    { name : String, size : Int, lastModified : Int, fileType : String }


type alias Model =
    { message : String
    , dropZone :
        DropZone.Model
    , files : List File
    }


init : Model
init =
    { message = "Waiting..."
    , dropZone =
        DropZone.init
    , files = []
    }



-- UPDATE


type Message
    = DnD (DropZone.DropZoneMessage (List File))


update : Message -> Model -> Model
update message model =
    case message of
        DnD (Drop files) ->
            -- this happens when the user dropped something into the dropzone
            { model
                | dropZone =
                    DropZone.update (Drop files) model.dropZone
                , files = files

                -- and store the dropped files
            }

        DnD a ->
            -- these are opaque DropZone messages, just hand them to DropZone to deal with them
            { model | dropZone = DropZone.update a model.dropZone }



-- VIEW


view : Model -> Html Message
view model =
    let
        fileView f =
            li []
                [ text <| f.name
                , ul []
                    [ li [] [ text <| "Size: " ++ String.fromInt f.size ]
                    , li [] [ text <| "Date: " ++ String.fromInt f.lastModified ]
                    , li [] [ text <| "Type: " ++ f.fileType ]
                    ]
                ]

        fileViewList =
            List.map fileView model.files
    in
    div containerStyles
        [ h1 [] [ text "Drag 'n Drop" ]
        , renderDropZone model.dropZone
        , ol [] fileViewList
        ]


renderDropZone : DropZone.Model -> Html Message
renderDropZone dropZoneModel =
    Html.map DnD
        (div (renderZoneAttributes dropZoneModel) [])


renderZoneAttributes :
    DropZone.Model
    -> List (Html.Attribute (DropZoneMessage (List File)))
renderZoneAttributes dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        dropZoneHover
        -- style the dropzone differently depending on whether the user is hovering

     else
        dropZoneDefault
    )
        ++ -- add the necessary DropZone event wiring
           dropZoneEventHandlers decodeFiles


decodeFileList : D.Decoder File
decodeFileList =
    D.map4 File
        (D.field "name" D.string)
        (D.field "size" D.int)
        (D.field "lastModified" D.int)
        (D.field "type" D.string)


toFileList : File -> List File -> List File
toFileList f lf =
    f :: lf


decodeFiles : D.Decoder (List File)
decodeFiles =
    D.at [ "dataTransfer", "files" ] <| D.oneOrMore toFileList decodeFileList



-- D.at [ "dataTranser", "files" ] D.value


containerStyles : List (Html.Attribute a)
containerStyles =
    [ style "padding" "20px"
    , style "font-family" "-apple-system,system-ui,BlinkMacSystemFont,\"Segoe UI\",Roboto,\"Helvetica Neuer\",Arial,sans-serif"
    ]


dropZoneDefault : List (Html.Attribute a)
dropZoneDefault =
    [ style "height" "120px"
    , style "border-radius" "10px"
    , style "border" "1px dashed steelblue"
    ]


dropZoneHover : List (Html.Attribute a)
dropZoneHover =
    [ style "height" "120px"
    , style "border-radius" "10px"
    , style "border" "1px dashed red"
    , style "background-color" "#efefef"
    ]



-- ----------------------------------


app : Program () Model Message
app =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


main : Program () Model Message
main =
    app

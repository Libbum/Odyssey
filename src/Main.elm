module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport, getViewportOf, setViewport)
import Browser.Events
import Html exposing (Html, a, div)
import Html.Attributes exposing (height, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Icons
import Json.Decode as Decode exposing (Decoder)
import List.Zipper as Zipper exposing (Zipper)
import Manifest exposing (Country(..), Filter(..), Image, Location(..), Trip(..), blurURL, countryId, countryNames, filterImages, imageURL, locale, locationCoordinates, locationNames, manifest, sortImages, stringToCountry, stringToLocation, stringToTrip, thumbURL, tripId, tripNames)
import Partition exposing (KPartition, greedyK)
import Ports exposing (nearBottom)
import Svg
import Svg.Attributes
import Task


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--- Model


type alias Model =
    { partition : KPartition Int
    , images : List Image
    , layout : Maybe (Zipper Image)
    , filter : Filter
    , filterSelected : ( Radio, String )
    , resizedAfterLoad : Bool
    , rows : Rows
    , window : Viewport
    , gallery : Viewport
    , viewportOffset : Float
    , scrollWidth : Float
    , locale : String
    , zoom : Maybe Image
    , showModal : Bool
    , showDescription : Bool
    , showControls : Bool
    , showMenu : Bool
    }


initialModel : Int -> Model
initialModel scrollWidth =
    { partition = []
    , images = manifest
    , layout = Nothing
    , filter = All
    , filterSelected = ( RadioAll, "" )
    , resizedAfterLoad = False
    , rows = { total = 0, visible = 10 }
    , window = emptyViewport --TODO: Drop this to viewport.height if we don't need anything else from this later
    , gallery = emptyViewport
    , viewportOffset = 0
    , scrollWidth = toFloat scrollWidth
    , locale = ""
    , zoom = Nothing
    , showModal = False
    , showDescription = True
    , showControls = False
    , showMenu = False
    }


type alias Viewport =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Rows =
    { total : Int
    , visible : Int
    }


emptyViewport : Viewport
emptyViewport =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    }


init : Int -> ( Model, Cmd Msg )
init scrollWidth =
    ( initialModel scrollWidth
    , Cmd.batch
        [ getWindow Init
        , Ports.drawMap ()
        ]
    )


type Event
    = Resize
    | Filter
    | Init


type Radio
    = RadioAll
    | RadioCountry
    | RadioLocation
    | RadioTrip



--- Update


type Msg
    = RePartition
    | Partition Event (Result Browser.Dom.Error Browser.Dom.Viewport)
    | SetWindow Event (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ToggleRadio Radio
    | LazyLoad
    | PutLocale ( String, String )
    | PopLocale
    | ZoomImage (Maybe Image)
    | SetZoom (Maybe Image) (Result Browser.Dom.Error Browser.Dom.Viewport)
    | NextZoom
    | PreviousZoom
    | ToggleModal
    | ToggleDescription
    | ToggleControls Bool
    | ToggleMenu
    | SetSelection String
    | GoToTop
    | KeyPress Keyboard
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- VIEWPORT
        SetWindow event result ->
            case result of
                Ok vp ->
                    ( { model | window = vp.viewport }, Task.attempt (Partition event) (getViewportOf "gallery") )

                Err _ ->
                    ( model, Cmd.none )

        -- GALLERY
        RePartition ->
            ( model, getWindow Resize )

        Partition event result ->
            case result of
                Ok vp ->
                    let
                        oldViewport =
                            vp.viewport

                        asideWidth =
                            if oldViewport.width >= 1800 then
                                495

                            else if oldViewport.width >= 1200 then
                                370

                            else if oldViewport.width >= 900 then
                                290

                            else
                                0

                        ratios =
                            getRatios <| filterImages model.filter model.images

                        rowsGuess =
                            -- So we have the old veiwport, and we need to figure out if our new
                            -- viewport will require a scrollbar or not. Take a guess at the new div height
                            optimalRowCount ratios (oldViewport.width - asideWidth) model.window.height

                        toggleResize =
                            case event of
                                Init ->
                                    True

                                _ ->
                                    model.resizedAfterLoad

                        newWidth =
                            case event of
                                Filter ->
                                    case ( oldViewport.height > model.window.height, rowsGuess < 4, model.resizedAfterLoad ) of
                                        ( True, True, _ ) ->
                                            oldViewport.width + model.scrollWidth

                                        ( False, False, True ) ->
                                            oldViewport.width - model.scrollWidth

                                        _ ->
                                            oldViewport.width

                                Init ->
                                    oldViewport.width - model.scrollWidth

                                Resize ->
                                    oldViewport.width

                        rowsBest =
                            optimalRowCount ratios (newWidth - asideWidth) model.window.height

                        rows =
                            model.rows

                        layout =
                            buildLayout model.images model.filter
                    in
                    ( { model
                        | partition = greedyK (weights ratios) rowsBest
                        , resizedAfterLoad = toggleResize
                        , gallery = { oldViewport | width = newWidth - asideWidth }
                        , rows = { rows | total = rowsBest }
                        , layout = layout
                      }
                    , case event of
                        Filter ->
                            Task.attempt (\_ -> NoOp) (setViewport 0 0)

                        _ ->
                            Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ToggleRadio selected ->
            let
                ( newModel, runCmd ) =
                    case selected of
                        RadioAll ->
                            let
                                rows =
                                    model.rows

                                filter =
                                    newFilter ( selected, "" ) model.filter
                            in
                            ( { model | rows = { rows | visible = 10 }, filterSelected = ( selected, "" ), filter = filter, showMenu = False }
                            , Cmd.batch
                                [ Task.attempt (Partition Filter) (getViewportOf "gallery")
                                , updateMap selected "" True
                                ]
                            )

                        _ ->
                            ( { model | filterSelected = ( selected, "" ) }, Cmd.none )
            in
            ( newModel, runCmd )

        LazyLoad ->
            let
                rows =
                    model.rows

                newRows =
                    model.rows.visible + 5

                visibleRows =
                    if newRows > model.rows.total then
                        model.rows.total

                    else
                        newRows
            in
            ( { model | rows = { rows | visible = newRows } }, Cmd.none )

        -- VIEW CHANGES
        PutLocale locale ->
            let
                ( newLocale, name ) =
                    locale

                map =
                    case model.filterSelected of
                        ( RadioLocation, _ ) ->
                            Cmd.none

                        _ ->
                            updateMap RadioLocation name False
            in
            ( { model | locale = newLocale }, map )

        PopLocale ->
            let
                map =
                    case model.filterSelected of
                        ( RadioLocation, _ ) ->
                            Cmd.none

                        _ ->
                            updateMap RadioLocation "" False
            in
            ( { model | locale = "" }, map )

        -- IMAGE VIEWER
        ZoomImage image ->
            let
                map =
                    case image of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            Ports.drawMap ()
            in
            ( model, Cmd.batch [ Task.attempt (SetZoom image) getViewport, map ] )

        SetZoom image result ->
            case result of
                Ok vp ->
                    let
                        layout =
                            case ( model.layout, image ) of
                                ( Just zip, Just _ ) ->
                                    Zipper.findFirst (\i -> Just i == image) zip

                                _ ->
                                    model.layout
                    in
                    ( { model
                        | zoom = image
                        , viewportOffset = vp.viewport.y
                        , layout = layout
                      }
                    , Task.attempt (\_ -> NoOp) (setViewport 0 model.viewportOffset)
                    )

                Err _ ->
                    ( { model | zoom = image }, Cmd.none )

        NextZoom ->
            let
                ( layout, image ) =
                    getNextZoom model
            in
            ( { model | zoom = image, layout = layout }, Cmd.none )

        PreviousZoom ->
            let
                ( layout, image ) =
                    getPreviousZoom model
            in
            ( { model | zoom = image, layout = layout }, Cmd.none )

        ToggleModal ->
            ( { model | showModal = not model.showModal }, Cmd.none )

        ToggleDescription ->
            ( { model | showDescription = not model.showDescription }, Cmd.none )

        ToggleControls setting ->
            ( { model | showControls = setting }, Cmd.none )

        ToggleMenu ->
            ( { model | showMenu = not model.showMenu }, Cmd.none )

        SetSelection selection ->
            let
                rows =
                    model.rows

                ( radio, _ ) =
                    model.filterSelected

                filter =
                    newFilter ( radio, selection ) model.filter
            in
            ( { model | rows = { rows | visible = 10 }, filter = filter, filterSelected = ( radio, selection ), showMenu = False }
            , Cmd.batch
                [ Task.attempt (Partition Filter) (getViewportOf "gallery")
                , updateMap radio selection True
                ]
            )

        GoToTop ->
            ( model, Task.attempt (\_ -> NoOp) (setViewport 0 0) )

        KeyPress key ->
            case ( key, model.zoom ) of
                ( Left, Just _ ) ->
                    case model.layout of
                        Just zip ->
                            case Zipper.next zip of
                                Just _ ->
                                    let
                                        ( layout, image ) =
                                            getPreviousZoom model
                                    in
                                    ( { model | zoom = image, layout = layout }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ( Right, Just _ ) ->
                    case model.layout of
                        Just zip ->
                            case Zipper.previous zip of
                                Just _ ->
                                    let
                                        ( layout, image ) =
                                            getNextZoom model
                                    in
                                    ( { model | zoom = image, layout = layout }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ( Escape, Just _ ) ->
                    ( model, Task.attempt (SetZoom Nothing) getViewport )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getWindow : Event -> Cmd Msg
getWindow event =
    Task.attempt (SetWindow event) getViewport



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> RePartition)
        , Browser.Events.onKeyDown (Decode.map KeyPress keyDecoder)
        , nearBottom (\_ -> LazyLoad)
        ]


type Keyboard
    = Left
    | Right
    | Escape
    | Other


keyDecoder : Decoder Keyboard
keyDecoder =
    Decode.map toKeyboard (Decode.field "key" Decode.string)


toKeyboard : String -> Keyboard
toKeyboard key =
    case key of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "Escape" ->
            Escape

        _ ->
            Other



--- View


view : Model -> Html Msg
view model =
    case model.zoom of
        Nothing ->
            let
                ( selected, _ ) =
                    model.filterSelected

                layout =
                    case model.layout of
                        Just images ->
                            images |> Zipper.toList

                        Nothing ->
                            []

                asideView =
                    case model.showMenu of
                        True ->
                            Html.Attributes.class "show-aside"

                        False ->
                            Html.Attributes.class ""
            in
            div [ Html.Attributes.class "content" ]
                [ Html.header [ Html.Attributes.id "title" ]
                    [ Html.button [ Html.Attributes.class "title", onClick GoToTop ] [ Html.text "Iridessence" ]
                    , Html.span [ Html.Attributes.class "burger" ]
                        [ Html.label []
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.name "menu-toggle"
                                , Html.Events.onClick ToggleMenu
                                , Html.Attributes.checked model.showMenu
                                ]
                                []
                            , Icons.menu
                            ]
                        ]
                    ]
                , Html.section [ Html.Attributes.id "aside", asideView ]
                    [ div [ Html.Attributes.id "map" ] []
                    , Html.header []
                        [ Html.h1 [] [ Html.text "Odyssey" ]
                        , Html.i [ Html.Attributes.class "quote" ]
                            [ Html.text "The world is a book and those who do not travel read only one page."
                            , Html.div [ Html.Attributes.class "right" ] [ Html.text "— Aurelius Augustinus Hipponensis" ]
                            ]
                        ]
                    , div [ Html.Attributes.class "locale" ] [ Html.text model.locale ]
                    , Html.nav []
                        [ div [ Html.Attributes.class "middle" ]
                            [ radioView RadioAll selected
                            , radioView RadioCountry selected
                            , radioView RadioLocation selected
                            , radioView RadioTrip selected
                            ]
                        , filterMenu model.filterSelected
                        ]
                    , Html.footer []
                        [ Html.ul [ Html.Attributes.class "icons" ]
                            [ Html.li [] [ Html.a [ Html.Attributes.href "https://www.github.com/Libbum/Odyssey" ] [ Icons.github ] ]
                            , Html.li [] [ Html.button [ onClick ToggleModal ] [ Icons.mail ] ]
                            , Html.li [] [ Html.a [ Html.Attributes.href "https://telegram.me/Libbum" ] [ Icons.telegram ] ]
                            ]
                        ]
                    ]
                , Html.main_
                    [ Html.Attributes.id "gallery" ]
                  <|
                    List.take model.rows.visible <|
                        displayImages layout model.gallery.width model.partition []
                , coverView model.showModal
                , modalView model.showModal
                ]

        Just image ->
            let
                ( nextVisible, previousVisible ) =
                    case model.layout of
                        Just zip ->
                            case ( Zipper.previous zip, Zipper.next zip ) of
                                ( Just _, Just _ ) ->
                                    ( True, True )

                                ( Just _, Nothing ) ->
                                    ( True, False )

                                ( Nothing, Just _ ) ->
                                    ( False, True )

                                ( Nothing, Nothing ) ->
                                    ( False, False )

                        Nothing ->
                            ( False, False )
            in
            zoomImage image model.showControls previousVisible nextVisible model.showDescription


displayImages : List Image -> Float -> KPartition Int -> List (Html Msg) -> List (Html Msg)
displayImages images viewportWidth partition imageRows =
    let
        gallerySingleImage =
            List.length images == 1
    in
    case partition of
        one :: theRest ->
            let
                rowWidth =
                    List.length one

                newImageRows =
                    displayRowOfImages (List.take rowWidth images) viewportWidth gallerySingleImage :: imageRows
            in
            displayImages (List.drop rowWidth images) viewportWidth theRest newImageRows

        one ->
            let
                rowOfImages =
                    List.take (List.length one) images
            in
            displayRowOfImages rowOfImages viewportWidth gallerySingleImage :: imageRows


displayRowOfImages : List Image -> Float -> Bool -> Html Msg
displayRowOfImages images viewportWidth gallerySingleImage =
    let
        revImages =
            List.reverse images

        arSum =
            summedAspectRatios images

        ( widths, h ) =
            case gallerySingleImage of
                False ->
                    ( List.reverse <| getWidths revImages viewportWidth arSum [], floor (viewportWidth / arSum) )

                True ->
                    singleImageSize images
    in
    div [ Html.Attributes.class "flex" ] <| List.map2 (\img w -> displayImage img w h) revImages widths


displayImage : Image -> Float -> Int -> Html Msg
displayImage image w h =
    -- Note the - 8 here on the width is to take into account the two 4px margins in the css
    -- We also send in a float as the width attribute to clean up the right edge
    Html.img
        [ src (thumbURL image)
        , Html.Attributes.attribute "width" (String.fromFloat <| w - 8.0)
        , height h
        , onClick (ZoomImage <| Just image)
        , onMouseEnter (PutLocale <| locale image)
        , onMouseLeave PopLocale
        ]
        []


zoomImage : Image -> Bool -> Bool -> Bool -> Bool -> Html Msg
zoomImage image showControls showPrevious showNext showDescription =
    let
        ( description, descriptionIcon ) =
            case showDescription of
                True ->
                    ( div [ Html.Attributes.class "description" ] [ Html.text image.description ], Html.Attributes.class "" )

                _ ->
                    ( Html.text "", Html.Attributes.class "desc-off" )

        controlVisible =
            case showControls of
                True ->
                    Html.Attributes.class "visible"

                _ ->
                    Html.Attributes.class "hidden"

        previous =
            case showPrevious of
                True ->
                    Html.button [ Html.Attributes.class "previous", controlVisible, onClick PreviousZoom ] [ Icons.chevronLeft ]

                _ ->
                    Html.text ""

        next =
            case showNext of
                True ->
                    Html.button [ Html.Attributes.class "next", controlVisible, onClick NextZoom ] [ Icons.chevronRight ]

                _ ->
                    Html.text ""
    in
    div [ Html.Attributes.class "zoombox" ]
        [ Html.img [ Html.Attributes.class "blur", src (blurURL image) ] []
        , Html.img
            [ src (imageURL image)
            , Html.Attributes.class "zoom"
            ]
            []
        , div
            [ Html.Attributes.class "control", onMouseEnter (ToggleControls True), onMouseLeave (ToggleControls False) ]
            [ previous
            , next
            , Html.button [ Html.Attributes.class "description-button", descriptionIcon, controlVisible, onClick ToggleDescription ] [ Icons.info ]
            , Html.button [ Html.Attributes.class "close", controlVisible, onClick (ZoomImage Nothing), Html.Attributes.autofocus True ] [ Icons.x ]
            , description
            ]
        ]


singleImageSize : List Image -> ( List Float, Int )
singleImageSize images =
    List.map
        (\img ->
            let
                width =
                    if img.aspectRatio < 1 then
                        300

                    else
                        300 * img.aspectRatio

                height =
                    if img.aspectRatio >= 1 then
                        300

                    else
                        floor (300 * img.aspectRatio)
            in
            ( [ width ], height )
        )
        images
        |> List.head
        |> Maybe.withDefault ( [ 300 ], 300 )



-- Partition Helpers


buildLayout : List Image -> Filter -> Maybe (Zipper Image)
buildLayout images filter =
    images
        |> filterImages filter
        |> sortImages
        |> Zipper.fromList


getRatios : List Image -> List Float
getRatios =
    List.map .aspectRatio


summedAspectRatios : List Image -> Float
summedAspectRatios images =
    List.foldl (+) 0 (getRatios images)


weights : List Float -> List Int
weights =
    List.map (\p -> floor (p * 100))


optimalRowCount : List Float -> Float -> Float -> Int
optimalRowCount imageRatios viewportWidth sceneHeight =
    let
        idealHeight =
            sceneHeight / 4.0

        summedWidth =
            imageRatios |> List.map (\r -> r * idealHeight) |> List.foldl (+) 0
    in
    round (summedWidth / viewportWidth)


getWidths : List Image -> Float -> Float -> List Float -> List Float
getWidths images viewportWidth arSum widths =
    case images of
        one :: theRest ->
            let
                w =
                    viewportWidth / arSum * one.aspectRatio
            in
            getWidths theRest viewportWidth arSum (w :: widths)

        one ->
            viewportWidth - List.sum widths :: widths



-- Veiw Helpers


getNextZoom : Model -> ( Maybe (Zipper Image), Maybe Image )
getNextZoom model =
    let
        layout =
            case model.layout of
                Just zip ->
                    Zipper.previous zip

                Nothing ->
                    model.layout

        image =
            case layout of
                Just zip ->
                    Just <| Zipper.current zip

                Nothing ->
                    Nothing
    in
    ( layout, image )


getPreviousZoom : Model -> ( Maybe (Zipper Image), Maybe Image )
getPreviousZoom model =
    let
        layout =
            case model.layout of
                Just zip ->
                    Zipper.next zip

                Nothing ->
                    model.layout

        image =
            case layout of
                Just zip ->
                    Just <| Zipper.current zip

                Nothing ->
                    Nothing
    in
    ( layout, image )


radioView : Radio -> Radio -> Html Msg
radioView filter current =
    let
        isChecked =
            filter == current

        icon =
            if isChecked then
                Icons.checkCircle

            else
                Icons.circle

        label =
            case filter of
                RadioAll ->
                    "All"

                RadioLocation ->
                    "By Location"

                RadioCountry ->
                    "By Country"

                RadioTrip ->
                    "By Trip"
    in
    Html.label []
        [ Html.input
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.name "filtering"
            , Html.Events.onClick (ToggleRadio filter)
            , Html.Attributes.checked isChecked
            ]
            []
        , icon
        , Html.text label
        ]


filterMenu : ( Radio, String ) -> Html Msg
filterMenu ( radio, selected ) =
    let
        ( visible, list, name ) =
            case radio of
                RadioAll ->
                    ( "hidden", [], "" )

                RadioLocation ->
                    ( "visible", locationNames, "Location" )

                RadioCountry ->
                    ( "visible", countryNames, "Country" )

                RadioTrip ->
                    ( "visible", tripNames, "Trip" )
    in
    Html.select [ Html.Events.onInput SetSelection, Html.Attributes.class visible ] <|
        Html.option [ Html.Attributes.hidden True, Html.Attributes.selected (selected == "") ] [ Html.text ("— Select a " ++ name ++ " —") ]
            :: List.map
                (\label ->
                    Html.option [ Html.Attributes.selected (label == selected) ]
                        [ Html.text label ]
                )
                list


newFilter : ( Radio, String ) -> Filter -> Filter
newFilter ( radio, selected ) current =
    case radio of
        RadioAll ->
            All

        RadioCountry ->
            case stringToCountry selected of
                Just country ->
                    ByCountry country

                Nothing ->
                    current

        RadioLocation ->
            case stringToLocation selected of
                Just location ->
                    ByLocation location

                Nothing ->
                    current

        RadioTrip ->
            case stringToTrip selected of
                Just trip ->
                    ByTrip trip

                Nothing ->
                    current


coverView : Bool -> Html Msg
coverView show =
    let
        cover =
            case show of
                True ->
                    [ Html.Attributes.class "modal-cover" ]

                _ ->
                    [ Html.Attributes.class "modal-cover", Html.Attributes.class "none" ]
    in
    div cover []


modalView : Bool -> Html Msg
modalView show =
    let
        modal =
            case show of
                True ->
                    [ Html.Attributes.class "modal" ]

                _ ->
                    [ Html.Attributes.class "modal", Html.Attributes.class "off" ]
    in
    div modal
        [ Html.button [ Html.Attributes.class "close", onClick ToggleModal ] [ Icons.x ]
        , Html.form [ Html.Attributes.id "contactModal", Html.Attributes.method "post", Html.Attributes.action "process.php" ]
            [ Html.input [ Html.Attributes.required True, Html.Attributes.placeholder "Name", Html.Attributes.type_ "text", Html.Attributes.name "name" ] []
            , Html.input [ Html.Attributes.required True, Html.Attributes.placeholder "Email", Html.Attributes.type_ "email", Html.Attributes.name "email" ] []
            , Html.textarea [ Html.Attributes.required True, Html.Attributes.placeholder "Message", Html.Attributes.spellcheck True, Html.Attributes.rows 4, Html.Attributes.name "message" ] []
            , Html.img [ Html.Attributes.class "img-verify", Html.Attributes.src "image.php", Html.Attributes.width 80, Html.Attributes.height 30 ] []
            , Html.input [ Html.Attributes.id "verify", Html.Attributes.required True, Html.Attributes.placeholder "Copy the code", Html.Attributes.type_ "text", Html.Attributes.name "verify", Html.Attributes.title "This confirms you are a human user or strong AI and not a spam-bot." ] []
            , div [ Html.Attributes.class "center" ]
                [ Html.input [ Html.Attributes.type_ "submit", Html.Attributes.value "Send Message" ] []
                , div [ Html.Attributes.id "response" ] []
                ]
            ]
        ]



-- Map Helpers


updateMap : Radio -> String -> Bool -> Cmd msg
updateMap radio selected clearPrevious =
    case radio of
        RadioTrip ->
            case stringToTrip selected of
                Just trip ->
                    Ports.viewTrip (tripId trip)

                _ ->
                    Cmd.none

        RadioLocation ->
            let
                port_ =
                    case clearPrevious of
                        True ->
                            Ports.viewLocation

                        False ->
                            Ports.showLocation
            in
            case stringToLocation selected of
                Just location ->
                    let
                        coordinates =
                            locationCoordinates location
                    in
                    port_ ( String.replace " " "_" selected, [ negate <| Tuple.first coordinates, negate <| Tuple.second coordinates ] )

                Nothing ->
                    port_ ( "", [] )

        RadioCountry ->
            case stringToCountry selected of
                Just country ->
                    Ports.viewCountry (countryId country)

                Nothing ->
                    Cmd.none

        RadioAll ->
            Ports.viewAll ()


drawGlobe : Html Msg
drawGlobe =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 400 400"
        ]
        [ Svg.circle
            [ Svg.Attributes.cx "200"
            , Svg.Attributes.cy "200"
            , Svg.Attributes.r "190"
            ]
            []
        ]

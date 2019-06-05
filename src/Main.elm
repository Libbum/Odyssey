module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (getViewport, getViewportOf, setViewport)
import Browser.Events
import Browser.Navigation as Nav
import Gallery exposing (Filter(..))
import Html exposing (Html, a, div)
import Html.Attributes exposing (height, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Events.Extra.Touch as Touch
import Icons
import Json.Decode as Decode exposing (Decoder)
import List.Zipper as Zipper exposing (Zipper)
import Manifest exposing (Country(..), Image, Location(..), Month(..), Trip(..), manifest)
import Partition exposing (KPartition, greedyK)
import Ports exposing (nearBottom)
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
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
    , currentSwipeStart : Maybe Position
    , key : Nav.Key
    , url : Url
    }


initialModel : Int -> Nav.Key -> Url -> Model
initialModel scrollWidth key url =
    { partition = []
    , images = manifest
    , layout = Nothing
    , filter = All
    , filterSelected = ( RadioAll, "" )
    , resizedAfterLoad = False
    , rows = { total = 0, visible = 10 }
    , window = emptyViewport
    , gallery = emptyViewport
    , viewportOffset = 0
    , scrollWidth = toFloat scrollWidth
    , locale = ""
    , zoom = Nothing
    , showModal = False
    , showDescription = True
    , showControls = False
    , showMenu = False
    , currentSwipeStart = Nothing
    , key = key
    , url = url
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


init : Int -> Url -> Nav.Key -> ( Model, Cmd Msg )
init scrollWidth url key =
    ( initialModel scrollWidth key url, getWindow Init (Just url) )


type Event
    = Resize
    | Filter
    | Init


type Radio
    = RadioAll
    | RadioCountry
    | RadioLocation
    | RadioTrip


type Keyboard
    = Left
    | Right
    | Escape
    | Other


type alias Position =
    { x : Float
    , y : Float
    }


type SwipeDirection
    = Tap
    | SwipeLeft
    | SwipeRight



--- Routing


type Route
    = RouteCountry (Maybe Country)
    | RouteLocation (Maybe Location)
    | RouteTrip (Maybe Trip)
    | RouteLicense
    | RouteAll


routeURL : Url.Url -> Model -> ( Model, List (Cmd Msg) )
routeURL url model =
    case Parser.parse routeParser url of
        Just found ->
            routeModel found model

        Nothing ->
            ( model, [ Nav.replaceUrl model.key "/", Ports.drawMap () ] )


routeParser : Parser (Route -> a) a
routeParser =
    let
        sanitise str =
            str
                |> String.replace "-" "/"
                |> String.replace "_" " "
    in
    Parser.oneOf
        [ mapRoute Parser.top RouteAll
        , mapRoute (Parser.s "license") RouteLicense
        , mapRoute (Parser.s "trip" </> Parser.string) (\trip -> RouteTrip (sanitise trip |> Manifest.stringToTrip))
        , mapRoute Parser.string (\country -> RouteCountry (sanitise country |> Manifest.stringToCountry))
        , mapRoute (Parser.string </> Parser.string) (\_ location -> RouteLocation (sanitise location |> Manifest.stringToLocation))
        ]


clearFocus : Url -> Url
clearFocus url =
    case url.query of
        Just focus ->
            case focus of
                "focus" ->
                    { url | query = Nothing }

                _ ->
                    url

        Nothing ->
            url


routeModel : Route -> Model -> ( Model, List (Cmd Msg) )
routeModel route model =
    let
        url =
            model.url

        ( newModel, clearQuery ) =
            case model.url.query of
                Just _ ->
                    let
                        newUrl =
                            { url | query = Nothing }
                    in
                    ( { model | url = newUrl }, Nav.replaceUrl model.key (Url.toString newUrl) )

                Nothing ->
                    ( model, Cmd.none )
    in
    case route of
        RouteCountry maybeCountry ->
            case maybeCountry of
                Just country ->
                    ( { newModel | filter = ByCountry country, filterSelected = ( RadioCountry, Manifest.countryName country ) }, [ Ports.initMap ( 2, Manifest.countryId country, [] ), clearQuery ] )

                Nothing ->
                    resetRoute model

        RouteLocation maybeLocation ->
            case maybeLocation of
                Just location ->
                    let
                        info =
                            Manifest.locationInformation location
                    in
                    ( { newModel | filter = ByLocation location, filterSelected = ( RadioLocation, info.name ) }, [ Ports.initMap ( 3, info.name |> String.replace " " "_", [ negate <| Tuple.first info.coordinates, negate <| Tuple.second info.coordinates ] ), clearQuery ] )

                Nothing ->
                    resetRoute model

        RouteTrip maybeTrip ->
            case maybeTrip of
                Just trip ->
                    let
                        info =
                            Manifest.tripInformation trip
                    in
                    ( { newModel | filter = ByTrip trip, filterSelected = ( RadioTrip, info.description ) }, [ Ports.initMap ( 4, info.name |> String.replace " " "_", [] ), clearQuery ] )

                Nothing ->
                    resetRoute model

        RouteLicense ->
            ( newModel, [] )

        RouteAll ->
            ( newModel, [ Ports.drawMap (), clearQuery ] )


resetRoute : Model -> ( Model, List (Cmd Msg) )
resetRoute model =
    ( model, [ Nav.replaceUrl model.key "/", Ports.drawMap () ] )


mapRoute : Parser a b -> a -> Parser (b -> c) c
mapRoute parser handler =
    Parser.map handler parser



--- Update


type Msg
    = RePartition
    | Partition Event (Result Browser.Dom.Error Browser.Dom.Viewport)
    | SetWindow Event (Maybe Url) (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ToggleRadio Radio
    | LazyLoad
    | PutLocale ( String, String, String )
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
    | SwipeStart ( Float, Float )
    | SwipeEnd ( Float, Float )
    | TouchPreload Image
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- VIEWPORT
        SetWindow event maybeUrl result ->
            case result of
                Ok vp ->
                    let
                        ( newModel, commands ) =
                            case maybeUrl of
                                Just url ->
                                    routeURL url model

                                Nothing ->
                                    ( model, [ Cmd.none ] )
                    in
                    ( { newModel | window = vp.viewport }
                    , Cmd.batch (Task.attempt (Partition event) (getViewportOf "gallery") :: commands)
                    )

                Err _ ->
                    ( model, Cmd.none )

        -- GALLERY
        RePartition ->
            ( model, getWindow Resize Nothing )

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
                            getRatios <| Gallery.filterImages model.filter model.images

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
                                    let
                                        multiplier =
                                            if rowsGuess < 4 then
                                                0

                                            else
                                                1
                                    in
                                    oldViewport.width - multiplier * model.scrollWidth

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
                            ( { model | rows = { rows | visible = 10 }, filterSelected = ( selected, "" ), filter = filter }
                            , Cmd.batch
                                [ Task.attempt (Partition Filter) (getViewportOf "gallery")
                                , updateMap selected "" True
                                , Nav.pushUrl model.key "/"
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
            in
            ( { model | rows = { rows | visible = newRows } }, Cmd.none )

        -- VIEW CHANGES
        PutLocale locale ->
            let
                ( newLocale, name, path ) =
                    locale

                map =
                    case ( model.filterSelected, model.window.width >= 900 ) of
                        ( ( RadioLocation, _ ), _ ) ->
                            Cmd.none

                        ( _, False ) ->
                            Cmd.none

                        ( _, True ) ->
                            updateMap RadioLocation name False
            in
            ( { model | locale = newLocale }, Cmd.batch [ map, Ports.preloadImages [ path ] ] )

        PopLocale ->
            let
                map =
                    case ( model.filterSelected, model.window.width >= 900 ) of
                        ( ( RadioLocation, _ ), _ ) ->
                            Cmd.none

                        ( _, False ) ->
                            Cmd.none

                        ( _, True ) ->
                            updateMap RadioLocation "" False
            in
            ( { model | locale = "" }, map )

        -- IMAGE VIEWER
        ZoomImage image ->
            let
                mapCmd =
                    case image of
                        Just _ ->
                            [ Cmd.none ]

                        Nothing ->
                            [ Ports.drawMap (), getWindow Resize Nothing ]
            in
            ( model, Cmd.batch (Task.attempt (SetZoom image) getViewport :: mapCmd) )

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

                        urlCmd =
                            case image of
                                Just _ ->
                                    Nav.pushUrl model.key "?focus"

                                Nothing ->
                                    Nav.replaceUrl model.key (Url.toString (clearFocus model.url))

                        nextUrl =
                            case layout of
                                Just current ->
                                    Zipper.previous current |> Maybe.map Zipper.current |> Maybe.map Gallery.imageURL

                                _ ->
                                    Nothing

                        prevUrl =
                            case layout of
                                Just current ->
                                    Zipper.next current |> Maybe.map Zipper.current |> Maybe.map Gallery.imageURL

                                _ ->
                                    Nothing
                    in
                    ( { model
                        | zoom = image
                        , viewportOffset = vp.viewport.y
                        , layout = layout
                      }
                    , Cmd.batch [ Task.attempt (\_ -> NoOp) (setViewport 0 model.viewportOffset), urlCmd, Ports.preloadImages (maybesToList [ nextUrl, prevUrl ]) ]
                    )

                Err _ ->
                    ( { model | zoom = image }, Cmd.none )

        NextZoom ->
            let
                ( layout, image, nextUrl ) =
                    getNextZoom model
            in
            ( { model | zoom = image, layout = layout }, preloadCmd nextUrl )

        PreviousZoom ->
            let
                ( layout, image, prevUrl ) =
                    getPreviousZoom model
            in
            ( { model | zoom = image, layout = layout }, preloadCmd prevUrl )

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

                path =
                    case radio of
                        RadioLocation ->
                            let
                                country =
                                    case Manifest.stringToLocation selection of
                                        Just location ->
                                            "/" ++ (Manifest.locationInformation location |> (\info -> Manifest.countryName info.country)) ++ "/"

                                        Nothing ->
                                            "/"
                            in
                            country ++ selection |> String.replace " " "_"

                        RadioTrip ->
                            "/trip/" ++ (String.replace " " "_" selection |> String.replace "/" "-")

                        _ ->
                            "/" ++ String.replace " " "_" selection
            in
            ( { model | rows = { rows | visible = 10 }, filter = filter, filterSelected = ( radio, selection ) }
            , Cmd.batch
                [ Task.attempt (Partition Filter) (getViewportOf "gallery")
                , updateMap radio selection True
                , Nav.pushUrl model.key path
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
                                        ( layout, image, prevUrl ) =
                                            getPreviousZoom model
                                    in
                                    ( { model | zoom = image, layout = layout }, preloadCmd prevUrl )

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
                                        ( layout, image, nextUrl ) =
                                            getNextZoom model
                                    in
                                    ( { model | zoom = image, layout = layout }, preloadCmd nextUrl )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ( Escape, Just _ ) ->
                    ( model, Cmd.batch [ Task.attempt (SetZoom Nothing) getViewport, Ports.drawMap (), getWindow Resize Nothing ] )

                _ ->
                    ( model, Cmd.none )

        SwipeStart ( x, y ) ->
            ( { model | currentSwipeStart = Just { x = x, y = y } }, Cmd.none )

        SwipeEnd ( x, y ) ->
            case model.currentSwipeStart of
                Just start ->
                    let
                        direction =
                            getSwipeDirection start { x = x, y = y }
                    in
                    case ( direction, model.zoom ) of
                        ( SwipeLeft, Just _ ) ->
                            case model.layout of
                                Just zip ->
                                    case Zipper.next zip of
                                        Just _ ->
                                            let
                                                ( layout, image, nextUrl ) =
                                                    getPreviousZoom model
                                            in
                                            ( { model | zoom = image, layout = layout, currentSwipeStart = Nothing }, preloadCmd nextUrl )

                                        Nothing ->
                                            ( { model | currentSwipeStart = Nothing }, Cmd.none )

                                Nothing ->
                                    ( { model | currentSwipeStart = Nothing }, Cmd.none )

                        ( SwipeRight, Just _ ) ->
                            case model.layout of
                                Just zip ->
                                    case Zipper.previous zip of
                                        Just _ ->
                                            let
                                                ( layout, image, prevUrl ) =
                                                    getNextZoom model
                                            in
                                            ( { model | zoom = image, layout = layout, currentSwipeStart = Nothing }, preloadCmd prevUrl )

                                        Nothing ->
                                            ( { model | currentSwipeStart = Nothing }, Cmd.none )

                                Nothing ->
                                    ( { model | currentSwipeStart = Nothing }, Cmd.none )

                        ( Tap, Just _ ) ->
                            ( { model | currentSwipeStart = Nothing, showControls = not model.showControls }, Cmd.none )

                        _ ->
                            ( { model | currentSwipeStart = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        TouchPreload image ->
            ( model, Ports.preloadImages [ Gallery.imageURL image ] )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal _ ->
                    ( model, Cmd.none )

                Browser.External url ->
                    ( model, Nav.load url )

        ChangedUrl url ->
            case ( url.query, model.url.query, model.zoom ) of
                ( Nothing, Just _, Just _ ) ->
                    -- We have a close zoom event, but zoom is still open. Back button is hit.
                    ( { model | url = url }, Cmd.batch [ getWindow Resize Nothing, Task.attempt (SetZoom Nothing) getViewport, Ports.drawMap () ] )

                ( Just _, _, Nothing ) ->
                    ( { model | url = url }, Nav.replaceUrl model.key (Url.toString (clearFocus url)) )

                _ ->
                    case Parser.parse routeParser url of
                        Just route ->
                            -- In the event that the back, forward buttons are clicked, update the view.
                            case route of
                                RouteCountry maybeCountry ->
                                    case maybeCountry of
                                        Just newCountry ->
                                            case model.filter of
                                                ByCountry country ->
                                                    if country == newCountry then
                                                        ( { model | url = url }, Cmd.none )

                                                    else
                                                        doUpdate (ByCountry newCountry) RadioCountry (Manifest.countryName newCountry) model

                                                _ ->
                                                    doUpdate (ByCountry newCountry) RadioCountry (Manifest.countryName newCountry) model

                                        Nothing ->
                                            ( { model | url = url }, Cmd.none )

                                RouteLocation maybeLocation ->
                                    case maybeLocation of
                                        Just newLocation ->
                                            case model.filter of
                                                ByLocation location ->
                                                    if location == newLocation then
                                                        ( { model | url = url }, Cmd.none )

                                                    else
                                                        doUpdate (ByLocation newLocation) RadioLocation (Manifest.locationInformation newLocation |> .name) model

                                                _ ->
                                                    doUpdate (ByLocation newLocation) RadioLocation (Manifest.locationInformation newLocation |> .name) model

                                        Nothing ->
                                            ( { model | url = url }, Cmd.none )

                                RouteTrip maybeTrip ->
                                    case maybeTrip of
                                        Just newTrip ->
                                            case model.filter of
                                                ByTrip trip ->
                                                    if trip == newTrip then
                                                        ( { model | url = url }, Cmd.none )

                                                    else
                                                        doUpdate (ByTrip newTrip) RadioTrip (Manifest.tripInformation newTrip |> .description) model

                                                _ ->
                                                    doUpdate (ByTrip newTrip) RadioTrip (Manifest.tripInformation newTrip |> .description) model

                                        Nothing ->
                                            ( { model | url = url }, Cmd.none )

                                RouteLicense ->
                                    ( { model | url = url }, Cmd.none )

                                RouteAll ->
                                    case model.filter of
                                        All ->
                                            ( { model | url = url }, Cmd.none )

                                        _ ->
                                            doUpdate All RadioAll "" model

                        Nothing ->
                            ( { model | url = url }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


doUpdate : Filter -> Radio -> String -> Model -> ( Model, Cmd Msg )
doUpdate filter radio selection model =
    let
        rows =
            model.rows
    in
    ( { model | rows = { rows | visible = 10 }, filter = filter, filterSelected = ( radio, selection ) }
    , Cmd.batch
        [ Task.attempt (Partition Filter) (getViewportOf "gallery")
        , updateMap radio selection True
        ]
    )


getWindow : Event -> Maybe Url -> Cmd Msg
getWindow event maybeUrl =
    Task.attempt (SetWindow event maybeUrl) getViewport


preloadCmd : Maybe String -> Cmd Msg
preloadCmd url =
    case url of
        Just value ->
            Ports.preloadImages [ value ]

        Nothing ->
            Cmd.none



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> RePartition)
        , Browser.Events.onKeyDown (Decode.map KeyPress keyDecoder)
        , nearBottom (\_ -> LazyLoad)
        ]


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


view : Model -> Document Msg
view model =
    let
        license =
            model.url.path |> String.replace "/" "" |> (\path -> path == "license")
    in
    case ( model.zoom, license ) of
        ( Nothing, False ) ->
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
                    if model.showMenu then
                        Html.Attributes.class "show-aside"

                    else
                        Html.Attributes.class ""
            in
            { title = "Odyssey"
            , body =
                [ Html.header [ Html.Attributes.id "title" ]
                    [ Html.button [ Html.Attributes.class "title", onClick GoToTop ] [ Html.text "Odyssey" ]
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
                            , Html.li [] [ Html.a [ Html.Attributes.href "https://axiomatic.neophilus.net" ] [ Icons.axiomatic ] ]
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
            }

        ( Just image, False ) ->
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
            { title = "Odyssey"
            , body =
                [ zoomImage image model.showControls previousVisible nextVisible model.showDescription ]
            }

        _ ->
            { title = "Odyssey"
            , body =
                licenseView
            }


displayImages : List Image -> Float -> KPartition Int -> List (Html Msg) -> List (Html Msg)
displayImages images viewportWidth partition imageRows =
    let
        galleryNoPartition =
            not (List.isEmpty images) && List.isEmpty partition
    in
    case ( partition, galleryNoPartition ) of
        ( one :: theRest, _ ) ->
            let
                rowWidth =
                    List.length one

                newImageRows =
                    displayRowOfImages (List.take rowWidth images) viewportWidth :: imageRows
            in
            displayImages (List.drop rowWidth images) viewportWidth theRest newImageRows

        ( one, False ) ->
            let
                rowOfImages =
                    List.take (List.length one) images
            in
            displayRowOfImages rowOfImages viewportWidth :: imageRows

        ( _, True ) ->
            displayRowOfImages images viewportWidth :: imageRows


displayRowOfImages : List Image -> Float -> Html Msg
displayRowOfImages images viewportWidth =
    let
        revImages =
            List.reverse images

        arSum =
            summedAspectRatios images

        ( widths, h ) =
            if List.length images == 1 then
                singleImageSize images

            else
                ( List.reverse <| getWidths revImages viewportWidth arSum [], floor (viewportWidth / arSum) )
    in
    div [ Html.Attributes.class "flex" ] <| List.map2 (\img w -> displayImage img w h) revImages widths


displayImage : Image -> Float -> Int -> Html Msg
displayImage image w h =
    let
        swipeOptions =
            { stopPropagation = False
            , preventDefault = False
            }
    in
    -- Note the - 8 here on the width is to take into account the two 4px margins in the css
    -- We also send in a float as the width attribute to clean up the right edge
    Html.img
        [ src (Gallery.thumbURL image)
        , Html.Attributes.attribute "width" (String.fromFloat <| w - 8.0)
        , height h
        , Touch.onWithOptions "touchstart" swipeOptions (\_ -> TouchPreload image)
        , onClick (ZoomImage <| Just image)
        , onMouseEnter (PutLocale <| Gallery.locale image)
        , onMouseLeave PopLocale
        ]
        []


zoomImage : Image -> Bool -> Bool -> Bool -> Bool -> Html Msg
zoomImage image showControls showPrevious showNext showDescription =
    let
        ( description, descriptionIcon ) =
            if showDescription then
                let
                    ( locale, _, _ ) =
                        Gallery.locale image
                in
                ( div [ Html.Attributes.class "description" ] [ Html.text locale, Html.br [] [], Html.text image.description ], Html.Attributes.class "" )

            else
                ( Html.text "", Html.Attributes.class "desc-off" )

        controlVisible =
            if showControls then
                Html.Attributes.class "visible"

            else
                Html.Attributes.class "hidden"

        previous =
            if showPrevious then
                Html.button [ Html.Attributes.class "previous", controlVisible, onClick PreviousZoom ] [ Icons.chevronLeft ]

            else
                Html.text ""

        next =
            if showNext then
                Html.button [ Html.Attributes.class "next", controlVisible, onClick NextZoom ] [ Icons.chevronRight ]

            else
                Html.text ""

        swipeOptions =
            { stopPropagation = False
            , preventDefault = False -- We still want to zoom, refresh etc
            }
    in
    div [ Html.Attributes.class "zoombox" ]
        [ Html.img [ Html.Attributes.class "blur", src (Gallery.blurURL image) ] []
        , Html.img
            [ src (Gallery.imageURL image)
            , Html.Attributes.class "zoom"
            ]
            []
        , div
            [ Html.Attributes.class "control"
            , onMouseEnter (ToggleControls True)
            , onMouseLeave (ToggleControls False)
            , Touch.onWithOptions "touchstart" swipeOptions (SwipeStart << touchCoordinates)
            , Touch.onWithOptions "touchend" swipeOptions (SwipeEnd << touchCoordinates)
            ]
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
                        floor (300 / img.aspectRatio)
            in
            ( [ width ], height )
        )
        images
        |> List.head
        |> Maybe.withDefault ( [ 300 ], 300 )


licenseView : List (Html Msg)
licenseView =
    let
        odyssey =
            "https://odyssey.neophilus.net"

        neophilus_about =
            "https://axiomatic.neophilus.net/about"

        cc_namespace =
            Html.Attributes.attribute "xmlns:cc" "http://creativecommons.org/ns#"

        by_nc_sa =
            [ Html.Attributes.href "http://creativecommons.org/licenses/by-nc-sa/4.0/", Html.Attributes.rel "license" ]
    in
    [ Html.div [ Html.Attributes.id "license" ]
        [ Html.div [ Html.Attributes.class "license-info" ]
            [ Html.a by_nc_sa
                [ Html.img [ src "https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" ] []
                ]
            , Html.br [] []
            , Html.span
                [ Html.Attributes.attribute "href" "http://purl.org/dc/dcmitype/StillImage"
                , Html.Attributes.attribute "ref" "dct:type"
                , Html.Attributes.attribute "xmlns:dct" "http://purl.org/dc/terms/"
                ]
                [ Html.text "Images" ]
            , Html.text " displayed on this website ("
            , Html.a
                [ Html.Attributes.href odyssey
                , Html.Attributes.rel "cc:attributionURL"
                , cc_namespace
                ]
                [ Html.text odyssey ]
            , Html.text ") are works by "
            , Html.a
                [ Html.Attributes.href neophilus_about
                , Html.Attributes.attribute "property" "cc:attributionName"
                , cc_namespace
                ]
                [ Html.text "Timothy C. DuBois" ]
            , Html.text " and are licensed under a "
            , Html.a by_nc_sa
                [ Html.text "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License" ]
            , Html.text "."
            , Html.br [] []
            , Html.text "Permissions beyond the scope of this license will be considered on a case-by-case basis. Please contact me via "
            , Html.a [ Html.Attributes.href "https://keybase.io/Libbum" ] [ Html.text "keybase" ]
            , Html.text " or the email address on my "
            , Html.a [ Html.Attributes.href neophilus_about ] [ Html.text "about page" ]
            , Html.text " in this instance."
            , Html.hr [] []
            , Html.text "All code for the website is released under the BSD 3-Clause \"New\" or \"Revised\" License, in line with most Elm libraries used within. "
            , Html.br [] []
            , Html.a [ Html.Attributes.href "https://github.com/Libbum/Odyssey" ] [ Html.text "View on Github." ]
            , Html.br [] []
            , Html.a [ Html.Attributes.href "https://app.fossa.io/projects/git%2Bgithub.com%2FLibbum%2FOdyssey?ref=badge_large" ]
                [ Html.img [ src "https://app.fossa.io/api/projects/git%2Bgithub.com%2FLibbum%2FOdyssey.svg?type=large" ] []
                ]
            ]
        ]
    ]



-- Partition Helpers


buildLayout : List Image -> Filter -> Maybe (Zipper Image)
buildLayout images filter =
    images
        |> Gallery.filterImages filter
        |> Gallery.sortImages
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

        _ ->
            viewportWidth - List.sum widths :: widths



-- Veiw Helpers


getNextZoom : Model -> ( Maybe (Zipper Image), Maybe Image, Maybe String )
getNextZoom model =
    let
        layout =
            case model.layout of
                Just zip ->
                    Zipper.previous zip

                Nothing ->
                    model.layout

        nextUrl =
            case layout of
                Just current ->
                    Zipper.previous current |> Maybe.map Zipper.current |> Maybe.map Gallery.imageURL

                _ ->
                    Nothing
    in
    ( layout, Maybe.map Zipper.current layout, nextUrl )


getPreviousZoom : Model -> ( Maybe (Zipper Image), Maybe Image, Maybe String )
getPreviousZoom model =
    let
        layout =
            case model.layout of
                Just zip ->
                    Zipper.next zip

                Nothing ->
                    model.layout

        prevUrl =
            case layout of
                Just current ->
                    Zipper.previous current |> Maybe.map Zipper.current |> Maybe.map Gallery.imageURL

                _ ->
                    Nothing
    in
    ( layout, Maybe.map Zipper.current layout, prevUrl )


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
                    ( "visible", Gallery.locationNames, "Location" )

                RadioCountry ->
                    ( "visible", Gallery.countryNames, "Country" )

                RadioTrip ->
                    ( "visible", Gallery.tripNames, "Trip" )
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
            case Manifest.stringToCountry selected of
                Just country ->
                    ByCountry country

                Nothing ->
                    current

        RadioLocation ->
            case Manifest.stringToLocation selected of
                Just location ->
                    ByLocation location

                Nothing ->
                    current

        RadioTrip ->
            case Manifest.stringToTrip selected of
                Just trip ->
                    ByTrip trip

                Nothing ->
                    current


coverView : Bool -> Html Msg
coverView show =
    let
        cover =
            if show then
                [ Html.Attributes.class "modal-cover" ]

            else
                [ Html.Attributes.class "modal-cover", Html.Attributes.class "none" ]
    in
    div cover []


modalView : Bool -> Html Msg
modalView show =
    let
        modal =
            if show then
                [ Html.Attributes.class "modal" ]

            else
                [ Html.Attributes.class "modal", Html.Attributes.class "off" ]
    in
    div modal
        [ Html.button [ Html.Attributes.class "close", onClick ToggleModal ] [ Icons.x ]
        , Html.form [ Html.Attributes.id "contactModal", Html.Attributes.method "post", Html.Attributes.action "/process.php" ]
            [ Html.input [ Html.Attributes.required True, Html.Attributes.placeholder "Name", Html.Attributes.type_ "text", Html.Attributes.name "name" ] []
            , Html.input [ Html.Attributes.required True, Html.Attributes.placeholder "Email", Html.Attributes.type_ "email", Html.Attributes.name "email" ] []
            , Html.textarea [ Html.Attributes.required True, Html.Attributes.placeholder "Message", Html.Attributes.spellcheck True, Html.Attributes.rows 4, Html.Attributes.name "message" ] []
            , Html.img [ Html.Attributes.class "img-verify", Html.Attributes.src "/image.php", Html.Attributes.width 80, Html.Attributes.height 30 ] []
            , Html.input [ Html.Attributes.id "verify", Html.Attributes.autocomplete False, Html.Attributes.required True, Html.Attributes.placeholder "Copy the code", Html.Attributes.type_ "text", Html.Attributes.name "verify", Html.Attributes.title "This confirms you are a human user or strong AI and not a spam-bot." ] []
            , div [ Html.Attributes.class "center" ]
                [ Html.input [ Html.Attributes.type_ "submit", Html.Attributes.value "Send Message" ] []
                , div [ Html.Attributes.id "response" ] []
                ]
            ]
        ]


maybesToList : List (Maybe a) -> List a
maybesToList =
    List.foldr maybeListHelper []


maybeListHelper : Maybe a -> List a -> List a
maybeListHelper item list =
    case item of
        Nothing ->
            list

        Just v ->
            v :: list



-- Swipe Interactions


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


getSwipeDirection : Position -> Position -> SwipeDirection
getSwipeDirection start end =
    let
        deltaX =
            end.x - start.x

        deltaY =
            end.y - start.y

        sensitivity =
            3
    in
    if abs deltaX > abs deltaY && abs deltaX > sensitivity then
        if deltaX > 0 then
            SwipeLeft

        else
            SwipeRight

    else
        Tap



-- Map Helper


updateMap : Radio -> String -> Bool -> Cmd msg
updateMap radio selected clearPrevious =
    case radio of
        RadioTrip ->
            case Manifest.stringToTrip selected of
                Just trip ->
                    Ports.viewTrip (Gallery.tripId trip)

                _ ->
                    Cmd.none

        RadioLocation ->
            let
                port_ =
                    if clearPrevious then
                        Ports.viewLocation

                    else
                        Ports.showLocation
            in
            case Manifest.stringToLocation selected of
                Just location ->
                    let
                        coordinates =
                            Gallery.locationCoordinates location
                    in
                    port_ ( String.replace " " "_" selected, [ negate <| Tuple.first coordinates, negate <| Tuple.second coordinates ] )

                Nothing ->
                    port_ ( "", [] )

        RadioCountry ->
            case Manifest.stringToCountry selected of
                Just country ->
                    Ports.viewCountry (Manifest.countryId country)

                Nothing ->
                    Cmd.none

        RadioAll ->
            Ports.viewAll ()

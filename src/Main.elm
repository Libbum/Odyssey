module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport, getViewportOf, setViewport)
import Browser.Events
import Html exposing (Html, a, div)
import Html.Attributes exposing (height, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Icons
import Manifest exposing (Country(..), Filter(..), Image, Location(..), SortOrder(..), Trip(..), blurURL, filterImages, imageURL, locale, manifest, sortImages, thumbURL)
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
    , sort : SortOrder
    , filter : Filter
    , resizedAfterLoad : Bool
    , rows : Rows
    , window : Viewport
    , gallery : Viewport
    , viewportOffset : Float
    , scrollWidth : Float
    , locale : String
    , zoom : Maybe Image
    }


initialModel : Int -> Model
initialModel scrollWidth =
    { partition = []
    , images = manifest
    , sort = DateNewest
    , filter = All
    , resizedAfterLoad = False
    , rows = { total = 0, visible = 10 }
    , window = emptyViewport --TODO: Drop this to viewport.height if we don't need anything else from this later
    , gallery = emptyViewport
    , viewportOffset = 0
    , scrollWidth = toFloat scrollWidth
    , locale = ""
    , zoom = Nothing
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
    , getWindow Init
    )


type Event
    = Resize
    | Filter
    | Init



--- Update


type Msg
    = RePartition
    | Partition Event (Result Browser.Dom.Error Browser.Dom.Viewport)
    | SetWindow Event (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ToggleOrder
    | ToggleFilter
    | LazyLoad
    | PutLocale String
    | PopLocale
    | ZoomImage (Maybe Image)
    | SetZoom (Maybe Image) (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ToggleModal
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

                        ratios =
                            getRatios <| filterImages model.filter model.images

                        rowsGuess =
                            -- So we have the old veiwport, and we need to figure out if our new
                            -- viewport will require a scrollbar or not. Take a guess at the new div height
                            -- TODO: The 495 offset here is hardcoded for the aside div, but it will hide
                            -- at some stage, thus will need to be dynamic in the future
                            optimalRowCount ratios (oldViewport.width - 495) model.window.height

                        toggleResize =
                            case event of
                                Init ->
                                    True

                                _ ->
                                    model.resizedAfterLoad

                        newWidth =
                            case event of
                                Filter ->
                                    case ( oldViewport.height > model.window.height, rowsGuess <= 4, model.resizedAfterLoad ) of
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
                            optimalRowCount ratios (newWidth - 495) model.window.height

                        rows =
                            model.rows
                    in
                    ( { model
                        | partition = greedyK (weights ratios) rowsBest
                        , resizedAfterLoad = toggleResize
                        , gallery = { oldViewport | width = newWidth - 495 }
                        , rows = { rows | total = rowsBest }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ToggleOrder ->
            let
                newOrder =
                    case model.sort of
                        DateNewest ->
                            DateOldest

                        DateOldest ->
                            DateNewest
            in
            ( { model | sort = newOrder }, Cmd.none )

        ToggleFilter ->
            let
                newFilter =
                    case model.filter of
                        All ->
                            ByLocation Melbourne

                        _ ->
                            All

                rows =
                    model.rows
            in
            ( { model
                | filter = newFilter
                , rows = { rows | visible = 10 }
              }
            , Task.attempt (Partition Filter) (getViewportOf "gallery")
            )

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
            ( { model | locale = locale }, Cmd.none )

        PopLocale ->
            ( { model | locale = "" }, Cmd.none )

        -- IMAGE VIEWER
        ZoomImage image ->
            ( model, Task.attempt (SetZoom image) getViewport )

        SetZoom image result ->
            case result of
                Ok vp ->
                    ( { model
                        | zoom = image
                        , viewportOffset = vp.viewport.y
                      }
                    , Task.attempt (\_ -> NoOp) (setViewport 0 model.viewportOffset)
                    )

                Err _ ->
                    ( { model | zoom = image }, Cmd.none )

        ToggleModal ->
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
        , nearBottom (\_ -> LazyLoad)
        ]



--- View


view : Model -> Html Msg
view model =
    case model.zoom of
        Nothing ->
            let
                layout =
                    model.images
                        |> filterImages model.filter
                        |> sortImages model.sort

                orderIcon =
                    case model.sort of
                        DateNewest ->
                            Icons.chevronDown

                        DateOldest ->
                            Icons.chevronUp
            in
            div [ Html.Attributes.class "content" ]
                [ Html.section [ Html.Attributes.id "aside" ]
                    [ div [ Html.Attributes.id "map" ] [ drawGlobe ]
                    , Html.header []
                        [ Html.h1 [] [ Html.text "Odyssey" ]
                        , Html.i []
                            [ Html.text "The world is a book and those who do not travel read only one page."
                            , Html.span [ Html.Attributes.class "right" ] [ Html.text "— Aurelius Augustinus Hipponensis" ]
                            ]
                        ]
                    , div []
                        [ Html.button [ onClick ToggleOrder ] [ orderIcon ]
                        , Html.button [ onClick ToggleFilter ] [ Icons.filter ]
                        ]
                    , Html.text model.locale
                    , Html.footer []
                        [ Html.ul [ Html.Attributes.class "icons" ]
                            [ Html.li [] [ Html.a [ Html.Attributes.href "https://www.github.com/Libbum/Odyssey" ] [ Icons.github ] ]
                            , Html.li [] [ Html.button [ onClick ToggleModal ] [ Icons.mail ] ]
                            , Html.li [] [ Html.a [ Html.Attributes.href "https://telegram.me/Libbum" ] [ Icons.telegram ] ]
                            ]
                        ]
                    ]
                , Html.section
                    [ Html.Attributes.id "gallery" ]
                  <|
                    List.take model.rows.visible <|
                        displayImages layout model.gallery.width model.partition []
                ]

        Just image ->
            zoomImage image


displayImages : List Image -> Float -> KPartition Int -> List (Html Msg) -> List (Html Msg)
displayImages images viewportWidth partition imageRows =
    case partition of
        one :: theRest ->
            let
                rowWidth =
                    List.length one

                newImageRows =
                    displayRowOfImages (List.take rowWidth images) viewportWidth :: imageRows
            in
            displayImages (List.drop rowWidth images) viewportWidth theRest newImageRows

        one ->
            let
                rowOfImages =
                    List.take (List.length one) images
            in
            displayRowOfImages rowOfImages viewportWidth :: imageRows


displayRowOfImages : List Image -> Float -> Html Msg
displayRowOfImages images viewportWidth =
    let
        arSum =
            summedAspectRatios images

        widths =
            List.reverse <| getWidths images viewportWidth arSum []

        h =
            floor (viewportWidth / arSum)
    in
    div [ Html.Attributes.class "flex" ] <| List.map2 (\img w -> displayImage img w h) images widths


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


zoomImage : Image -> Html Msg
zoomImage image =
    div [ Html.Attributes.class "zoombox" ]
        [ Html.img [ Html.Attributes.class "blur", src (blurURL image) ] []
        , Html.img
            [ src (imageURL image)
            , Html.Attributes.class "zoom"
            ]
            []
        , div
            [ Html.Attributes.class "control"
            , onClick (ZoomImage Nothing)
            ]
            []
        ]



-- Partition Helpers


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



-- Map Helpers


drawGlobe : Html Msg
drawGlobe =
    Svg.svg
        [ Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        , Svg.Attributes.viewBox "0 0 400 400"
        ]
        [ Svg.circle
            [ Svg.Attributes.cx "200"
            , Svg.Attributes.cy "200"
            , Svg.Attributes.r "190"
            ]
            []
        ]

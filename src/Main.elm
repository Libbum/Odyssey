module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Element
import Element.Events
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html, a, div)
import Html.Attributes exposing (height, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Manifest exposing (Country(..), Image, Location(..), Trip(..), byCountry, byLocation, byTrip, dateOrderLatest, dateOrderOldest, imageURL, locale, manifest, thumbURL)
import Partition exposing (KPartition, greedyK)
import Task
import Url.Builder


main : Program () Model Msg
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
    , viewportWidth : Float
    , locale : String
    , zoom : Maybe Image
    }


initialModel : Model
initialModel =
    { partition = []
    , images = manifest
    , viewportWidth = 0
    , locale = ""
    , zoom = Nothing
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , getPartition
    )


getRatios : List Image -> List Float
getRatios =
    List.map .aspectRatio



--- Update


type Msg
    = RePartition
    | Partition (Result Browser.Dom.Error Browser.Dom.Viewport)
    | PutLocale String
    | PopLocale
    | ZoomImage (Maybe Image)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- GALLERY
        RePartition ->
            ( model, getPartition )

        Partition result ->
            case result of
                Ok vp ->
                    let
                        ratios =
                            getRatios model.images

                        rowsBest =
                            optimalRowCount ratios vp.viewport.width vp.viewport.height
                    in
                    ( { model
                        | partition = greedyK (weights ratios) rowsBest
                        , viewportWidth = vp.viewport.width
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        -- VIEW CHANGES
        PutLocale locale ->
            ( { model | locale = locale }, Cmd.none )

        PopLocale ->
            ( { model | locale = "" }, Cmd.none )

        -- IMAGE VIEWER
        ZoomImage image ->
            ( { model | zoom = image }, Cmd.none )


getPartition : Cmd Msg
getPartition =
    Task.attempt Partition getViewport



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> RePartition)



--- View


view : Model -> Html Msg
view model =
    case model.zoom of
        Nothing ->
            Element.layout [] <|
                Element.row [ Element.width Element.fill ]
                    [ navigation
                    , gallery model
                    ]

        -- displayImages layout model.viewportWidth model.partition []
        Just image ->
            Element.layout [] <| showImage image (floor model.viewportWidth)


gallery : Model -> Element.Element Msg
gallery model =
    let
        layout =
            List.sortWith dateOrderLatest model.images
                |> List.filter (byTrip Summer2017)
    in
    Element.row [ Region.mainContent, Element.width Element.fill ] <|
        displayImages layout model.viewportWidth model.partition []


navigation : Element.Element Msg
navigation =
    Element.row [ Region.navigation, Element.width Element.fill ]
        [ Input.button []
            { onPress = Just RePartition
            , label = Element.text "Repartition"
            }
        ]


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


displayImages : List Image -> Float -> KPartition Int -> List (Element.Element Msg) -> List (Element.Element Msg)
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


displayRowOfImages : List Image -> Float -> Element.Element Msg
displayRowOfImages images viewportWidth =
    let
        arSum =
            summedAspectRatios images

        widths =
            List.reverse <| getWidths images viewportWidth arSum []

        h =
            floor (viewportWidth / arSum)
    in
    Element.row [] <| List.map2 (\img w -> displayImage img w h) images widths


displayImage : Image -> Float -> Int -> Element.Element Msg
displayImage image w h =
    -- Note the - 8 here on the width is to take into account the two 4px margins in resets.css
    -- We alse send in a float as the width attribute to clean up the right edge
    Element.image
        [ Element.width (Element.px (floor <| w - 8.0)) --TODO: This kills our Float px fix
        , Element.height (Element.px h)
        , Element.Events.onClick (ZoomImage <| Just image)
        , Element.Events.onMouseEnter (PutLocale <| locale image)
        , Element.Events.onMouseLeave PopLocale
        , Element.spacing 4
        ]
        { src = thumbURL image
        , description = locale image
        }


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


summedAspectRatios : List Image -> Float
summedAspectRatios images =
    List.foldl (+) 0 (getRatios images)


showImage : Image -> Int -> Element.Element Msg
showImage image viewportWidth =
    Element.image
        [ Element.Events.onClick (ZoomImage Nothing)
        , Element.width (Element.px viewportWidth)
        ]
        { src = imageURL image
        , description = locale image
        }

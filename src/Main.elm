module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Html exposing (Html, a, div)
import Html.Attributes exposing (height, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Manifest exposing (Country(..), Filter(..), Image, Location(..), SortOrder(..), Trip(..), filterImages, imageURL, locale, manifest, sortImages, thumbURL)
import Partition exposing (KPartition, greedyK)
import Task


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
    , sort : SortOrder
    , filter : Filter
    , viewportWidth : Float
    , locale : String
    , zoom : Maybe Image
    }


initialModel : Model
initialModel =
    { partition = []
    , images = manifest
    , sort = DateNewest
    , filter = All
    , viewportWidth = 0
    , locale = ""
    , zoom = Nothing
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , getPartition
    )



--- Update


type Msg
    = RePartition
    | Partition (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ToggleOrder
    | ToggleFilter
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
                            getRatios <| filterImages model.filter model.images

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
                            ByCountry Germany

                        _ ->
                            All
            in
            ( { model | filter = newFilter }, getPartition )

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
            let
                layout =
                    model.images
                        |> filterImages model.filter
                        |> sortImages model.sort
            in
            div []
                [ div []
                    [ Html.button [ onClick ToggleOrder ] [ Html.text "Toggle Order" ]
                    , Html.button [ onClick ToggleFilter ] [ Html.text "Toggle Filter" ]
                    ]
                , div
                    [ Html.Attributes.id "gallery" ]
                  <|
                    displayImages layout model.viewportWidth model.partition []
                ]

        Just image ->
            showImage image (floor model.viewportWidth)


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
    div [] <| List.map2 (\img w -> displayImage img w h) images widths


displayImage : Image -> Float -> Int -> Html Msg
displayImage image w h =
    -- Note the - 8 here on the width is to take into account the two 4px margins in resets.css
    -- We alse send in a float as the width attribute to clean up the right edge
    Html.img
        [ src (thumbURL image)
        , Html.Attributes.attribute "width" (String.fromFloat <| w - 8.0)
        , height h
        , onClick (ZoomImage <| Just image)
        , onMouseEnter (PutLocale <| locale image)
        , onMouseLeave PopLocale
        ]
        []


showImage : Image -> Int -> Html Msg
showImage image viewportWidth =
    Html.img
        [ src (imageURL image)
        , onClick (ZoomImage Nothing)
        , width viewportWidth
        ]
        []



-- Helpers


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

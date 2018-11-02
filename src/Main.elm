module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom exposing (getViewportOf)
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, input, li, text, ul)
import Html.Attributes exposing (height, href, src, type_, value, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Partition exposing (KPartition, greedyK, sumOfKSets)
import Task
import Url exposing (Url)
import Url.Builder


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



--- Model


type alias Model =
    { key : Nav.Key
    , url : Url
    , partition : KPartition Int
    , images : List Image
    , viewportWidth : Float
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [] [] 0
    , Http.send LoadManifest (Http.get manifest manifestDecoder)
    )


manifest : String
manifest =
    Url.Builder.relative [ "..", "manifest.json" ] []


type alias Image =
    { thumbnail : String
    , full : String
    , aspectRatio : Float
    , description : String
    , locale : String
    }


manifestDecoder : Decoder (List Image)
manifestDecoder =
    Json.Decode.list <|
        Json.Decode.map5 Image
            (Json.Decode.field "small" Json.Decode.string)
            (Json.Decode.field "big" Json.Decode.string)
            (Json.Decode.field "aspect_ratio" Json.Decode.float)
            (Json.Decode.field "desc" Json.Decode.string)
            (Json.Decode.field "locale" Json.Decode.string)


getRatios : List Image -> List Float
getRatios =
    List.map .aspectRatio



--- Update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoadManifest (Result Http.Error (List Image))
    | RePartition
    | Partition (Result Browser.Dom.Error Browser.Dom.Viewport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        LoadManifest result ->
            case result of
                Ok imageList ->
                    ( { model | images = imageList }
                    , getPartition
                    )

                Err _ ->
                    ( model, Cmd.none )

        RePartition ->
            ( model, getPartition )

        Partition result ->
            case result of
                Ok vp ->
                    let
                        ratios =
                            getRatios model.images

                        rowsBest =
                            optimalRowCount ratios vp.viewport.width vp.scene.height
                    in
                    ( { model
                        | partition = greedyK (weights ratios) rowsBest
                        , viewportWidth = vp.viewport.width
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


getPartition : Cmd Msg
getPartition =
    Task.attempt Partition (getViewportOf "gallery")



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> RePartition)



--- View


view : Model -> Browser.Document Msg
view model =
    { title = "Iridescence"
    , body =
        [ div [ Html.Attributes.id "gallery" ] <|
            displayImages model.images model.viewportWidth model.partition []
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


weights : List Float -> List Int
weights =
    List.map (\p -> floor (p * 100))


optimalRowCount : List Float -> Float -> Float -> Int
optimalRowCount imageRatios viewportWidth sceneHeight =
    let
        idealHeight =
            sceneHeight / 4.0

        summedWidth =
            imageRatios |> List.map (\r -> r * 100.0) |> List.foldl (+) 0
    in
    round (summedWidth / viewportWidth)


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


displayImage : Image -> Int -> Int -> Html Msg
displayImage image w h =
    Html.img [ src image.thumbnail, width w, height h ] []


getWidths : List Image -> Float -> Float -> List Int -> List Int
getWidths images viewportWidth arSum widths =
    case images of
        one :: theRest ->
            let
                w =
                    floor (viewportWidth / arSum * one.aspectRatio)
            in
            getWidths theRest viewportWidth arSum (w :: widths)

        one ->
            floor viewportWidth - List.sum widths :: widths


summedAspectRatios : List Image -> Float
summedAspectRatios images =
    List.foldl (+) 0 (getRatios images)

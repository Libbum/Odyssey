module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, input, li, text, ul)
import Html.Attributes exposing (href, type_, value)
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
    , rows : KPartition Int
    , images : List Image
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [] []
    , Task.sequence
        [ Http.send LoadManifest (Http.get manifest manifestDecoder)
        , Task.attempt GotViewPort getViewport
        ]
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
getRatios images =
    List.map .aspectRatio images



--- Update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoadManifest (Result Http.Error (List Image))
    | GetViewPort
    | GotViewPort (Result () Browser.Dom.Viewport)


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
                    ( { model | images = imageList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetViewPort ->
            ( model, Task.attempt GotViewPort getViewport )

        GotViewPort result ->
            case result of
                Ok vp ->
                    let
                        ratios =
                            getRatios model.images

                        rowsBest =
                            optimalRowCount ratios vp.viewport.width vp.scene.height
                    in
                    ( { model | rows = greedyK (weights ratios) rowsBest }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> GetViewPort)



--- View


view : Model -> Browser.Document Msg
view model =
    { title = "Iridescence"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            ]
        , div [] [ text (Debug.toString <| getRatios model.images) ]
        , div [] [ text (Debug.toString model.rows) ]
        , div [] [ text (String.fromInt <| List.length model.rows) ]
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


weights : List Float -> List Int
weights =
    List.map (\p -> floor (p * 100))


stats : List Int -> String
stats lst =
    String.concat
        [ String.fromFloat (mean lst)
        , "±"
        , String.fromFloat (std lst)
        ]


mean : List Int -> Float
mean lst =
    toFloat (List.sum lst) / toFloat (List.length lst)


meanf : List Float -> Float
meanf lst =
    List.sum lst / toFloat (List.length lst)


std : List Int -> Float
std lst =
    let
        seriesMean =
            mean lst
    in
    lst
        |> List.map (\n -> (toFloat n - seriesMean) ^ 2)
        |> meanf
        |> sqrt


optimalRowCount : List Float -> Float -> Float -> Int
optimalRowCount imageRatios viewportWidth sceneHeight =
    let
        idealHeight =
            sceneHeight / 4.0

        summedWidth =
            imageRatios |> List.map (\r -> r * 100.0) |> List.foldl (+) 0
    in
    round (summedWidth / viewportWidth)

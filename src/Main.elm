module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, input, li, text, ul)
import Html.Attributes exposing (href, type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Partition exposing (Partition, largestDifference)
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
    , left : List Int
    , right : List Int
    , images : List Image
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [] [] [], Http.send LoadManifest (Http.get manifest manifestDecoder) )


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
    | GetPartition
    | LoadManifest (Result Http.Error (List Image))


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

        GetPartition ->
            let
                ( left, right ) =
                    largestDifference <| weights <| getRatios model.images
            in
            ( { model | left = left, right = right }, Cmd.none )

        LoadManifest result ->
            case result of
                Ok imageList ->
                    ( { model | images = imageList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
        , button [ onClick GetPartition ] [ text "partition" ]
        , div [] [ text (Debug.toString model.left) ]
        , div [] [ text (Debug.toString model.right) ]
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


weights : List Float -> List Int
weights =
    List.map (\p -> floor (p * 100))

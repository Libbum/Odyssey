module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, input, li, text, ul)
import Html.Attributes exposing (href, type_, value)
import Html.Events exposing (onClick)
import Partition exposing (Partition, largestDifference)
import Task
import Url


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
    , url : Url.Url
    , sequence : List Float
    , left : List Int
    , right : List Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [ 2.2598870056497176, 0.6665, 1.5, 2.094240837696335, 1.6835016835016836, 1.7777777777777777, 1.8050541516245486, 1.7777777777777777, 0.5625, 0.4975, 1.3540961408259986, 0.4135, 0.6875, 1.7777777777777777, 0.5625, 2.242152466367713, 0.544, 0.5845, 0.494, 1.4992503748125936 ] [] [], Cmd.none )



--- Update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GetPartition


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
                    largestDifference <| weights model.sequence
            in
            ( { model | left = left, right = right }, Cmd.none )



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
            , viewLink "/reviews/the-century-of-the-self"
            , viewLink "/reviews/public-opinion"
            , viewLink "/reviews/shah-of-shahs"
            ]
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

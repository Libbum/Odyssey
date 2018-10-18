module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)
import Partition exposing (Partition, largestDifference)


main =
    Browser.sandbox { init = "[ 8, 7, 6, 5, 4 ]", update = update, view = view }


type Msg
    = Partition


update msg model =
    case msg of
        Partition ->
            model


view model =
    let
        ( first, second ) =
            largestDifference [ 8, 7, 6, 5, 4 ]
    in
    div []
        [ input [ type_ "text", value model ] []
        , button [ onClick Partition ] [ text "partition" ]
        , div [] [ text (Debug.toString first) ]
        , div [] [ text (Debug.toString second) ]
        ]

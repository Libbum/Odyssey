port module Ports exposing (nearBottom, viewCountry, viewLocation, viewTrip)


port nearBottom : (Bool -> msg) -> Sub msg


port viewCountry : String -> Cmd msg


port viewLocation : ( String, List Float ) -> Cmd msg


port viewTrip : String -> Cmd msg

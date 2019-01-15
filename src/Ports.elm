port module Ports exposing (drawMap, initMap, nearBottom, showLocation, viewAll, viewCountry, viewLocation, viewTrip)


port nearBottom : (Bool -> msg) -> Sub msg


port initMap : ( Int, String, List Float ) -> Cmd msg


port drawMap : () -> Cmd msg


port viewAll : () -> Cmd msg


port viewCountry : String -> Cmd msg


port viewLocation : ( String, List Float ) -> Cmd msg


port showLocation : ( String, List Float ) -> Cmd msg


port viewTrip : String -> Cmd msg

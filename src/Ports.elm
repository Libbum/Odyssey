port module Ports exposing (nearBottom, viewTrip)


port nearBottom : (Bool -> msg) -> Sub msg


port viewTrip : String -> Cmd msg

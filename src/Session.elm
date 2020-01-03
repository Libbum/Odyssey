module Session exposing (Session, fromString, header, toString)

import Http


type Session
    = Session String


fromString : String -> Session
fromString str =
    Session str


toString : Session -> String
toString (Session val) =
    val


header : Session -> Http.Header
header (Session uuid) =
    Http.header "session" uuid

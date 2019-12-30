module Contact exposing (Captcha, Error(..), Model, Msg(..), Response(..))

import Http
import Session exposing (Session)


type alias Model =
    { name : String
    , email : String

    --, website : String
    , message : String
    , challenge : String
    , captcha : Captcha
    , session : Maybe Session
    , response : Response
    , formEnabled : Bool
    }


type Msg
    = RequestCaptcha
    | GotCaptchaImage (Result Http.Error ( Captcha, Maybe Session ))
    | SendContact
    | ConfirmSendContact (Result Error String)
    | UpdateName String
    | UpdateEmail String
    | UpdateWebsite String
    | UpdateSubject String
    | UpdateMessage String
    | UpdateChallenge String


type alias Captcha =
    String


type Response
    = Good String
    | Bad String
    | NotSent


type Error
    = SessionExpired
    | InvalidChallenge
    | NoSessionHeader
    | Http Http.Error

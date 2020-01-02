module Contact exposing (Model, Msg(..), getCaptcha, init, update, view)

import Dict
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Body, Expect)
import Icons
import Json.Encode as Encode
import Session exposing (Session)


type alias Model =
    { name : String
    , email : String
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
    | UpdateMessage String
    | UpdateChallenge String
    | CloseModal


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


init : Model
init =
    Model "" "" "" "" "" Nothing NotSent True



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg model =
    case msg of
        RequestCaptcha ->
            ( model
            , getCaptcha model.session
            , True
            )

        GotCaptchaImage (Ok ( image, session )) ->
            ( { model | captcha = image, session = session, response = NotSent }
            , Cmd.none
            , True
            )

        GotCaptchaImage (Err err) ->
            let
                ( response, formEnabled ) =
                    case err of
                        Http.NetworkError ->
                            ( "Contact system is currently offline. Please try again later.", False )

                        Http.BadStatus 500 ->
                            ( "Cannot generate a captcha image. Please try again later.", False )

                        Http.BadStatus 503 ->
                            ( "Please refresh a little slower, we have limited your requests.", True )

                        _ ->
                            ( "Something is currently wong with the contact system. The administrator has been notified. Please try again later.", False )
            in
            ( { model | response = Bad response, formEnabled = formEnabled }
            , Cmd.none
            , True
            )

        SendContact ->
            ( model
            , sendContactRequest model
            , True
            )

        ConfirmSendContact (Ok _) ->
            ( { model | response = Good "Your message has been recieved! We'll get back to you as soon as we can.", name = "", email = "", message = "", challenge = "", formEnabled = False }, getCaptcha Nothing, True )

        ConfirmSendContact (Err err) ->
            let
                ( response, cmd, formEnabled ) =
                    case err of
                        InvalidChallenge ->
                            ( "The captcha challenge value you entered was incorrect. Please try again.", Cmd.none, True )

                        SessionExpired ->
                            ( "Your session expired. Please type in the new value of the captcha and resend your message.", getCaptcha Nothing, True )

                        NoSessionHeader ->
                            ( "Your browser didn't send me a session value, I can't confirm you're not a bot. Please try again with this new captcha.", getCaptcha Nothing, True )

                        _ ->
                            ( "An error occured processing your message. Please try again or a little later. The administrator has been notified of this failure so will hopefully fix this issue soon.", getCaptcha Nothing, True )
            in
            ( { model | response = Bad response, challenge = "", formEnabled = formEnabled }, cmd, True )

        UpdateName name ->
            ( { model | name = name }, Cmd.none, True )

        UpdateEmail email ->
            ( { model | email = email }, Cmd.none, True )

        UpdateMessage message ->
            ( { model | message = message }, Cmd.none, True )

        UpdateChallenge challenge ->
            ( { model | challenge = challenge }, Cmd.none, True )

        CloseModal ->
            ( model, Cmd.none, False )



--- REQUESTS


buildHeaders : Maybe Session -> List Http.Header
buildHeaders maybeSession =
    case maybeSession of
        Just session ->
            [ Session.header session ]

        Nothing ->
            []


getCaptcha : Maybe Session -> Cmd Msg
getCaptcha session =
    Http.request
        { method = "GET"
        , headers = buildHeaders session
        , url = "https://www.exactlyinfinite.com/notify/captcha"

        --, url = "https://odyssey.neophilus.net/notify/captcha"
        --, url = "http://127.0.0.1:7361/captcha"
        , body = Http.emptyBody
        , expect = expectCaptcha GotCaptchaImage
        , timeout = Nothing
        , tracker = Nothing
        }


sendContactRequest : Model -> Cmd Msg
sendContactRequest model =
    sendContactRequestHelper (contact model) (buildHeaders model.session)


sendContactRequestHelper : Encode.Value -> List Http.Header -> Cmd Msg
sendContactRequestHelper contact_block headers =
    Http.request
        { method = "POST"
        , headers = headers

        --, url = "https://odyssey.neophilus.net/notify/contact"
        , url = "https://www.exactlyinfinite.com/notify/contact"

        --, url = "http://127.0.0.1:7361/contact"
        , body = Http.jsonBody contact_block
        , expect = expectContactConfirm ConfirmSendContact
        , timeout = Nothing
        , tracker = Nothing
        }


expectCaptcha : (Result Http.Error ( Captcha, Maybe Session ) -> msg) -> Http.Expect msg
expectCaptcha toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok ( body, Dict.get "session" metadata.headers |> Maybe.map Session.fromString )


expectContactConfirm : (Result Error String -> msg) -> Http.Expect msg
expectContactConfirm toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| Http (Http.BadUrl url)

                Http.Timeout_ ->
                    Err <| Http Http.Timeout

                Http.NetworkError_ ->
                    Err <| Http Http.NetworkError

                Http.BadStatus_ metadata body ->
                    let
                        err_type =
                            case metadata.statusCode of
                                400 ->
                                    case body of
                                        "Session Expired" ->
                                            SessionExpired

                                        "Invalid challenge value" ->
                                            InvalidChallenge

                                        _ ->
                                            Http (Http.BadStatus metadata.statusCode)

                                401 ->
                                    NoSessionHeader

                                _ ->
                                    Http (Http.BadStatus metadata.statusCode)
                    in
                    Err err_type

                Http.GoodStatus_ _ body ->
                    Ok body



-- FORM PROCESSING


contact : Model -> Encode.Value
contact model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "email", Encode.string model.email )
        , ( "website", Encode.string "" )
        , ( "subject", Encode.string "" )
        , ( "message", Encode.string model.message )
        , ( "challenge", Encode.string model.challenge )
        ]



-- VIEW


view : Bool -> Model -> Html Msg
view show model =
    let
        modal =
            if show then
                [ Html.Attributes.class "modal" ]

            else
                [ Html.Attributes.class "modal", Html.Attributes.class "off" ]

        ( responseClass, responseText ) =
            case model.response of
                Good response ->
                    ( "green", response )

                Bad error ->
                    ( "red", error )

                NotSent ->
                    ( "", "" )
    in
    if model.formEnabled then
        div modal
            [ Html.button [ Html.Attributes.class "close", onClick CloseModal ] [ Icons.x ]
            , Html.form [ Html.Attributes.id "contactModal", Html.Attributes.method "post", onSubmit SendContact ]
                [ Html.input [ Html.Attributes.required True, Html.Attributes.placeholder "Name", Html.Attributes.type_ "text", Html.Attributes.name "name", Html.Attributes.value model.name, onInput UpdateName ] []
                , Html.input [ Html.Attributes.required True, Html.Attributes.placeholder "Email", Html.Attributes.type_ "email", Html.Attributes.name "email", Html.Attributes.value model.email, onInput UpdateEmail ] []
                , Html.textarea [ Html.Attributes.required True, Html.Attributes.placeholder "Message", Html.Attributes.spellcheck True, Html.Attributes.rows 4, Html.Attributes.name "message", Html.Attributes.value model.message, onInput UpdateMessage ] []
                , div [ Html.Attributes.class "captcha" ]
                    [ Html.img [ Html.Attributes.class "img-verify", Html.Attributes.src model.captcha ] []
                    , div [ Html.Attributes.class "captchacontrol" ]
                        [ Html.button [ onClick RequestCaptcha ] [ Html.text "Refresh" ]
                        , Html.input [ Html.Attributes.class "verify", Html.Attributes.autocomplete False, Html.Attributes.required True, Html.Attributes.placeholder "Copy code", Html.Attributes.type_ "text", Html.Attributes.name "verify", Html.Attributes.title "This confirms you are a human user or strong AI and not a spam-bot.", Html.Attributes.value model.challenge, onInput UpdateChallenge ] []
                        ]
                    ]
                , div [ Html.Attributes.class "center" ]
                    [ Html.input [ Html.Attributes.type_ "submit", Html.Attributes.value "Send Message" ] []
                    , div [ Html.Attributes.class "response", Html.Attributes.class responseClass ] [ Html.text responseText ]
                    ]
                ]
            ]

    else
        let
            alternate =
                if responseClass == "green" then
                    Html.text ""

                else
                    div [ Html.Attributes.class "alternate" ]
                        [ Html.text "Alternatively, contact Tim directly on "
                        , Html.a [ Html.Attributes.href "https://keybase.io/Libbum" ] [ Html.text "Keybase" ]
                        , Html.text " or "
                        , Html.a [ Html.Attributes.href "https://telegram.me/Libbum" ] [ Html.text "Telegram" ]
                        , Html.text "."
                        ]
        in
        div (Html.Attributes.class "disabled" :: modal)
            [ Html.button [ Html.Attributes.class "close", onClick CloseModal ] [ Icons.x ]
            , div [ Html.Attributes.class "response", Html.Attributes.class responseClass ]
                [ Html.text responseText
                , alternate
                ]
            ]

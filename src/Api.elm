module Api exposing (getCaptcha, sendContactRequest)

import Contact exposing (Captcha, Error(..), Model, Msg(..))
import Dict
import Http exposing (Body, Expect)
import Json.Encode as Encode
import Session exposing (Session)



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
        , url = "https://odyssey.neophilus.net/notify/captcha"
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
        , url = "https://odyssey.neophilus.net/notify/contact"
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


expectContactConfirm : (Result Contact.Error String -> msg) -> Http.Expect msg
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

        --, ( "website", Encode.string model.website )
        , ( "message", Encode.string model.message )
        , ( "challenge", Encode.string model.challenge )
        ]

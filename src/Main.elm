module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, a, div)
import Html.Attributes exposing (height, href, src, width)
import Http
import Json.Decode exposing (Decoder)
import Partition exposing (KPartition, greedyK)
import Task
import Url.Builder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--- Model


type alias Model =
    { partition : KPartition Int
    , images : List Image
    , viewportWidth : Float
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model [] [] 0
    , Http.send LoadManifest (Http.get manifest manifestDecoder)
    )


manifest : String
manifest =
    Url.Builder.relative [ "..", "manifester/manifest.json" ] []



{- Consider expanding path to country, data, location to better search for an image.
   Should we consider merging the manifiest into a module in general? Make country a union type etc?
-}


type alias Image =
    { file : String
    , thumb : String
    , path : String
    , description : String
    , locale : String
    , aspectRatio : Float
    }


type alias ManifestImage =
    { file : String
    , description : String
    , locale : String
    , aspectRatio : Float
    }


type alias Manifest =
    Dict String (Dict String (Dict String (Dict String (List ManifestImage))))


manifestDecoder : Decoder Manifest
manifestDecoder =
    Json.Decode.dict <|
        Json.Decode.dict <|
            Json.Decode.dict <|
                Json.Decode.dict imageListDecoder


imageListDecoder : Decoder (List ManifestImage)
imageListDecoder =
    Json.Decode.list <|
        Json.Decode.map4 ManifestImage
            (Json.Decode.field "file" Json.Decode.string)
            (Json.Decode.field "desc" Json.Decode.string)
            (Json.Decode.field "loc" Json.Decode.string)
            (Json.Decode.field "ar" Json.Decode.float)


parseManifest : Manifest -> List Image
parseManifest mani =
    Dict.map
        (\year mnth ->
            Dict.map
                (\month cntry ->
                    Dict.map
                        (\country loc ->
                            Dict.foldl unwrapManifestList [] loc
                                |> List.map (\i -> { i | path = String.join "/" [ "gallery", year, month, country, i.path ] })
                        )
                        cntry
                        |> Dict.values
                        |> List.concat
                )
                mnth
                |> Dict.values
                |> List.concat
        )
        mani
        |> Dict.values
        |> List.concat


unwrapManifestList : String -> List ManifestImage -> List Image -> List Image
unwrapManifestList location manifestImages images =
    List.map
        (\m ->
            Image m.file (thumbnailFromFile m.file) location m.description m.locale m.aspectRatio
        )
        manifestImages
        ++ images


getRatios : List Image -> List Float
getRatios =
    List.map .aspectRatio



--- Update


type Msg
    = LoadManifest (Result Http.Error Manifest)
    | RePartition
    | Partition (Result Browser.Dom.Error Browser.Dom.Viewport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadManifest result ->
            case result of
                Ok mani ->
                    ( { model | images = parseManifest mani }
                    , getPartition
                    )

                Err err ->
                    let
                        a =
                            Debug.log "Manifest" err
                    in
                    ( model, Cmd.none )

        RePartition ->
            ( model, getPartition )

        Partition result ->
            case result of
                Ok vp ->
                    let
                        ratios =
                            getRatios model.images

                        rowsBest =
                            optimalRowCount ratios vp.viewport.width vp.viewport.height
                    in
                    ( { model
                        | partition = greedyK (weights ratios) rowsBest
                        , viewportWidth = vp.viewport.width
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


getPartition : Cmd Msg
getPartition =
    Task.attempt Partition getViewport



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> RePartition)



--- View


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "gallery" ] <|
        displayImages model.images model.viewportWidth model.partition []


weights : List Float -> List Int
weights =
    List.map (\p -> floor (p * 100))


optimalRowCount : List Float -> Float -> Float -> Int
optimalRowCount imageRatios viewportWidth sceneHeight =
    let
        idealHeight =
            sceneHeight / 4.0

        summedWidth =
            imageRatios |> List.map (\r -> r * idealHeight) |> List.foldl (+) 0
    in
    round (summedWidth / viewportWidth)


displayImages : List Image -> Float -> KPartition Int -> List (Html Msg) -> List (Html Msg)
displayImages images viewportWidth partition imageRows =
    case partition of
        one :: theRest ->
            let
                rowWidth =
                    List.length one

                newImageRows =
                    displayRowOfImages (List.take rowWidth images) viewportWidth :: imageRows
            in
            displayImages (List.drop rowWidth images) viewportWidth theRest newImageRows

        one ->
            let
                rowOfImages =
                    List.take (List.length one) images
            in
            displayRowOfImages rowOfImages viewportWidth :: imageRows


displayRowOfImages : List Image -> Float -> Html Msg
displayRowOfImages images viewportWidth =
    let
        arSum =
            summedAspectRatios images

        widths =
            List.reverse <| getWidths images viewportWidth arSum []

        h =
            floor (viewportWidth / arSum)
    in
    div [] <| List.map2 (\img w -> displayImage img w h) images widths


displayImage : Image -> Float -> Int -> Html Msg
displayImage image w h =
    -- Note the - 8 here on the width is to take into account the two 4px margins in resets.css
    -- We also send in a float as the width attribute to clean up the right edge
    Html.img [ src (String.join "/" [ image.path, image.thumb ]), Html.Attributes.attribute "width" (String.fromFloat <| w - 8.0), height h ] []


getWidths : List Image -> Float -> Float -> List Float -> List Float
getWidths images viewportWidth arSum widths =
    case images of
        one :: theRest ->
            let
                w =
                    viewportWidth / arSum * one.aspectRatio
            in
            getWidths theRest viewportWidth arSum (w :: widths)

        one ->
            viewportWidth - List.sum widths :: widths


summedAspectRatios : List Image -> Float
summedAspectRatios images =
    List.foldl (+) 0 (getRatios images)


imgURL : Image -> String
imgURL image =
    String.join "/" [ image.path, image.file ]


thumbURL : Image -> String
thumbURL image =
    String.join "/" [ image.path, image.file ]


thumbnailFromFile : String -> String
thumbnailFromFile file =
    let
        splitfile =
            unconsLast <| String.split "." file
    in
    case splitfile of
        Just ( ext, splitname ) ->
            let
                name =
                    String.join "." splitname
            in
            String.join "_small." [ name, ext ]

        Nothing ->
            ""


unconsLast : List a -> Maybe ( a, List a )
unconsLast list =
    --List.Extra
    case List.reverse list of
        [] ->
            Nothing

        last_ :: rest ->
            ( last_, List.reverse rest )
                |> Just

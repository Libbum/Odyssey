module Gallery exposing (Filter(..), blurURL, countryNames, displayURL, filterImages, imageURL, locale, locationCoordinates, locationNames, sortImages, thumbURL, tripId, tripNames)

import List.Extra exposing (unconsLast)
import Manifest exposing (Country(..), Date, Image, Location(..), Month(..), Trip(..), Year)
import Ordering exposing (Ordering)


type Alternate
    = Thumb
    | Blur


imagePath : Image -> String
imagePath image =
    let
        info =
            Manifest.locationInformation image.location
    in
    String.join "/"
        [ "/gallery"
        , String.fromInt image.date.year
        , monthToDirectory image.date.month
        , Manifest.countryName info.country |> String.replace " " "_"
        , String.replace " " "_" info.name
        ]


imageURL : Image -> String
imageURL image =
    imagePath image ++ "/" ++ image.file


thumbURL : Image -> String
thumbURL image =
    String.join "/" [ imagePath image, alternateFile Thumb image.file ]


blurURL : Image -> String
blurURL image =
    String.join "/" [ imagePath image, alternateFile Blur image.file ]


displayURL : Image -> String
displayURL image =
    let
        info =
            Manifest.locationInformation image.location

        countryString =
            Manifest.countryName info.country
    in
    "/" ++ countryString ++ "/" ++ info.name


alternateFile : Alternate -> String -> String
alternateFile alt file =
    let
        -- We split here and then join the name later to catch `file.name.jpg` conventions (if they are used)
        splitFile =
            unconsLast <| String.split "." file
    in
    case splitFile of
        Just ( ext, splitName ) ->
            let
                name =
                    String.join "." splitName
            in
            case alt of
                Thumb ->
                    String.join "_small." [ name, ext ]

                Blur ->
                    String.join "_blur." [ name, ext ]

        Nothing ->
            -- Unsure if it's best to return the image or fail here. It'll look nicer with the image, but use more bandwidth
            file


countryNames : List String
countryNames =
    List.map Manifest.countryName Manifest.countryList


locationNames : List String
locationNames =
    List.map (\location -> Manifest.locationInformation location |> .name) Manifest.locationList


locationCoordinates : Location -> ( Float, Float )
locationCoordinates location =
    Manifest.locationInformation location |> .coordinates


locale : Image -> ( String, String )
locale image =
    let
        info =
            Manifest.locationInformation image.location

        countryString =
            Manifest.countryName info.country

        date =
            dateString image.date
    in
    case Manifest.countryLocalName info.country of
        Just localCountry ->
            case Manifest.locationLocalName image.location of
                Just localLocation ->
                    ( localLocation ++ ", " ++ localCountry ++ " (" ++ info.name ++ ", " ++ countryString ++ "); " ++ date ++ ".", info.name )

                Nothing ->
                    ( info.name ++ ", " ++ countryString ++ " (" ++ localCountry ++ "); " ++ date ++ ".", info.name )

        Nothing ->
            case Manifest.locationLocalName image.location of
                Just localLocation ->
                    ( localLocation ++ " (" ++ info.name ++ "), " ++ countryString ++ "; " ++ date ++ ".", info.name )

                Nothing ->
                    ( info.name ++ ", " ++ countryString ++ "; " ++ date ++ ".", info.name )


dateString : Date -> String
dateString date =
    let
        monthString =
            case date.month of
                Jan ->
                    "Jan "

                Feb ->
                    "Feb "

                Mar ->
                    "Mar "

                Apr ->
                    "Apr "

                May ->
                    "May "

                Jun ->
                    "Jun "

                Jul ->
                    "Jul "

                Aug ->
                    "Aug "

                Sep ->
                    "Sep "

                Oct ->
                    "Oct "

                Nov ->
                    "Nov "

                Dec ->
                    "Dec "
    in
    monthString ++ String.fromInt date.year


tripNames : List String
tripNames =
    List.map (\trip -> Manifest.tripInformation trip |> .description) Manifest.tripList
        |> List.reverse


tripId : Trip -> String
tripId trip =
    Manifest.tripInformation trip |> .name


{-| Generate a directory string for our month, padding the int to a width of 2.
-}
monthToDirectory : Month -> String
monthToDirectory month =
    let
        monthInt =
            case month of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12
    in
    monthInt
        |> String.fromInt
        |> String.padLeft 2
            '0'



-- ORDERING


sortImages : List Image -> List Image
sortImages =
    List.sortWith dateOrderLatest


dateOrderLatest : Ordering Image
dateOrderLatest =
    Ordering.byFieldWith yearOrdering (.date >> .year)
        |> Ordering.breakTiesWith (Ordering.byFieldWith monthOrdering (.date >> .month))
        |> Ordering.breakTiesWith (Ordering.byField .file)


yearOrdering : Ordering Year
yearOrdering =
    Ordering.natural


monthOrdering : Ordering Month
monthOrdering =
    Ordering.explicit [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]



-- FILTERING


type Filter
    = All
    | ByCountry Country
    | ByLocation Location
    | ByTrip Trip


filterImages : Filter -> List Image -> List Image
filterImages filter images =
    case filter of
        All ->
            images

        ByCountry country ->
            List.filter (byCountry country) images

        ByLocation location ->
            List.filter (byLocation location) images

        ByTrip trip ->
            List.filter (byTrip trip) images


byCountry : Country -> Image -> Bool
byCountry country image =
    let
        loc =
            Manifest.locationInformation image.location
    in
    loc.country == country


byLocation : Location -> Image -> Bool
byLocation location image =
    image.location == location


byTrip : Trip -> Image -> Bool
byTrip trip image =
    let
        tripInfo =
            Manifest.tripInformation trip
    in
    -- locations are not unique, but these lists are small, so it's better to traverse and double count rather than
    -- find unique values and then test.
    List.any (\d -> d == image.date) tripInfo.dates && List.any (\l -> l == image.location) tripInfo.locations

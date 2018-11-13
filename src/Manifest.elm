module Manifest exposing (Image, imageURL, locale, manifest, thumbURL)

import List.Extra exposing (unconsLast)



-- Would be good to autogenerate this file


type alias Image =
    { file : String
    , date : ( Year, Month )
    , location : Location
    , trip : Maybe Trip
    , aspectRatio : Float
    , description : String
    }


{-| TODO: There is an assumption here that `image.location.name` has the same value in
the directory structure apart from an undescore replacing whitespace. Country cannot do that
if we keep the d3-geo structure, since the names don't match.

However, this may not actually be the case if we hard code values like we have here.
So ultimately we could simplify the `countryToDirectory` to just `countryName` in the future perhaps.

-}
imagePath : Image -> String
imagePath image =
    let
        ( year, month ) =
            image.date

        info =
            locationInformation image.location
    in
    String.join "/"
        [ "gallery"
        , String.fromInt year
        , monthToDirectory month
        , countryToDirectory info.country
        , String.replace " " "_" info.name
        ]


imageURL : Image -> String
imageURL image =
    imagePath image ++ "/" ++ image.file


thumbURL : Image -> String
thumbURL image =
    String.join "/" [ imagePath image, thumbnailFromFile image.file ]


thumbnailFromFile : String -> String
thumbnailFromFile file =
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
            String.join "_small." [ name, ext ]

        Nothing ->
            -- Unsure if it's best to return the image or fail here. It'll look nicer with the image, but use more bandwidth
            file



-- COUNTRIES


type Country
    = Armenia
    | Australia
    | Austria
    | CzechRepublic
    | Germany
    | Denmark
    | Estonia
    | Finland
    | France
    | FaroeIslands
    | UnitedKingdom
    | Greece
    | HongKong
    | Hungary
    | Iceland
    | Japan
    | Latvia
    | Netherlands
    | Norway
    | Poland
    | Qatar
    | Russia
    | Singapore
    | Slovakia
    | Sweden
    | Thailand
    | Ukraine
    | Vietnam


countryName : Country -> String
countryName country =
    case country of
        Armenia ->
            "Armenia"

        Australia ->
            "Australia"

        Austria ->
            "Austria"

        CzechRepublic ->
            "Czech Republic"

        Germany ->
            "Germany"

        Denmark ->
            "Denmark"

        Estonia ->
            "Estonia"

        Finland ->
            "Finland"

        France ->
            "France"

        FaroeIslands ->
            "Faroe Islands"

        UnitedKingdom ->
            "United Kingdom"

        Greece ->
            "Greece"

        HongKong ->
            "Hong Kong"

        Hungary ->
            "Hungary"

        Iceland ->
            "Iceland"

        Japan ->
            "Japan"

        Latvia ->
            "Latvia"

        Netherlands ->
            "Netherlands"

        Norway ->
            "Norway"

        Poland ->
            "Poland"

        Qatar ->
            "Qatar"

        Russia ->
            "Russia"

        Singapore ->
            "Singapore"

        Slovakia ->
            "Slovakia"

        Sweden ->
            "Sweden"

        Thailand ->
            "Thailand"

        Ukraine ->
            "Ukraine"

        Vietnam ->
            "Vietnam"


{-| TODO: Historically we pull this out of world data. We can probably do that again.
-}
countryLocalName : Country -> Maybe String
countryLocalName country =
    case country of
        Armenia ->
            Just "Հայաստան"

        Austria ->
            Just "Österreich"

        CzechRepublic ->
            Just "Česká republika"

        Germany ->
            Just "Deutschland"

        Denmark ->
            Just "Danmark"

        Estonia ->
            Just "Eesti"

        Finland ->
            Just "Suomi"

        FaroeIslands ->
            Just "Føroyar"

        Greece ->
            Just "Ελλάδα"

        HongKong ->
            Just "香港"

        Hungary ->
            Just "Magyarország"

        Iceland ->
            Just "Ísland"

        Japan ->
            Just "日本"

        Latvia ->
            Just "Latvija"

        Netherlands ->
            Just "Nederland"

        Norway ->
            Just "Norge"

        Poland ->
            Just "Polska"

        Qatar ->
            Just "قطر"

        Russia ->
            Just "Росси́я"

        Slovakia ->
            Just "Slovensko"

        Sweden ->
            Just "Sverige"

        Thailand ->
            Just "ประเทศไทย"

        Ukraine ->
            Just "Україна"

        Vietnam ->
            Just "Việt Nam"

        _ ->
            Nothing


countryToDirectory : Country -> String
countryToDirectory country =
    case country of
        Armenia ->
            "Armenia"

        Australia ->
            "Australia"

        Austria ->
            "Austria"

        CzechRepublic ->
            "Czech_Rep"

        Germany ->
            "Germany"

        Denmark ->
            "Denmark"

        Estonia ->
            "Estonia"

        Finland ->
            "Finland"

        France ->
            "France"

        FaroeIslands ->
            "Faroe_Is"

        UnitedKingdom ->
            "United_Kingdom"

        Greece ->
            "Greece"

        HongKong ->
            "Hong_Kong"

        Hungary ->
            "Hungary"

        Iceland ->
            "Iceland"

        Japan ->
            "Japan"

        Latvia ->
            "Latvia"

        Netherlands ->
            "Netherlands"

        Norway ->
            "Norway"

        Poland ->
            "Poland"

        Qatar ->
            "Qatar"

        Russia ->
            "Russia"

        Singapore ->
            "Singapore"

        Slovakia ->
            "Slovakia"

        Sweden ->
            "Sweden"

        Thailand ->
            "Thailand"

        Ukraine ->
            "Ukraine"

        Vietnam ->
            "Vietnam"



-- LOCATIONS


type Location
    = Gothenburg
    | Umea
    | Are
    | Ostersund
    | Trollhattan
    | Stockholm
    | Revsund
    | Kinnekulle
    | Lund
    | Helsingborg


type alias LocationInformation =
    { name : String
    , country : Country
    , coordinates : ( Float, Float )
    }


locationLocalName : Location -> Maybe String
locationLocalName location =
    case location of
        Gothenburg ->
            Just "Göteborg"

        Umea ->
            Just "Umeå"

        Are ->
            Just "Åre"

        Ostersund ->
            Just "Östersund"

        Trollhattan ->
            Just "Trollhättan"

        _ ->
            Nothing


{-| TODO: try to pull coordinates from an internet source automatically.
Does latlong have an API?
-}
locationInformation : Location -> LocationInformation
locationInformation location =
    case location of
        Gothenburg ->
            { name = "Gothenburg"
            , country = Sweden
            , coordinates = ( 11.97, 57.72 )
            }

        Umea ->
            { name = "Umea"
            , country = Sweden
            , coordinates = ( 20.26, 63.83 )
            }

        Are ->
            { name = "Are"
            , country = Sweden
            , coordinates = ( 13.08, 63.4 )
            }

        Ostersund ->
            { name = "Ostersund"
            , country = Sweden
            , coordinates = ( 14.64, 63.18 )
            }

        Trollhattan ->
            { name = "Trollhattan"
            , country = Sweden
            , coordinates = ( 12.27, 58.28 )
            }

        Stockholm ->
            { name = "Stockholm"
            , country = Sweden
            , coordinates = ( 18.07, 59.33 )
            }

        Revsund ->
            { name = "Revsund"
            , country = Sweden
            , coordinates = ( 15.13, 62.9 )
            }

        Kinnekulle ->
            { name = "Kinnekulle"
            , country = Sweden
            , coordinates = ( 13.38, 58.58 )
            }

        Lund ->
            { name = "Lund"
            , country = Sweden
            , coordinates = ( 13.19, 55.71 )
            }

        Helsingborg ->
            { name = "Helsingborg"
            , country = Sweden
            , coordinates = ( 12.69, 56.05 )
            }


locale : Image -> String
locale image =
    let
        info =
            locationInformation image.location

        countryString =
            countryName info.country
    in
    case countryLocalName info.country of
        Just localCountry ->
            case locationLocalName image.location of
                Just localLocation ->
                    localLocation ++ ", " ++ localCountry ++ " (" ++ info.name ++ ", " ++ countryString ++ ")"

                Nothing ->
                    info.name ++ ", " ++ countryString ++ " (" ++ localCountry ++ ")"

        Nothing ->
            case locationLocalName image.location of
                Just localLocation ->
                    localLocation ++ " (" ++ info.name ++ "), " ++ countryString

                Nothing ->
                    info.name ++ ", " ++ countryString



-- TRIPS


type Trip
    = SingaporeJapan2007
    | Europe2012
    | EuropeRussia2014
    | VietnamThailand2015
    | Summer2016
    | Summer2017
    | Winter20172018
    | Summer2018


type alias TripInformation =
    { name : String
    , description : String
    , locations : List Location
    , dates : List ( Year, Month )
    }


tripInformation : Trip -> TripInformation
tripInformation trip =
    case trip of
        SingaporeJapan2007 ->
            { name = "J07"
            , description = "Singapore/Japan 2007"
            , locations = [] --[ Melbourne, Singapore, Osaka, Himeji, Osaka, Hiroshima, Koyasan, Osaka, Tokyo, Kyoto, Osaka ]
            , dates = [ ( 2007, Dec ) ]
            }

        _ ->
            { name = "E12"
            , description = "Europe 2012"
            , locations = [] --[ Melbourne, Singapore, Frankfurt, Karlsruhe, Heidelberg, Munich, Vienna, Budapest, Prague, Riga, Copenhagen, Dronningmolle, Helsingor, Helsingborg, Copenhagen, Berlin, Paris, Singapore ]
            , dates = [ ( 2012, Jun ), ( 2012, Jul ) ]
            }



-- TIME KEEPING


type alias Year =
    Int


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


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



-- MANIFEST


manifest : List Image
manifest =
    [ Image "20151115_103822.jpg" ( 2015, Nov ) Gothenburg Nothing 1.778 "Slottsskogen has a few ducks."
    , Image "DSC_1758.jpg" ( 2018, Jan ) Gothenburg Nothing 1.778 ""
    , Image "20180102_141636.jpg" ( 2018, Jan ) Revsund Nothing 0.75 ""
    ]

module Manifest exposing (Image, Manifest)

-- Would be good to autogenerate this file


type alias Image =
    { file : String
    , thumb : String
    , date : ( Year, Month )
    , location : Location
    , trip : Maybe Trip
    , aspectRatio : Float
    , description : String
    }


imagePath : Image -> String
imagePath image =
    let
        ( year, month ) =
            image.date
    in
    String.join "/"
        [ "gallery"
        , String.fromInt year
        , monthToDirectory month
        , countryToDirectory image.location.country
        , String.replace " " "_" image.location.name
        ]



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



-- Country is listed here as well as Image. Can we drop the one in image (or here for that matter? Perhaps image is better)?


type alias LocationInformation =
    { name : String
    , localName : String
    , country : Country
    , coordinates : ( Float, Float )
    }


locationInformation : Location -> LocationInformation
locationInformation location =
    case location of
        Gothenburg ->
            { name = "Gothenburg"
            , localName = "Göteborg"
            , country = Sweden
            , coordinates = ( 11.97, 57.72 )
            }

        _ ->
            { name = "Tokyo"
            , localName = "東京"
            , country = Japan
            , coordinates = ( 139.73, 35.71 )
            }



-- TODO: locale can be generated too rather than storing it.
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
            , locations = [ Melbourne, Singapore, Osaka, Himeji, Osaka, Hiroshima, Koyasan, Osaka, Tokyo, Kyoto, Osaka ]
            , dates = [ ( 2007, Dec ) ]
            }

        _ ->
            { name = "E12"
            , description = "Europe 2012"
            , locations = [ Melbourne, Singapore, Frankfurt, Karlsruhe, Heidelberg, Munich, Vienna, Budapest, Prague, Riga, Copenhagen, Dronningmolle, Helsingor, Helsingborg, Copenhagen, Berlin, Paris, Singapore ]
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
    monthInt |> String.fromInt |> String.padLeft 2 '0'

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
    | FaeroeIslands
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

        FaeroeIslands ->
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

        FaeroeIslands ->
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

        FaeroeIslands ->
            "Faeroe_Is"

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
    = Amsterdam
    | Are
    | Athens
    | Auschwitz
    | Ayutthaya
    | Balestrand
    | Bangkok
    | Bergen
    | Berlin
    | Bodo
    | Bordoy
    | Bratislava
    | Budapest
    | Chernobyl
    | Copenhagen
    | Crete
    | Doha
    | Dronningmolle
    | Exeter
    | Eysturoy
    | Fjaerland
    | Flam
    | Frankfurt
    | Freiburg
    | Geysir
    | Gothenburg
    | HaLongBay
    | Hanoi
    | Heidelberg
    | Helsingborg
    | Helsingor
    | Helsinki
    | Hestur
    | Himeji
    | Hiroshima
    | HoChiMinhCity
    | HongKongCity
    | Jokulsarlon
    | Kanchanaburi
    | Karlsruhe
    | Katowice
    | Kiev
    | Kinnekulle
    | KoSamui
    | KoTao
    | Koyasan
    | Krakow
    | Kristiansund
    | Kyoto
    | London
    | Lund
    | Melbourne
    | Munich
    | Osaka
    | Oslo
    | Ostersund
    | Paris
    | Petergof
    | Prague
    | Pripyat
    | Pushkin
    | Revsund
    | Reykjavik
    | Riga
    | Rorvik
    | Roskilde
    | SaintPetersburg
    | SingaporeCity
    | Skaftafell
    | Skogarfoss
    | Stockholm
    | Streymoy
    | Svolvaer
    | Sydney
    | Tallinn
    | Thingvellir
    | Tokyo
    | Torshavn
    | Trollhattan
    | Tromso
    | Trondheim
    | Trysil
    | Umea
    | Vagar
    | Vidoy
    | Vienna
    | Vik
    | Warsaw
    | Yerevan


type alias LocationInformation =
    { name : String
    , country : Country
    , coordinates : ( Float, Float )
    }


locationLocalName : Location -> Maybe String
locationLocalName location =
    case location of
        Are ->
            Just "Åre"

        Athens ->
            Just "Αθήνα"

        Auschwitz ->
            Just "Oświęcim"

        Ayutthaya ->
            Just "อยุธยา"

        Bangkok ->
            Just "กรุงเทพมหานคร"

        Bodo ->
            Just "Bodø"

        Bordoy ->
            Just "Borðoy"

        Chernobyl ->
            Just "Чорнобиль"

        Copenhagen ->
            Just "København"

        Crete ->
            Just "Κρήτη"

        Doha ->
            Just "الدوحة"

        Dronningmolle ->
            Just "Dronningmølle"

        Fjaerland ->
            Just "Fjærland"

        Flam ->
            Just "Flåm"

        Gothenburg ->
            Just "Göteborg"

        HaLongBay ->
            Just "Vịnh Hạ Long"

        Hanoi ->
            Just "Hà Nội"

        Helsingor ->
            Just "Helsingør"

        Himeji ->
            Just "姫路市"

        Hiroshima ->
            Just "広島市"

        HoChiMinhCity ->
            Just "Thành phố Hồ Chí Minh"

        HongKongCity ->
            Just "香港"

        Jokulsarlon ->
            Just "Jökulsárlón"

        Kanchanaburi ->
            Just "กาญจนบุรี"

        Kiev ->
            Just "Київ"

        KoSamui ->
            Just "เกาะสมุย"

        KoTao ->
            Just "เกาะเต่า"

        Koyasan ->
            Just "高野山"

        Krakow ->
            Just "Kraków"

        Kyoto ->
            Just "京都市"

        Osaka ->
            Just "大阪市"

        Ostersund ->
            Just "Östersund"

        Petergof ->
            Just "Петергоф"

        Prague ->
            Just "Praha"

        Pripyat ->
            Just "При́п'ять"

        Pushkin ->
            Just "Пушкин"

        Riga ->
            Just "Rīga"

        Rorvik ->
            Just "Rørvik"

        SaintPetersburg ->
            Just "Санкт-Петербу́рг"

        Skogarfoss ->
            Just "Skógarfoss"

        Svolvaer ->
            Just "Svolvær"

        Thingvellir ->
            Just "Þingvellir"

        Tokyo ->
            Just "東京"

        Torshavn ->
            Just "Tórshavn"

        Trollhattan ->
            Just "Trollhättan"

        Tromso ->
            Just "Tromsø"

        Umea ->
            Just "Umeå"

        Vagar ->
            Just "Vágar"

        Vidoy ->
            Just "Viðoy"

        Vienna ->
            Just "Wien"

        Vik ->
            Just "Vík í Mýrdal"

        Warsaw ->
            Just "Warszawa"

        Yerevan ->
            Just "Երևան"

        _ ->
            Nothing


{-| TODO: try to pull coordinates from an internet source automatically.
Does latlong have an API?
-}
locationInformation : Location -> LocationInformation
locationInformation location =
    case location of
        Amsterdam ->
            { name = "Amsterdam"
            , country = Netherlands
            , coordinates = ( 4.9, 52.37 )
            }

        Are ->
            { name = "Are"
            , country = Sweden
            , coordinates = ( 13.08, 63.4 )
            }

        Athens ->
            { name = "Athens"
            , country = Greece
            , coordinates = ( 23.73, 37.98 )
            }

        Auschwitz ->
            { name = "Auschwitz"
            , country = Poland
            , coordinates = ( 19.21, 50.03 )
            }

        Ayutthaya ->
            { name = "Ayutthaya"
            , country = Thailand
            , coordinates = ( 100.37, 14.46 )
            }

        Balestrand ->
            { name = "Balestrand"
            , country = Norway
            , coordinates = ( 6.53, 61.2 )
            }

        Bangkok ->
            { name = "Bangkok"
            , country = Thailand
            , coordinates = ( 100.5, 13.76 )
            }

        Bergen ->
            { name = "Bergen"
            , country = Norway
            , coordinates = ( 5.32, 60.39 )
            }

        Berlin ->
            { name = "Berlin"
            , country = Germany
            , coordinates = ( 13.41, 52.52 )
            }

        Bodo ->
            { name = "Bodo"
            , country = Norway
            , coordinates = ( 14.41, 67.28 )
            }

        Bordoy ->
            { name = "Bordoy"
            , country = FaeroeIslands
            , coordinates = ( -6.55, 62.28 )
            }

        Bratislava ->
            { name = "Bratislava"
            , country = Slovakia
            , coordinates = ( 17.11, 48.15 )
            }

        Budapest ->
            { name = "Budapest"
            , country = Hungary
            , coordinates = ( 19.04, 47.5 )
            }

        Chernobyl ->
            { name = "Chernobyl"
            , country = Ukraine
            , coordinates = ( 30.22, 51.28 )
            }

        Copenhagen ->
            { name = "Copenhagen"
            , country = Denmark
            , coordinates = ( 12.57, 55.68 )
            }

        Crete ->
            { name = "Crete"
            , country = Greece
            , coordinates = ( 24.81, 35.24 )
            }

        Doha ->
            { name = "Doha"
            , country = Qatar
            , coordinates = ( 51.53, 25.29 )
            }

        Dronningmolle ->
            { name = "Dronningmolle"
            , country = Denmark
            , coordinates = ( 12.39, 56.1 )
            }

        Exeter ->
            { name = "Exeter"
            , country = UnitedKingdom
            , coordinates = ( -3.53, 50.72 )
            }

        Eysturoy ->
            { name = "Eysturoy"
            , country = FaeroeIslands
            , coordinates = ( -6.88, 62.22 )
            }

        Fjaerland ->
            { name = "Fjaerland"
            , country = Norway
            , coordinates = ( 6.74, 61.4 )
            }

        Flam ->
            { name = "Flam"
            , country = Norway
            , coordinates = ( 7.11, 60.86 )
            }

        Frankfurt ->
            { name = "Frankfurt"
            , country = Germany
            , coordinates = ( 8.68, 50.11 )
            }

        Freiburg ->
            { name = "Freiburg"
            , country = Germany
            , coordinates = ( 7.84, 48.0 )
            }

        Geysir ->
            { name = "Geysir"
            , country = Iceland
            , coordinates = ( -20.12, 64.33 )
            }

        Gothenburg ->
            { name = "Gothenburg"
            , country = Sweden
            , coordinates = ( 11.97, 57.72 )
            }

        HaLongBay ->
            { name = "Ha Long Bay"
            , country = Vietnam
            , coordinates = ( 107.18, 20.91 )
            }

        Hanoi ->
            { name = "Hanoi"
            , country = Vietnam
            , coordinates = ( 105.83, 21.03 )
            }

        Heidelberg ->
            { name = "Heidelberg"
            , country = Germany
            , coordinates = ( 8.67, 49.4 )
            }

        Helsingborg ->
            { name = "Helsingborg"
            , country = Sweden
            , coordinates = ( 12.69, 56.05 )
            }

        Helsingor ->
            { name = "Helsingor"
            , country = Denmark
            , coordinates = ( 12.59, 56.03 )
            }

        Helsinki ->
            { name = "Helsinki"
            , country = Finland
            , coordinates = ( 24.94, 60.17 )
            }

        Hestur ->
            { name = "Hestur"
            , country = FaeroeIslands
            , coordinates = ( -6.9, 62 )
            }

        Himeji ->
            { name = "Himeji"
            , country = Japan
            , coordinates = ( 134.69, 34.82 )
            }

        Hiroshima ->
            { name = "Hiroshima"
            , country = Japan
            , coordinates = ( 132.46, 34.39 )
            }

        HoChiMinhCity ->
            { name = "Ho Chi Minh City"
            , country = Vietnam
            , coordinates = ( 106.63, 10.82 )
            }

        HongKongCity ->
            { name = "Hong Kong"
            , country = HongKong
            , coordinates = ( 114.18, 22.31 )
            }

        Jokulsarlon ->
            { name = "Jokulsarlon"
            , country = Iceland
            , coordinates = ( -16.23, 64.08 )
            }

        Kanchanaburi ->
            { name = "Kanchanaburi"
            , country = Thailand
            , coordinates = ( 99.53, 14.02 )
            }

        Karlsruhe ->
            { name = "Karlsruhe"
            , country = Germany
            , coordinates = ( 8.4, 49 )
            }

        Katowice ->
            { name = "Katowice"
            , country = Poland
            , coordinates = ( 19.02, 50.26 )
            }

        Kiev ->
            { name = "Kiev"
            , country = Ukraine
            , coordinates = ( 30.52, 50.45 )
            }

        Kinnekulle ->
            { name = "Kinnekulle"
            , country = Sweden
            , coordinates = ( 13.38, 58.58 )
            }

        KoSamui ->
            { name = "Ko Samui"
            , country = Thailand
            , coordinates = ( 100.01, 9.51 )
            }

        KoTao ->
            { name = "Ko Tao"
            , country = Thailand
            , coordinates = ( 99.84, 10.1 )
            }

        Koyasan ->
            { name = "Koyasan"
            , country = Japan
            , coordinates = ( 135.59, 34.2 )
            }

        Krakow ->
            { name = "Krakow"
            , country = Poland
            , coordinates = ( 19.95, 50.07 )
            }

        Kristiansund ->
            { name = "Kristiansund"
            , country = Norway
            , coordinates = ( 7.73, 63.11 )
            }

        Kyoto ->
            { name = "Kyoto"
            , country = Japan
            , coordinates = ( 135.77, 35.01 )
            }

        London ->
            { name = "London"
            , country = UnitedKingdom
            , coordinates = ( -0.13, 51.51 )
            }

        Lund ->
            { name = "Lund"
            , country = Sweden
            , coordinates = ( 13.19, 55.71 )
            }

        Melbourne ->
            { name = "Melbourne"
            , country = Australia
            , coordinates = ( 144.96, -37.81 )
            }

        Munich ->
            { name = "Munich"
            , country = Germany
            , coordinates = ( 11.58, 48.14 )
            }

        Osaka ->
            { name = "Osaka"
            , country = Japan
            , coordinates = ( 135.5, 34.7 )
            }

        Oslo ->
            { name = "Oslo"
            , country = Norway
            , coordinates = ( 10.75, 59.91 )
            }

        Ostersund ->
            { name = "Ostersund"
            , country = Sweden
            , coordinates = ( 14.64, 63.18 )
            }

        Paris ->
            { name = "Paris"
            , country = France
            , coordinates = ( 2.35, 48.86 )
            }

        Petergof ->
            { name = "Petergof"
            , country = Russia
            , coordinates = ( 29.89, 59.89 )
            }

        Prague ->
            { name = "Prague"
            , country = CzechRepublic
            , coordinates = ( 14.44, 50.08 )
            }

        Pripyat ->
            { name = "Pripyat"
            , country = Ukraine
            , coordinates = ( 30.05, 51.4 )
            }

        Pushkin ->
            { name = "Pushkin"
            , country = Russia
            , coordinates = ( 30.41, 59.72 )
            }

        Revsund ->
            { name = "Revsund"
            , country = Sweden
            , coordinates = ( 15.13, 62.9 )
            }

        Reykjavik ->
            { name = "Reykjavik"
            , country = Iceland
            , coordinates = ( -21.82, 64.13 )
            }

        Riga ->
            { name = "Riga"
            , country = Latvia
            , coordinates = ( 24.11, 56.95 )
            }

        Rorvik ->
            { name = "Rorvik"
            , country = Norway
            , coordinates = ( 11.23, 64.86 )
            }

        Roskilde ->
            { name = "Roskilde"
            , country = Denmark
            , coordinates = ( 12.09, 55.64 )
            }

        SaintPetersburg ->
            { name = "Saint Petersburg"
            , country = Russia
            , coordinates = ( 30.36, 59.93 )
            }

        SingaporeCity ->
            { name = "Singapore"
            , country = Singapore
            , coordinates = ( 103.87, 1.36 )
            }

        Skaftafell ->
            { name = "Skaftafell"
            , country = Iceland
            , coordinates = ( -17, 64.02 )
            }

        Skogarfoss ->
            { name = "Skogarfoss"
            , country = Iceland
            , coordinates = ( -19.51, 63.53 )
            }

        Stockholm ->
            { name = "Stockholm"
            , country = Sweden
            , coordinates = ( 18.07, 59.33 )
            }

        Streymoy ->
            { name = "Streymoy"
            , country = FaeroeIslands
            , coordinates = ( -7.08, 62.2 )
            }

        Svolvaer ->
            { name = "Svolvaer"
            , country = Norway
            , coordinates = ( 14.57, 68.23 )
            }

        Sydney ->
            { name = "Sydney"
            , country = Australia
            , coordinates = ( 151.21, -33.87 )
            }

        Tallinn ->
            { name = "Tallinn"
            , country = Estonia
            , coordinates = ( 24.75, 59.44 )
            }

        Thingvellir ->
            { name = "Thingvellir"
            , country = Iceland
            , coordinates = ( -21.08, 64.27 )
            }

        Tokyo ->
            { name = "Tokyo"
            , country = Japan
            , coordinates = ( 139.73, 35.71 )
            }

        Torshavn ->
            { name = "Torshavn"
            , country = FaeroeIslands
            , coordinates = ( -6.79, 62.0 )
            }

        Trollhattan ->
            { name = "Trollhattan"
            , country = Sweden
            , coordinates = ( 12.27, 58.28 )
            }

        Tromso ->
            { name = "Tromso"
            , country = Norway
            , coordinates = ( 19.96, 69.65 )
            }

        Trondheim ->
            { name = "Trondheim"
            , country = Norway
            , coordinates = ( 10.4, 63.43 )
            }

        Trysil ->
            { name = "Trysil"
            , country = Norway
            , coordinates = ( 12.29, 61.28 )
            }

        Umea ->
            { name = "Umea"
            , country = Sweden
            , coordinates = ( 20.26, 63.83 )
            }

        Vagar ->
            { name = "Vagar"
            , country = FaeroeIslands
            , coordinates = ( -7.22, 62.1 )
            }

        Vidoy ->
            { name = "Vidoy"
            , country = FaeroeIslands
            , coordinates = ( -6.5, 62.33 )
            }

        Vienna ->
            { name = "Vienna"
            , country = Austria
            , coordinates = ( 16.38, 48.21 )
            }

        Vik ->
            { name = "Vik"
            , country = Iceland
            , coordinates = ( -19, 63.42 )
            }

        Warsaw ->
            { name = "Warsaw"
            , country = Poland
            , coordinates = ( 21.01, 52.23 )
            }

        Yerevan ->
            { name = "Yerevan"
            , country = Armenia
            , coordinates = ( 44.5, 40.18 )
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
    [ Image "IMG_20160310_131012.jpg" ( 2016, Mar ) Trysil Nothing 2.26 ""
    , Image "IMG_1286.jpg" ( 2016, Mar ) Trysil Nothing 0.666 "If you look closely you can see my helmet has already paid for itself."
    , Image "IMG_20160310_130924.jpg" ( 2016, Mar ) Trysil Nothing 1.537 "Epic black slope coming down from barely visible conditions to crystal clear. Soon after this point I fell over and slid for a good 20 meters before stopping."
    , Image "IMG_20160308_111338.jpg" ( 2016, Mar ) Trysil Nothing 2.094 ""
    , Image "IMG_20160308_073926.jpg" ( 2016, Mar ) Trysil Nothing 1.684 ""
    , Image "IMG_1290.jpg" ( 2016, Mar ) Trysil Nothing 1.778 ""
    , Image "IMG_20160325_180815.jpg" ( 2016, Mar ) Gothenburg Nothing 1.805 ""
    , Image "IMG_20161228_151154.jpg" ( 2016, Dec ) Berlin Nothing 0.562 ""
    , Image "IMG_20161228_131557.jpg" ( 2016, Dec ) Berlin Nothing 1.778 ""
    , Image "IMG_20161230_133731.jpg" ( 2016, Dec ) Berlin Nothing 0.562 ""
    , Image "IMG_20161229_150018.jpg" ( 2016, Dec ) Berlin Nothing 0.497 ""
    , Image "IMG_20161229_144204.jpg" ( 2016, Dec ) Berlin Nothing 1.354 ""
    , Image "IMG_20161228_154330.jpg" ( 2016, Dec ) Berlin Nothing 0.413 ""
    , Image "IMG_20161228_131242.jpg" ( 2016, Dec ) Berlin Nothing 0.688 ""
    , Image "IMG_20161228_131222.jpg" ( 2016, Dec ) Berlin Nothing 1.778 ""
    , Image "IMG_20161228_131459.jpg" ( 2016, Dec ) Berlin Nothing 0.562 ""
    , Image "IMG_20161228_162556.jpg" ( 2016, Dec ) Berlin Nothing 2.242 ""
    , Image "IMG_20161228_161207.jpg" ( 2016, Dec ) Berlin Nothing 0.544 ""
    , Image "IMG_20161228_160803.jpg" ( 2016, Dec ) Berlin Nothing 0.585 ""
    , Image "IMG_20161229_150504.jpg" ( 2016, Dec ) Berlin Nothing 0.494 ""
    , Image "under15.jpg" ( 2016, Dec ) Gothenburg Nothing 1.499 ""
    , Image "under11.jpg" ( 2016, Dec ) Gothenburg Nothing 1.503 ""
    , Image "under6.jpg" ( 2016, Dec ) Gothenburg Nothing 1.499 ""
    , Image "under8.jpg" ( 2016, Dec ) Gothenburg Nothing 1.499 ""
    , Image "under4.jpg" ( 2016, Dec ) Gothenburg Nothing 1.499 ""
    , Image "IMG_20161209_191627.jpg" ( 2016, Dec ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20161209_183316.jpg" ( 2016, Dec ) Gothenburg Nothing 1.233 ""
    , Image "under3.jpg" ( 2016, Dec ) Gothenburg Nothing 1.503 ""
    , Image "IMG_20160611_160646.jpg" ( 2016, Jun ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20160624_134758.jpg" ( 2016, Jun ) Gothenburg Nothing 1.717 "Swedes do love their frog dance."
    , Image "IMG_20160611_170601.jpg" ( 2016, Jun ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20160611_162144.jpg" ( 2016, Jun ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20160611_161721.jpg" ( 2016, Jun ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20160611_161627.jpg" ( 2016, Jun ) Gothenburg Nothing 2.11 ""
    , Image "IMG_20160611_164852.jpg" ( 2016, Jun ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20160709_165115.jpg" ( 2016, Jul ) Yerevan Nothing 1.878 ""
    , Image "DSC08568.jpg" ( 2016, Jul ) Yerevan Nothing 1.929 "Photo by Felix Mackenroth."
    , Image "DSC08562.jpg" ( 2016, Jul ) Yerevan Nothing 3.448 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_142806.jpg" ( 2016, Jul ) Yerevan Nothing 0.6 "I'm only interested in the fanciest of drinks. It took four peolpe to make this."
    , Image "IMG_20160710_160726.jpg" ( 2016, Jul ) Yerevan Nothing 1.531 ""
    , Image "IMG_20160709_123331.jpg" ( 2016, Jul ) Yerevan Nothing 2.817 ""
    , Image "IMG_20160713_194242.jpg" ( 2016, Jul ) Yerevan Nothing 1.689 ""
    , Image "DSC08516.jpg" ( 2016, Jul ) Yerevan Nothing 1.267 "Photo by Felix Mackenroth."
    , Image "IMG_20160713_193119.jpg" ( 2016, Jul ) Yerevan Nothing 1.778 ""
    , Image "DSC08842.jpg" ( 2016, Jul ) Yerevan Nothing 1.349 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_210227.jpg" ( 2016, Jul ) Yerevan Nothing 0.778 ""
    , Image "IMG_20160709_132935.jpg" ( 2016, Jul ) Yerevan Nothing 2.101 ""
    , Image "IMG_20160709_122416.jpg" ( 2016, Jul ) Yerevan Nothing 1.117 ""
    , Image "IMG_20160709_201004.jpg" ( 2016, Jul ) Yerevan Nothing 1.37 ""
    , Image "IMG_20160709_121934.jpg" ( 2016, Jul ) Yerevan Nothing 1.562 ""
    , Image "IMG_20160710_180837.jpg" ( 2016, Jul ) Yerevan Nothing 1.709 ""
    , Image "IMG_20160712_215814.jpg" ( 2016, Jul ) Yerevan Nothing 0.585 ""
    , Image "DSC08615.jpg" ( 2016, Jul ) Yerevan Nothing 1.813 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_162137.jpg" ( 2016, Jul ) Yerevan Nothing 1.778 ""
    , Image "DSC08778_fixed.jpg" ( 2016, Jul ) Yerevan Nothing 1.468 "Photo by Felix Mackenroth."
    , Image "DSC08810.jpg" ( 2016, Jul ) Yerevan Nothing 1.503 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_134526.jpg" ( 2016, Jul ) Yerevan Nothing 1.988 ""
    , Image "IMG_20160710_143701.jpg" ( 2016, Jul ) Yerevan Nothing 1.916 ""
    , Image "DSC08783_stitch.jpg" ( 2016, Jul ) Yerevan Nothing 3.984 "Photo by Felix Mackenroth."
    , Image "DSC08755.jpg" ( 2016, Jul ) Yerevan Nothing 1.186 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_131933.jpg" ( 2016, Jul ) Yerevan Nothing 1.778 ""
    , Image "DSC08746.jpg" ( 2016, Jul ) Yerevan Nothing 1.404 "Photo by Felix Mackenroth."
    , Image "IMG_20160713_193623.jpg" ( 2016, Jul ) Yerevan Nothing 1.768 ""
    , Image "DSC08545.jpg" ( 2016, Jul ) Yerevan Nothing 1.732 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_161515.jpg" ( 2016, Jul ) Yerevan Nothing 1.684 ""
    , Image "DSC08799.jpg" ( 2016, Jul ) Yerevan Nothing 1.609 "Photo by Felix Mackenroth."
    , Image "IMG_20160710_160532.jpg" ( 2016, Jul ) Yerevan Nothing 0.562 ""
    , Image "IMG_20160709_131641.jpg" ( 2016, Jul ) Yerevan Nothing 1.481 ""
    , Image "IMG_20160713_211827.jpg" ( 2016, Jul ) Yerevan Nothing 2.169 ""
    , Image "IMG_20160710_160306.jpg" ( 2016, Jul ) Yerevan Nothing 0.562 ""
    , Image "DSC08541.jpg" ( 2016, Jul ) Yerevan Nothing 0.807 "Photo by Felix Mackenroth."
    , Image "IMG_20160713_194114.jpg" ( 2016, Jul ) Yerevan Nothing 1.912 ""
    , Image "IMG_20160710_141819.jpg" ( 2016, Jul ) Yerevan Nothing 1.459 ""
    , Image "IMG_20160709_132308.jpg" ( 2016, Jul ) Yerevan Nothing 1.059 ""
    , Image "IMG_20160713_194255.jpg" ( 2016, Jul ) Yerevan Nothing 1.901 ""
    , Image "DSC08535.jpg" ( 2016, Jul ) Yerevan Nothing 2.06 "Photo by Felix Mackenroth."
    , Image "IMG_20160710_160554.jpg" ( 2016, Jul ) Yerevan Nothing 1.665 ""
    , Image "IMG_20160710_154729.jpg" ( 2016, Jul ) Yerevan Nothing 0.562 ""
    , Image "DSC08772.jpg" ( 2016, Jul ) Yerevan Nothing 1.303 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_121130.jpg" ( 2016, Jul ) Yerevan Nothing 1.641 ""
    , Image "DSC08736.jpg" ( 2016, Jul ) Yerevan Nothing 1.503 "Photo by Felix Mackenroth."
    , Image "IMG_20160709_120827.jpg" ( 2016, Jul ) Yerevan Nothing 1.778 ""
    , Image "IMG_20160709_124008.jpg" ( 2016, Jul ) Yerevan Nothing 1.778 ""
    , Image "IMG_20160709_122203.jpg" ( 2016, Jul ) Yerevan Nothing 1.778 ""
    , Image "IMG_20160730_084850.jpg" ( 2016, Jul ) Helsinki Nothing 3.306 "A decent panorama of the main square of Helsinki. That is of course if you ignore the time travelling police car."
    , Image "IMG_20160730_084534.jpg" ( 2016, Jul ) Helsinki Nothing 0.641 ""
    , Image "IMG_20160730_090927.jpg" ( 2016, Jul ) Helsinki Nothing 0.665 ""
    , Image "IMG_20160730_105126.jpg" ( 2016, Jul ) Helsinki Nothing 2.008 ""
    , Image "IMG_20160730_080917.jpg" ( 2016, Jul ) Helsinki Nothing 0.691 "This is a tiny little contemplative chapel."
    , Image "IMG_20160730_085859.jpg" ( 2016, Jul ) Helsinki Nothing 1.778 ""
    , Image "IMG_20160730_090012.jpg" ( 2016, Jul ) Helsinki Nothing 1.709 ""
    , Image "IMG_20160729_170438.jpg" ( 2016, Jul ) Tallinn Nothing 2.26 ""
    , Image "IMG_20160728_174329.jpg" ( 2016, Jul ) Tallinn Nothing 1.778 "The Russians built this place for the water sports portion when they hosted the olympics. It's gigantic and weird and the Estonians have left it to rot for good reason."
    , Image "IMG_20160728_230159.jpg" ( 2016, Jul ) Tallinn Nothing 0.61 ""
    , Image "IMG_20160729_163017.jpg" ( 2016, Jul ) Tallinn Nothing 1.778 ""
    , Image "IMG_20160728_171621.jpg" ( 2016, Jul ) Tallinn Nothing 1.778 "Part of the old gaol."
    , Image "IMG_20160728_180110.jpg" ( 2016, Jul ) Tallinn Nothing 1.391 ""
    , Image "IMG_20160729_173819.jpg" ( 2016, Jul ) Tallinn Nothing 1.859 ""
    , Image "IMG_20160728_175520.jpg" ( 2016, Jul ) Tallinn Nothing 0.562 ""
    , Image "IMG_20160729_152355.jpg" ( 2016, Jul ) Tallinn Nothing 0.531 "Nicklas looking particularly tiny."
    , Image "IMG_20160728_230555.jpg" ( 2016, Jul ) Tallinn Nothing 1.172 "This fredom from the soviets monument looks pretty soviet. The Estonians hate it and it's also costing them a fortune because it was poorly constructed and it keeps falling apart. They're suing the guys who built it whos defence is 'Changing weather conditions were not part of the design plan'."
    , Image "IMG_20160729_170850.jpg" ( 2016, Jul ) Tallinn Nothing 1.898 ""
    , Image "IMG_20160728_113045.jpg" ( 2016, Jul ) Tallinn Nothing 2.759 ""
    , Image "IMG_20160728_130245.jpg" ( 2016, Jul ) Tallinn Nothing 0.464 "If you manage to change the flag on this flagpole to your own, you basically own Estonia. Good luck."
    , Image "IMG_20160729_140328.jpg" ( 2016, Jul ) Tallinn Nothing 0.728 ""
    , Image "IMG_20160728_130509.jpg" ( 2016, Jul ) Tallinn Nothing 1.507 ""
    , Image "IMG_20160728_195913.jpg" ( 2016, Jul ) Tallinn Nothing 2.0 ""
    , Image "IMG_20160729_170012.jpg" ( 2016, Jul ) Tallinn Nothing 1.916 ""
    , Image "IMG_20160728_113608.jpg" ( 2016, Jul ) Tallinn Nothing 1.736 ""
    , Image "IMG_20160729_135447.jpg" ( 2016, Jul ) Tallinn Nothing 1.778 "A Swede protecting Tallinn from the Swedes..."
    , Image "IMG_20160728_231102.jpg" ( 2016, Jul ) Tallinn Nothing 0.592 ""
    , Image "IMG_20160729_170244.jpg" ( 2016, Jul ) Tallinn Nothing 2.567 ""
    , Image "IMG_20160729_122511.jpg" ( 2016, Jul ) Tallinn Nothing 1.778 "Inside a tunnel underneath one of the cities bastions."
    , Image "IMG_20160728_113055.jpg" ( 2016, Jul ) Tallinn Nothing 1.778 "This little guy loved sitting here with 100s of tourists taking his photo."
    , Image "IMG_20160729_171738.jpg" ( 2016, Jul ) Tallinn Nothing 0.562 ""
    , Image "IMG_20160108_233039.jpg" ( 2016, Jan ) Gothenburg Nothing 1.416 ""
    , Image "IMG_20160120_145800.jpg" ( 2016, Jan ) Gothenburg Nothing 1.59 ""
    , Image "IMG_20160123_122130_20160131_122906.jpg" ( 2016, Jan ) Gothenburg Nothing 1.778 "Kids call this building The Lipstick. The boat is now a restaurant because it's actually too tall. The Swedes didn't learn from their mistakes this time."
    , Image "IMG_20160108_232659.jpg" ( 2016, Jan ) Gothenburg Nothing 1.778 ""
    , Image "IMG_20160220_124621.jpg" ( 2016, Feb ) Amsterdam Nothing 1.908 ""
    , Image "IMG_20160918_010221.jpg" ( 2016, Sep ) Crete Nothing 1.85 ""
    , Image "IMG_20160918_120741.jpg" ( 2016, Sep ) Crete Nothing 1.695 ""
    , Image "IMG_20160919_091510.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160919_090635.jpg" ( 2016, Sep ) Crete Nothing 1.567 ""
    , Image "IMG_20160918_191632.jpg" ( 2016, Sep ) Crete Nothing 1.94 ""
    , Image "IMG_20160914_190527.jpg" ( 2016, Sep ) Crete Nothing 1.729 ""
    , Image "IMG_20160918_185602.jpg" ( 2016, Sep ) Crete Nothing 1.799 ""
    , Image "IMG_20160916_123836.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160914_192308.jpg" ( 2016, Sep ) Crete Nothing 1.468 ""
    , Image "IMG_20160918_190208.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160918_140058.jpg" ( 2016, Sep ) Crete Nothing 2.367 ""
    , Image "IMG_20160919_092836.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160918_005928.jpg" ( 2016, Sep ) Crete Nothing 0.568 ""
    , Image "IMG_20160918_162639.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160918_120628.jpg" ( 2016, Sep ) Crete Nothing 1.441 ""
    , Image "IMG_20160919_162358.jpg" ( 2016, Sep ) Crete Nothing 2.151 ""
    , Image "IMG_20160916_155951.jpg" ( 2016, Sep ) Crete Nothing 1.984 ""
    , Image "IMG_20160916_123845.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160919_085652.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160915_112054.jpg" ( 2016, Sep ) Crete Nothing 1.742 ""
    , Image "IMG_20160916_160124.jpg" ( 2016, Sep ) Crete Nothing 1.616 ""
    , Image "IMG_20160919_090957.jpg" ( 2016, Sep ) Crete Nothing 2.26 ""
    , Image "IMG_20160916_123849.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160916_161753.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160914_191723.jpg" ( 2016, Sep ) Crete Nothing 0.75 ""
    , Image "IMG_20160914_093514.jpg" ( 2016, Sep ) Crete Nothing 1.333 ""
    , Image "IMG_20160919_092203.jpg" ( 2016, Sep ) Crete Nothing 2.232 ""
    , Image "IMG_20160919_085837.jpg" ( 2016, Sep ) Crete Nothing 2.442 ""
    , Image "IMG_20160919_085103.jpg" ( 2016, Sep ) Crete Nothing 3.106 ""
    , Image "IMG_20160919_090101.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160918_010027.jpg" ( 2016, Sep ) Crete Nothing 1.555 ""
    , Image "IMG_20160916_161712.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160919_092650.jpg" ( 2016, Sep ) Crete Nothing 2.024 ""
    , Image "IMG_20160915_130037.jpg" ( 2016, Sep ) Crete Nothing 1.944 ""
    , Image "IMG_20160918_180653.jpg" ( 2016, Sep ) Crete Nothing 2.128 ""
    , Image "IMG_20160916_160900.jpg" ( 2016, Sep ) Crete Nothing 1.778 ""
    , Image "IMG_20160916_160233.jpg" ( 2016, Sep ) Crete Nothing 1.99 ""
    , Image "IMG_20160920_121632.jpg" ( 2016, Sep ) Athens Nothing 1.931 ""
    , Image "IMG_20160921_110837.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_100930.jpg" ( 2016, Sep ) Athens Nothing 1.448 ""
    , Image "IMG_20160920_105018.jpg" ( 2016, Sep ) Athens Nothing 0.562 ""
    , Image "IMG_20160921_113911.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_123605.jpg" ( 2016, Sep ) Athens Nothing 1.257 ""
    , Image "IMG_20160920_141455.jpg" ( 2016, Sep ) Athens Nothing 1.137 ""
    , Image "IMG_20160921_121704.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160920_175603.jpg" ( 2016, Sep ) Athens Nothing 0.562 ""
    , Image "IMG_20160920_142035.jpg" ( 2016, Sep ) Athens Nothing 0.778 ""
    , Image "IMG_20160921_112120.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_152738.jpg" ( 2016, Sep ) Athens Nothing 2.342 ""
    , Image "IMG_20160921_135628.jpg" ( 2016, Sep ) Athens Nothing 1.745 ""
    , Image "IMG_20160921_151210.jpg" ( 2016, Sep ) Athens Nothing 1.548 ""
    , Image "IMG_20160920_105144.jpg" ( 2016, Sep ) Athens Nothing 0.387 ""
    , Image "IMG_20160921_115911.jpg" ( 2016, Sep ) Athens Nothing 1.72 ""
    , Image "IMG_20160920_100210.jpg" ( 2016, Sep ) Athens Nothing 1.008 ""
    , Image "IMG_20160921_162231.jpg" ( 2016, Sep ) Athens Nothing 1.434 ""
    , Image "IMG_20160921_120949.jpg" ( 2016, Sep ) Athens Nothing 2.0 ""
    , Image "IMG_20160920_072748.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_120657.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_172033.jpg" ( 2016, Sep ) Athens Nothing 1.609 ""
    , Image "IMG_20160921_123929.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_095135.jpg" ( 2016, Sep ) Athens Nothing 1.576 ""
    , Image "IMG_20160921_122701.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160920_111152.jpg" ( 2016, Sep ) Athens Nothing 0.638 ""
    , Image "IMG_20160921_122425.jpg" ( 2016, Sep ) Athens Nothing 0.562 ""
    , Image "IMG_20160920_160946.jpg" ( 2016, Sep ) Athens Nothing 0.562 ""
    , Image "IMG_20160921_121118.jpg" ( 2016, Sep ) Athens Nothing 1.625 ""
    , Image "IMG_20160921_172038.jpg" ( 2016, Sep ) Athens Nothing 2.307 ""
    , Image "IMG_20160921_164242.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_164324.jpg" ( 2016, Sep ) Athens Nothing 1.878 ""
    , Image "IMG_20160921_151938.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_134959.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160921_120737.jpg" ( 2016, Sep ) Athens Nothing 1.333 ""
    , Image "IMG_20160920_112211~01.jpg" ( 2016, Sep ) Athens Nothing 1.739 ""
    , Image "IMG_20160921_111620.jpg" ( 2016, Sep ) Athens Nothing 1.854 ""
    , Image "IMG_20160920_112407.jpg" ( 2016, Sep ) Athens Nothing 1.502 ""
    , Image "IMG_20160920_170629.jpg" ( 2016, Sep ) Athens Nothing 1.213 ""
    , Image "IMG_20160921_170034.jpg" ( 2016, Sep ) Athens Nothing 0.594 ""
    , Image "IMG_20160920_161216.jpg" ( 2016, Sep ) Athens Nothing 1.385 ""
    , Image "IMG_20160921_100027.jpg" ( 2016, Sep ) Athens Nothing 0.51 ""
    , Image "IMG_20160921_163845.jpg" ( 2016, Sep ) Athens Nothing 2.252 ""
    , Image "IMG_20160921_170937.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160920_164914.jpg" ( 2016, Sep ) Athens Nothing 1.778 ""
    , Image "IMG_20160923_140650.jpg" ( 2016, Sep ) Freiburg Nothing 1.682 ""
    , Image "IMG_20160922_185142.jpg" ( 2016, Sep ) Freiburg Nothing 1.778 ""
    , Image "IMG_20160923_112631.jpg" ( 2016, Sep ) Freiburg Nothing 1.778 ""
    , Image "IMG_20160923_105929.jpg" ( 2016, Sep ) Freiburg Nothing 0.562 ""
    , Image "IMG_20160923_140652.jpg" ( 2016, Sep ) Freiburg Nothing 0.75 ""
    , Image "IMG_20160923_112733.jpg" ( 2016, Sep ) Freiburg Nothing 0.562 ""
    , Image "IMG_20160923_140651.jpg" ( 2016, Sep ) Freiburg Nothing 1.333 ""
    , Image "IMG_20160923_113014.jpg" ( 2016, Sep ) Freiburg Nothing 1.678 ""
    , Image "IMG_20160923_110736.jpg" ( 2016, Sep ) Freiburg Nothing 0.675 ""
    , Image "IMG_20160923_111049.jpg" ( 2016, Sep ) Freiburg Nothing 1.778 ""
    , Image "IMG_20160923_140653.jpg" ( 2016, Sep ) Freiburg Nothing 1.0 ""
    , Image "IMG_20160923_112935.jpg" ( 2016, Sep ) Freiburg Nothing 0.562 ""
    , Image "IMG_20160924_110946.jpg" ( 2016, Sep ) Freiburg Nothing 1.778 ""
    , Image "IMG_20160924_110947.jpg" ( 2016, Sep ) Freiburg Nothing 0.75 ""
    , Image "IMG_20160923_140654.jpg" ( 2016, Sep ) Freiburg Nothing 1.333 ""
    , Image "IMG_20160901_143301.jpg" ( 2016, Sep ) Stockholm Nothing 0.788 ""
    , Image "20160409_135654.jpg" ( 2016, Apr ) Gothenburg Nothing 2.014 ""
    , Image "20160409_140613.jpg" ( 2016, Apr ) Gothenburg Nothing 1.778 "Bohus Fortress in Kungälv."
    , Image "20160409_143546.jpg" ( 2016, Apr ) Gothenburg Nothing 1.664 ""
    , Image "IMG_20161029_131116.jpg" ( 2016, Oct ) Gothenburg Nothing 1.6 ""
    , Image "IMG_20161029_122257.jpg" ( 2016, Oct ) Gothenburg Nothing 1.333 ""
    , Image "IMG_20160519_223459.jpg" ( 2016, May ) Gothenburg Nothing 1.792 "Just some crazy looking bunnies doing crazy stuff."
    , Image "IMG_20160510_210253.jpg" ( 2016, May ) Gothenburg Nothing 1.8 ""
    , Image "IMG_20160510_210024.jpg" ( 2016, May ) Gothenburg Nothing 1.203 ""
    , Image "PC072572.jpg" ( 2007, Dec ) Koyasan Nothing 1.333 ""
    , Image "PC082649.jpg" ( 2007, Dec ) Koyasan Nothing 0.933 ""
    , Image "PC082602.jpg" ( 2007, Dec ) Koyasan Nothing 0.618 ""
    , Image "PC082608.jpg" ( 2007, Dec ) Koyasan Nothing 1.333 ""
    , Image "PC072579.jpg" ( 2007, Dec ) Koyasan Nothing 1.06 ""
    , Image "PC072569.jpg" ( 2007, Dec ) Koyasan Nothing 1.333 ""
    , Image "PC082610.jpg" ( 2007, Dec ) Koyasan Nothing 1.341 ""
    , Image "PC082670.jpg" ( 2007, Dec ) Koyasan Nothing 1.333 ""
    , Image "PC082614.jpg" ( 2007, Dec ) Koyasan Nothing 0.581 ""
    , Image "PC082629.jpg" ( 2007, Dec ) Koyasan Nothing 0.705 ""
    , Image "PC082619.jpg" ( 2007, Dec ) Koyasan Nothing 2.74 ""
    , Image "PC082679.jpg" ( 2007, Dec ) Koyasan Nothing 1.786 ""
    , Image "PC082660.jpg" ( 2007, Dec ) Koyasan Nothing 1.225 ""
    , Image "PC052526.jpg" ( 2007, Dec ) Himeji Nothing 1.775 ""
    , Image "PC052504.jpg" ( 2007, Dec ) Himeji Nothing 1.572 ""
    , Image "PC052510.jpg" ( 2007, Dec ) Himeji Nothing 1.944 ""
    , Image "PC052459.jpg" ( 2007, Dec ) Himeji Nothing 1.333 ""
    , Image "PC052497.jpg" ( 2007, Dec ) Himeji Nothing 1.613 ""
    , Image "PC052465.jpg" ( 2007, Dec ) Himeji Nothing 1.642 ""
    , Image "PC052519.jpg" ( 2007, Dec ) Himeji Nothing 1.333 ""
    , Image "PC052531.jpg" ( 2007, Dec ) Himeji Nothing 1.961 ""
    , Image "PC132735.jpg" ( 2007, Dec ) Kyoto Nothing 0.856 ""
    , Image "PC132738.jpg" ( 2007, Dec ) Kyoto Nothing 1.333 ""
    , Image "PC132728.jpg" ( 2007, Dec ) Kyoto Nothing 1.594 ""
    , Image "PC132718.jpg" ( 2007, Dec ) Kyoto Nothing 1.333 ""
    , Image "PC112697.jpg" ( 2007, Dec ) Tokyo Nothing 0.613 ""
    , Image "PC102683.jpg" ( 2007, Dec ) Tokyo Nothing 1.333 ""
    , Image "PC112693.jpg" ( 2007, Dec ) Tokyo Nothing 1.267 ""
    , Image "PC102687.jpg" ( 2007, Dec ) Tokyo Nothing 1.805 ""
    , Image "PC022372.jpg" ( 2007, Dec ) SingaporeCity Nothing 1.384 ""
    , Image "PC022447.jpg" ( 2007, Dec ) SingaporeCity Nothing 2.137 ""
    , Image "PC012340.jpg" ( 2007, Dec ) SingaporeCity Nothing 2.497 ""
    , Image "PC022375.jpg" ( 2007, Dec ) SingaporeCity Nothing 1.862 ""
    , Image "PC022428.jpg" ( 2007, Dec ) SingaporeCity Nothing 3.044 ""
    , Image "PC022408.jpg" ( 2007, Dec ) SingaporeCity Nothing 0.885 ""
    , Image "PC012348.jpg" ( 2007, Dec ) SingaporeCity Nothing 1.564 ""
    , Image "PC022402.jpg" ( 2007, Dec ) SingaporeCity Nothing 2.286 ""
    , Image "PB302296.jpg" ( 2007, Dec ) SingaporeCity Nothing 1.693 ""
    , Image "PC012369.jpg" ( 2007, Dec ) SingaporeCity Nothing 1.594 ""
    , Image "PC012315.jpg" ( 2007, Dec ) SingaporeCity Nothing 1.534 ""
    , Image "20151114_144449.jpg" ( 2015, Nov ) Gothenburg Nothing 1.996 ""
    , Image "20151114_155147.jpg" ( 2015, Nov ) Gothenburg Nothing 1.502 ""
    , Image "20151114_150719.jpg" ( 2015, Nov ) Gothenburg Nothing 1.283 "Gothenburg. Ferris wheels, dinosaurs. What else does one need in life really?"
    , Image "20151115_103822.jpg" ( 2015, Nov ) Gothenburg Nothing 1.778 "Slottsskogen has a few ducks."
    , Image "20151104_162854.jpg" ( 2015, Nov ) Stockholm Nothing 1.778 ""
    , Image "20151227_100603.jpg" ( 2015, Dec ) Kristiansund Nothing 1.778 ""
    , Image "20151226_144318.jpg" ( 2015, Dec ) Kristiansund Nothing 1.778 ""
    , Image "20151226_141342.jpg" ( 2015, Dec ) Kristiansund Nothing 0.562 "This was the first time in my life seeing it actually snow. Both Zita and I were happy that day."
    , Image "20151230_175140.jpg" ( 2015, Dec ) Gothenburg Nothing 0.489 ""
    , Image "20151230_173534.jpg" ( 2015, Dec ) Gothenburg Nothing 1.072 "What every tree in Liseberg looks like around Christmas time."
    , Image "20151204_170206.jpg" ( 2015, Dec ) Lund Nothing 1.461 "Check out this majestic bastard."
    , Image "20151204_121250.jpg" ( 2015, Dec ) Lund Nothing 1.61 ""
    , Image "20151204_121304.jpg" ( 2015, Dec ) Lund Nothing 1.553 "Need to put this laser in a vacuum chamber otherwise it sets the air on fire."
    , Image "20151216_201748.jpg" ( 2015, Dec ) Melbourne Nothing 1.778 "38,000 people in attendence. WITNESS ME."
    , Image "IMG_1286.jpg" ( 2015, Dec ) Melbourne Nothing 1.333 "Obligatory PhD selfie with robes and wizard hats."
    , Image "20151216_161510.jpg" ( 2015, Dec ) Melbourne Nothing 2.116 "4 years of my life, aint no-one gonna read them, but they look sooo fancy."
    , Image "20151216_185000.jpg" ( 2015, Dec ) Melbourne Nothing 2.188 "Awww yiss, it's graduatin' time."
    , Image "20151219_155346.jpg" ( 2015, Dec ) Melbourne Nothing 1.245 "Why have a real tree that you have to water and can set on fire, when you can just make one out of lego?"
    , Image "P1060309.jpg" ( 2015, Jul ) KoSamui Nothing 1.73 "Majestic AF"
    , Image "P1060322.jpg" ( 2015, Jul ) KoSamui Nothing 1.5 "These little ones were awesome."
    , Image "IMG_9318.jpg" ( 2015, Jul ) KoSamui Nothing 1.867 ""
    , Image "P1060349.jpg" ( 2015, Jul ) KoSamui Nothing 1.402 "This guy has some good mates."
    , Image "P1060286.jpg" ( 2015, Jul ) KoSamui Nothing 1.298 ""
    , Image "IMG_9457.jpg" ( 2015, Jul ) KoSamui Nothing 1.333 ""
    , Image "P1060289.jpg" ( 2015, Jul ) KoSamui Nothing 1.5 "How's the serenity?"
    , Image "P7230560.jpg" ( 2015, Jul ) KoSamui Nothing 2.215 ""
    , Image "P1060274.jpg" ( 2015, Jul ) KoSamui Nothing 0.898 "Kitty."
    , Image "IMG_9370.jpg" ( 2015, Jul ) KoSamui Nothing 1.333 "We'd had a few long island ice teas on the beach about 15 minutes before this photo..."
    , Image "P1060298.jpg" ( 2015, Jul ) KoSamui Nothing 1.934 "Approaching Ang Thong Marine Park"
    , Image "IMG_9309.jpg" ( 2015, Jul ) KoSamui Nothing 0.75 "Shit like this was all over the place."
    , Image "P1060317.jpg" ( 2015, Jul ) KoSamui Nothing 1.431 ""
    , Image "P1060341.jpg" ( 2015, Jul ) KoSamui Nothing 1.333 "About to head into a cave after winning a race and catching up to the slow pokes in the last batch of people."
    , Image "P1060303.jpg" ( 2015, Jul ) KoSamui Nothing 1.333 ""
    , Image "P1060300.jpg" ( 2015, Jul ) KoSamui Nothing 1.688 ""
    , Image "IMG_9307.jpg" ( 2015, Jul ) KoSamui Nothing 1.333 "The entrance to our funky villa."
    , Image "DSCF3570.jpg" ( 2015, Jul ) Kanchanaburi Nothing 0.779 ""
    , Image "P1060079.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.567 ""
    , Image "IMG_9180.jpg" ( 2015, Jul ) Kanchanaburi Nothing 0.904 ""
    , Image "IMG_9092.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.396 ""
    , Image "DSCF3616.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.333 ""
    , Image "IMG_9122.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.575 ""
    , Image "P1060156.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.584 ""
    , Image "DSCF3644.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.073 ""
    , Image "DSCF3572.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.609 ""
    , Image "DSCF3596.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.181 ""
    , Image "IMG_9121.jpg" ( 2015, Jul ) Kanchanaburi Nothing 0.959 ""
    , Image "DSCF3607.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.751 ""
    , Image "DSCF3595.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.744 ""
    , Image "DSCF3648.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.333 ""
    , Image "P1060068.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.828 ""
    , Image "IMG_9151.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.113 ""
    , Image "P1060097.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.333 ""
    , Image "DSCF3617.jpg" ( 2015, Jul ) Kanchanaburi Nothing 1.151 ""
    , Image "IMG_9138.jpg" ( 2015, Jul ) Kanchanaburi Nothing 0.75 ""
    , Image "IMG_20150723_164223.jpg" ( 2015, Jul ) KoTao Nothing 0.438 ""
    , Image "IMG_9492.jpg" ( 2015, Jul ) KoTao Nothing 3.584 ""
    , Image "P1060371.jpg" ( 2015, Jul ) KoTao Nothing 1.402 "Diving was fun, but we weren't allowed to take photos without some kind of fancy license to do so."
    , Image "IMG_9531.jpg" ( 2015, Jul ) KoTao Nothing 1.622 ""
    , Image "IMG_9523.jpg" ( 2015, Jul ) KoTao Nothing 1.522 ""
    , Image "IMG_9527.jpg" ( 2015, Jul ) KoTao Nothing 0.84 ""
    , Image "P1060362.jpg" ( 2015, Jul ) KoTao Nothing 1.333 ""
    , Image "P1060433.jpg" ( 2015, Jul ) KoTao Nothing 1.881 ""
    , Image "P1060413.jpg" ( 2015, Jul ) KoTao Nothing 1.384 ""
    , Image "P1060376.jpg" ( 2015, Jul ) KoTao Nothing 0.823 ""
    , Image "P1060264.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.333 ""
    , Image "IMG_20150721_082106.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.163 ""
    , Image "P7210516.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.574 ""
    , Image "IMG_9211.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.75 ""
    , Image "P1060221.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.75 ""
    , Image "P7210467.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.866 ""
    , Image "P7210505.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.775 ""
    , Image "P1060244.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.106 ""
    , Image "P7210531.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.775 ""
    , Image "P1060238.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.889 ""
    , Image "P7210482.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.435 ""
    , Image "P1060271.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.681 ""
    , Image "P1060247.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.28 ""
    , Image "P1060213.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.151 ""
    , Image "IMG_9214.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.929 ""
    , Image "IMG_20150721_081630.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.514 "Wat Ratchaburana"
    , Image "IMG_9218.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.824 "Wat Phra Si Sanphet"
    , Image "IMG_9255.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.395 ""
    , Image "IMG_9227.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.883 "Kitty."
    , Image "IMG_9220.jpg" ( 2015, Jul ) Ayutthaya Nothing 0.85 ""
    , Image "P1060251.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.503 ""
    , Image "IMG_9273.jpg" ( 2015, Jul ) Ayutthaya Nothing 2.014 ""
    , Image "IMG_9239.jpg" ( 2015, Jul ) Ayutthaya Nothing 1.775 "Wat Yai Chai Mongkol"
    , Image "DSCF3531.jpg" ( 2015, Jul ) Bangkok Nothing 1.744 "Wat Pho"
    , Image "DSCF3522.jpg" ( 2015, Jul ) Bangkok Nothing 0.684 ""
    , Image "IMG_8973.jpg" ( 2015, Jul ) Bangkok Nothing 0.786 "MahaNakhon, cunnently under construction. Will be the tallest building in Bangkok and perhaps the largest tetris game also."
    , Image "DSCF3564.jpg" ( 2015, Jul ) Bangkok Nothing 1.22 "Democracy Monument"
    , Image "IMG_9069.jpg" ( 2015, Jul ) Bangkok Nothing 0.624 ""
    , Image "DSCF3533.jpg" ( 2015, Jul ) Bangkok Nothing 1.083 "People were putting at least one coin in every single one of these."
    , Image "DSCF3544.jpg" ( 2015, Jul ) Bangkok Nothing 1.113 ""
    , Image "DSCF3555.jpg" ( 2015, Jul ) Bangkok Nothing 1.333 ""
    , Image "IMG_8983.jpg" ( 2015, Jul ) Bangkok Nothing 0.75 ""
    , Image "IMG_9061.jpg" ( 2015, Jul ) Bangkok Nothing 1.291 "Kitty."
    , Image "DSCF3540.jpg" ( 2015, Jul ) Bangkok Nothing 0.786 ""
    , Image "DSCF3509.jpg" ( 2015, Jul ) Bangkok Nothing 1.449 ""
    , Image "DSCF3342.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.411 ""
    , Image "DSCF3346.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.037 "This juxtaposition is constant here."
    , Image "IMG_8644.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 0.75 ""
    , Image "P1050909.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.641 ""
    , Image "P1050857.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.232 "Kitty."
    , Image "P7150231.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.267 "How to make rice paper rolls. Simple process, difficult to master."
    , Image "IMG_8638.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.333 "At least every second shop is a scooter mechanic."
    , Image "DSCF3368.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.333 ""
    , Image "P1050910.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.555 ""
    , Image "IMG_8622.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 0.821 ""
    , Image "DSCF3334.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.333 "Standard curb side electrical cabling here."
    , Image "P1050891.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.097 "One of the fox holes in the Củ Chi tunnel system."
    , Image "P1050919.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 0.75 ""
    , Image "IMG_8615.jpg" ( 2015, Jul ) HoChiMinhCity Nothing 1.333 ""
    , Image "P7180423.jpg" ( 2015, Jul ) Hanoi Nothing 0.564 ""
    , Image "DSCF3478.jpg" ( 2015, Jul ) Hanoi Nothing 1.881 ""
    , Image "DSCF3494.jpg" ( 2015, Jul ) Hanoi Nothing 1.098 ""
    , Image "DSCF3500.jpg" ( 2015, Jul ) Hanoi Nothing 1.739 ""
    , Image "IMG_8947.jpg" ( 2015, Jul ) Hanoi Nothing 1.739 ""
    , Image "DSCF3473.jpg" ( 2015, Jul ) Hanoi Nothing 0.862 ""
    , Image "P7180378.jpg" ( 2015, Jul ) Hanoi Nothing 0.889 "Outside the temple of liturature."
    , Image "DSCF3491.jpg" ( 2015, Jul ) Hanoi Nothing 1.333 ""
    , Image "DSCF3499.jpg" ( 2015, Jul ) Hanoi Nothing 1.138 ""
    , Image "IMG_8882.jpg" ( 2015, Jul ) Hanoi Nothing 0.75 "Tropical rainstorm in a temple? Check."
    , Image "IMG_8656.jpg" ( 2015, Jul ) Hanoi Nothing 1.103 ""
    , Image "P7160369.jpg" ( 2015, Jul ) HaLongBay Nothing 1.333 ""
    , Image "P1050960.jpg" ( 2015, Jul ) HaLongBay Nothing 1.333 ""
    , Image "DSCF3444.jpg" ( 2015, Jul ) HaLongBay Nothing 1.333 ""
    , Image "IMG_8679.jpg" ( 2015, Jul ) HaLongBay Nothing 3.361 ""
    , Image "P1060048.jpg" ( 2015, Jul ) HaLongBay Nothing 1.333 ""
    , Image "P1050968.jpg" ( 2015, Jul ) HaLongBay Nothing 2.043 ""
    , Image "P7160276.jpg" ( 2015, Jul ) HaLongBay Nothing 2.375 ""
    , Image "DSCF3452.jpg" ( 2015, Jul ) HaLongBay Nothing 3.872 ""
    , Image "P1060066.jpg" ( 2015, Jul ) HaLongBay Nothing 2.047 ""
    , Image "P7160327.jpg" ( 2015, Jul ) HaLongBay Nothing 1.947 ""
    , Image "P7160272.jpg" ( 2015, Jul ) HaLongBay Nothing 2.16 ""
    , Image "IMG_8760.jpg" ( 2015, Jul ) HaLongBay Nothing 2.345 ""
    , Image "DSCF3408.jpg" ( 2015, Jul ) HaLongBay Nothing 1.052 ""
    , Image "IMG_8757.jpg" ( 2015, Jul ) HaLongBay Nothing 0.75 ""
    , Image "P7160337.jpg" ( 2015, Jul ) HaLongBay Nothing 1.745 ""
    , Image "P7160367.jpg" ( 2015, Jul ) HaLongBay Nothing 0.75 ""
    , Image "P1060011.jpg" ( 2015, Jul ) HaLongBay Nothing 1.711 "An excellent chance to explore."
    , Image "P1060060.jpg" ( 2015, Jul ) HaLongBay Nothing 1.333 ""
    , Image "DSCF3419.jpg" ( 2015, Jul ) HaLongBay Nothing 1.333 ""
    , Image "DSCF3440.jpg" ( 2015, Jul ) HaLongBay Nothing 2.525 ""
    , Image "DSCF3400.jpg" ( 2015, Jul ) HaLongBay Nothing 1.837 ""
    , Image "DSCF3418.jpg" ( 2015, Jul ) HaLongBay Nothing 2.002 ""
    , Image "DSCF0606.jpg" ( 2012, Aug ) Prague Nothing 1.99 ""
    , Image "DSCF0679.jpg" ( 2012, Aug ) Prague Nothing 0.75 ""
    , Image "DSCF0778.jpg" ( 2012, Aug ) Prague Nothing 1.333 ""
    , Image "DSCF0723.jpg" ( 2012, Aug ) Prague Nothing 1.0 ""
    , Image "DSCF0737.jpg" ( 2012, Aug ) Prague Nothing 1.474 ""
    , Image "DSCF0602.jpg" ( 2012, Aug ) Prague Nothing 1.613 ""
    , Image "DSCF0711.jpg" ( 2012, Aug ) Prague Nothing 1.671 ""
    , Image "DSCF0766.jpg" ( 2012, Aug ) Prague Nothing 2.132 ""
    , Image "DSCF0702.jpg" ( 2012, Aug ) Prague Nothing 0.546 ""
    , Image "DSCF0747.jpg" ( 2012, Aug ) Prague Nothing 1.002 ""
    , Image "DSCF0692.jpg" ( 2012, Aug ) Prague Nothing 1.765 ""
    , Image "DSCF0634.jpg" ( 2012, Aug ) Prague Nothing 0.578 ""
    , Image "DSCF0780.jpg" ( 2012, Aug ) Prague Nothing 1.307 ""
    , Image "DSCF0755.jpg" ( 2012, Aug ) Prague Nothing 1.333 ""
    , Image "DSCF0654.jpg" ( 2012, Aug ) Prague Nothing 1.183 ""
    , Image "DSCF0632.jpg" ( 2012, Aug ) Prague Nothing 0.97 ""
    , Image "DSCF0613.jpg" ( 2012, Aug ) Prague Nothing 1.42 ""
    , Image "DSCF0746.jpg" ( 2012, Aug ) Prague Nothing 1.496 ""
    , Image "DSCF0777.jpg" ( 2012, Aug ) Prague Nothing 1.537 ""
    , Image "DSCF0743.jpg" ( 2012, Aug ) Prague Nothing 2.043 ""
    , Image "DSCF0652.jpg" ( 2012, Aug ) Prague Nothing 1.333 ""
    , Image "DSCF0703.jpg" ( 2012, Aug ) Prague Nothing 1.627 ""
    , Image "DSCF0612.jpg" ( 2012, Aug ) Prague Nothing 0.615 ""
    , Image "DSCF0609.jpg" ( 2012, Aug ) Prague Nothing 1.572 ""
    , Image "DSCF0656.jpg" ( 2012, Aug ) Prague Nothing 0.75 ""
    , Image "DSCF0726.jpg" ( 2012, Aug ) Prague Nothing 1.36 ""
    , Image "DSCF0878.jpg" ( 2012, Aug ) Berlin Nothing 1.109 ""
    , Image "DSCF0888.jpg" ( 2012, Aug ) Berlin Nothing 1.489 ""
    , Image "DSCF0944.jpg" ( 2012, Aug ) Berlin Nothing 1.554 "The Holocaust Memorial"
    , Image "DSCF0979.jpg" ( 2012, Aug ) Berlin Nothing 1.333 "Checkpoint Charlie"
    , Image "DSCF0879.jpg" ( 2012, Aug ) Berlin Nothing 1.609 "Sachsenhausen concentration camp"
    , Image "DSCF0880.jpg" ( 2012, Aug ) Berlin Nothing 1.333 "'Work sets you free'"
    , Image "DSCF1276.jpg" ( 2012, Aug ) Berlin Nothing 0.921 ""
    , Image "DSCF1214.jpg" ( 2012, Aug ) Berlin Nothing 0.494 ""
    , Image "DSCF1154.jpg" ( 2012, Aug ) Berlin Nothing 1.333 ""
    , Image "DSCF1266.jpg" ( 2012, Aug ) Berlin Nothing 1.272 "Public fountains are better than pools here."
    , Image "DSCF1019.jpg" ( 2012, Aug ) Berlin Nothing 0.648 ""
    , Image "DSCF0999.jpg" ( 2012, Aug ) Berlin Nothing 1.108 ""
    , Image "DSCF1027.jpg" ( 2012, Aug ) Berlin Nothing 1.136 ""
    , Image "DSCF1181.jpg" ( 2012, Aug ) Berlin Nothing 0.625 ""
    , Image "DSCF1039.jpg" ( 2012, Aug ) Berlin Nothing 1.333 ""
    , Image "DSCF0329.jpg" ( 2012, Aug ) Vienna Nothing 1.61 ""
    , Image "Vienna059.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "Vienna042.jpg" ( 2012, Aug ) Vienna Nothing 1.488 ""
    , Image "DSCF0263.jpg" ( 2012, Aug ) Vienna Nothing 0.88 ""
    , Image "IMAG0485.jpg" ( 2012, Aug ) Vienna Nothing 0.666 ""
    , Image "DSCF0215.jpg" ( 2012, Aug ) Vienna Nothing 0.75 ""
    , Image "Vienna016.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "DSCF0180.jpg" ( 2012, Aug ) Vienna Nothing 1.126 ""
    , Image "Vienna038.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "IMAG0417.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "DSCF0317.jpg" ( 2012, Aug ) Vienna Nothing 1.26 ""
    , Image "DSCF0359.jpg" ( 2012, Aug ) Vienna Nothing 2.275 ""
    , Image "DSCF0367.jpg" ( 2012, Aug ) Vienna Nothing 0.97 ""
    , Image "DSCF0361.jpg" ( 2012, Aug ) Vienna Nothing 1.747 ""
    , Image "DSCF0239.jpg" ( 2012, Aug ) Vienna Nothing 1.107 ""
    , Image "DSCF0308.jpg" ( 2012, Aug ) Vienna Nothing 1.011 ""
    , Image "DSCF0166.jpg" ( 2012, Aug ) Vienna Nothing 1.274 ""
    , Image "DSCF0255.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "DSCF0306.jpg" ( 2012, Aug ) Vienna Nothing 1.591 ""
    , Image "DSCF0284.jpg" ( 2012, Aug ) Vienna Nothing 1.329 ""
    , Image "DSCF0205.jpg" ( 2012, Aug ) Vienna Nothing 1.857 ""
    , Image "DSCF0250.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "DSCF0294.jpg" ( 2012, Aug ) Vienna Nothing 1.447 ""
    , Image "DSCF0275.jpg" ( 2012, Aug ) Vienna Nothing 1.333 ""
    , Image "DSCF0515.jpg" ( 2012, Aug ) Budapest Nothing 1.206 "This was a friendly one."
    , Image "DSCF0429.jpg" ( 2012, Aug ) Budapest Nothing 1.969 ""
    , Image "DSCF0565.jpg" ( 2012, Aug ) Budapest Nothing 1.885 ""
    , Image "DSCF0505.jpg" ( 2012, Aug ) Budapest Nothing 1.333 ""
    , Image "DSCF0370.jpg" ( 2012, Aug ) Budapest Nothing 1.281 "Heroes' Square"
    , Image "DSCF0551.jpg" ( 2012, Aug ) Budapest Nothing 1.171 "Liberty Statue on Gellért Hill"
    , Image "DSCF0378.jpg" ( 2012, Aug ) Budapest Nothing 1.333 ""
    , Image "DSCF0381.jpg" ( 2012, Aug ) Budapest Nothing 2.421 ""
    , Image "DSCF0485.jpg" ( 2012, Aug ) Budapest Nothing 0.856 ""
    , Image "DSCF0472.jpg" ( 2012, Aug ) Budapest Nothing 1.078 ""
    , Image "DSCF0439.jpg" ( 2012, Aug ) Budapest Nothing 1.333 "Buda Castle"
    , Image "DSCF0489.jpg" ( 2012, Aug ) Budapest Nothing 1.385 ""
    , Image "DSCF0531.jpg" ( 2012, Aug ) Budapest Nothing 1.404 ""
    , Image "DSCF0578.jpg" ( 2012, Aug ) Budapest Nothing 1.236 ""
    , Image "DSCF0463.jpg" ( 2012, Aug ) Budapest Nothing 0.782 ""
    , Image "DSCF0486.jpg" ( 2012, Aug ) Budapest Nothing 1.447 ""
    , Image "IMAG0538.jpg" ( 2012, Aug ) Budapest Nothing 1.043 ""
    , Image "DSCF0510.jpg" ( 2012, Aug ) Budapest Nothing 1.663 ""
    , Image "DSCF0413.jpg" ( 2012, Aug ) Budapest Nothing 1.091 ""
    , Image "DSCF0431.jpg" ( 2012, Aug ) Budapest Nothing 1.399 "Parliament"
    , Image "DSCF0387.jpg" ( 2012, Aug ) Budapest Nothing 1.333 ""
    , Image "DSCF0458.jpg" ( 2012, Aug ) Budapest Nothing 1.837 "The Chain Bridge"
    , Image "DSCF0576.jpg" ( 2012, Aug ) Budapest Nothing 0.517 ""
    , Image "DSCF1493.jpg" ( 2012, Aug ) Helsingor Nothing 1.394 ""
    , Image "DSCF1520.jpg" ( 2012, Aug ) Helsingor Nothing 1.333 "Ogier the Dane. Famous in France, although the Danes don't really know who he is."
    , Image "DSCF1484.jpg" ( 2012, Aug ) Helsingor Nothing 1.333 ""
    , Image "DSCF1499.jpg" ( 2012, Aug ) Helsingor Nothing 1.337 ""
    , Image "DSCF1704.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.178 ""
    , Image "DSCF1690.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.488 "This dog only speaks Danish, so we didn't have much to talk about."
    , Image "IMAG0622.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.498 "The lights across the water are in Sweden. We're standing in Denmark."
    , Image "DSCF1717.jpg" ( 2012, Aug ) Dronningmolle Nothing 0.9 ""
    , Image "DSCF1702.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.554 ""
    , Image "DSCF1460.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.333 ""
    , Image "DSCF1724.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.333 ""
    , Image "DSCF1726.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.333 ""
    , Image "DSCF1721.jpg" ( 2012, Aug ) Dronningmolle Nothing 1.483 ""
    , Image "DSCF1611.jpg" ( 2012, Aug ) Copenhagen Nothing 1.28 ""
    , Image "DSCF1622.jpg" ( 2012, Aug ) Copenhagen Nothing 1.794 ""
    , Image "DSCF1542.jpg" ( 2012, Aug ) Copenhagen Nothing 1.339 ""
    , Image "DSCF1470.jpg" ( 2012, Aug ) Copenhagen Nothing 1.333 ""
    , Image "DSCF1669.jpg" ( 2012, Aug ) Copenhagen Nothing 0.75 "Taken whilst waiting for my mate to vomit up the rest of his massively expensive steak after going on this ride."
    , Image "DSCF1571.jpg" ( 2012, Aug ) Copenhagen Nothing 1.106 ""
    , Image "DSCF1469.jpg" ( 2012, Aug ) Copenhagen Nothing 1.333 ""
    , Image "DSCF1548.jpg" ( 2012, Aug ) Copenhagen Nothing 1.55 ""
    , Image "DSCF1659.jpg" ( 2012, Aug ) Copenhagen Nothing 1.302 ""
    , Image "DSCF1660.jpg" ( 2012, Aug ) Copenhagen Nothing 0.77 ""
    , Image "DSCF1604.jpg" ( 2012, Aug ) Copenhagen Nothing 1.313 "A sand castle competition was in full swing"
    , Image "DSCF1533.jpg" ( 2012, Aug ) Helsingborg Nothing 1.304 ""
    , Image "DSCF1530.jpg" ( 2012, Aug ) Helsingborg Nothing 0.944 ""
    , Image "DSCF1412.jpg" ( 2012, Aug ) Riga Nothing 0.992 ""
    , Image "DSCF1329.jpg" ( 2012, Aug ) Riga Nothing 1.329 ""
    , Image "DSCF1308.jpg" ( 2012, Aug ) Riga Nothing 1.567 ""
    , Image "DSCF1315.jpg" ( 2012, Aug ) Riga Nothing 0.699 ""
    , Image "DSCF1342.jpg" ( 2012, Aug ) Riga Nothing 1.363 ""
    , Image "DSCF1397.jpg" ( 2012, Aug ) Riga Nothing 1.274 ""
    , Image "DSCF1435.jpg" ( 2012, Aug ) Riga Nothing 0.82 ""
    , Image "DSCF1384.jpg" ( 2012, Aug ) Riga Nothing 1.333 ""
    , Image "DSCF1359.jpg" ( 2012, Aug ) Riga Nothing 1.225 ""
    , Image "DSCF1420.jpg" ( 2012, Aug ) Riga Nothing 1.436 ""
    , Image "DSCF1328.jpg" ( 2012, Aug ) Riga Nothing 0.891 ""
    , Image "DSCF1285.jpg" ( 2012, Aug ) Riga Nothing 0.861 "Latvian Academy of Sciences"
    , Image "DSCF1356.jpg" ( 2012, Aug ) Riga Nothing 2.381 ""
    , Image "DSCF1430.jpg" ( 2012, Aug ) Riga Nothing 0.493 "Freedom Monument"
    , Image "DSCF1297.jpg" ( 2012, Aug ) Riga Nothing 1.562 ""
    , Image "DSCF1311.jpg" ( 2012, Aug ) Riga Nothing 1.333 ""
    , Image "DSCF1324.jpg" ( 2012, Aug ) Riga Nothing 1.037 ""
    , Image "DSCF1386.jpg" ( 2012, Aug ) Riga Nothing 0.652 ""
    , Image "DSCF1321.jpg" ( 2012, Aug ) Riga Nothing 0.497 "Venus of Willendorf 21st century"
    , Image "DSCF1391.jpg" ( 2012, Aug ) Riga Nothing 0.75 ""
    , Image "DSCF2110.jpg" ( 2012, Aug ) Paris Nothing 0.949 ""
    , Image "DSCF1912.jpg" ( 2012, Aug ) Paris Nothing 1.333 "No issues chasing you down whilst still looking radical. Didn't see them do any sweet jumps though."
    , Image "DSCF1993.jpg" ( 2012, Aug ) Paris Nothing 1.369 ""
    , Image "DSCF1786.jpg" ( 2012, Aug ) Paris Nothing 1.295 "The Pantheon. Mausoleum for the greatest French citizens and all round excellent museum."
    , Image "DSCF1975.jpg" ( 2012, Aug ) Paris Nothing 1.333 "Near Centre Pompidou"
    , Image "DSCF1871.jpg" ( 2012, Aug ) Paris Nothing 1.693 ""
    , Image "DSCF1992.jpg" ( 2012, Aug ) Paris Nothing 1.349 "Champs-Elysees has a lot of concept stores with high end cars in them."
    , Image "DSCF1854.jpg" ( 2012, Aug ) Paris Nothing 1.272 ""
    , Image "DSCF2221.jpg" ( 2012, Aug ) Paris Nothing 1.333 ""
    , Image "DSCF1778.jpg" ( 2012, Aug ) Paris Nothing 1.24 ""
    , Image "DSCF1746.jpg" ( 2012, Aug ) Paris Nothing 0.878 ""
    , Image "DSCF2332.jpg" ( 2012, Aug ) Paris Nothing 0.798 ""
    , Image "DSCF2512.jpg" ( 2012, Aug ) Paris Nothing 1.079 "The cottage I stayed in in Bures-sur-Yvette"
    , Image "DSCF2310.jpg" ( 2012, Aug ) Paris Nothing 0.584 ""
    , Image "DSCF1732.jpg" ( 2012, Aug ) Paris Nothing 1.41 ""
    , Image "DSCF2193.jpg" ( 2012, Aug ) Paris Nothing 2.849 ""
    , Image "DSCF2418.jpg" ( 2012, Aug ) Paris Nothing 1.009 ""
    , Image "DSCF2174.jpg" ( 2012, Aug ) Paris Nothing 1.02 ""
    , Image "DSCF1819.jpg" ( 2012, Aug ) Paris Nothing 1.333 ""
    , Image "DSCF2071.jpg" ( 2012, Aug ) Paris Nothing 1.294 ""
    , Image "DSCF1846.jpg" ( 2012, Aug ) Paris Nothing 1.333 ""
    , Image "DSCF2121.jpg" ( 2012, Aug ) Paris Nothing 0.296 ""
    , Image "DSCF1997.jpg" ( 2012, Aug ) Paris Nothing 1.591 "Arc de Triomphe"
    , Image "DSCF1783.jpg" ( 2012, Aug ) Paris Nothing 1.014 ""
    , Image "DSCF2068.jpg" ( 2012, Aug ) Paris Nothing 1.077 "Jardins du Trocadero"
    , Image "DSCF2011.jpg" ( 2012, Aug ) Paris Nothing 1.231 ""
    , Image "DSCF1757.jpg" ( 2012, Aug ) Paris Nothing 0.937 ""
    , Image "DSCF2049.jpg" ( 2012, Aug ) Paris Nothing 1.159 ""
    , Image "DSCF1748.jpg" ( 2012, Aug ) Paris Nothing 1.048 "Notre Dame Cathedral"
    , Image "DSCF1857.jpg" ( 2012, Aug ) Paris Nothing 1.333 ""
    , Image "DSCF2035.jpg" ( 2012, Aug ) Paris Nothing 1.333 ""
    , Image "DSCF1880.jpg" ( 2012, Aug ) Paris Nothing 1.168 ""
    , Image "DSCF2504.jpg" ( 2012, Aug ) Paris Nothing 1.333 ""
    , Image "DSCF1981.jpg" ( 2012, Aug ) Paris Nothing 1.638 ""
    , Image "DSCF2072.jpg" ( 2012, Aug ) Paris Nothing 1.705 ""
    , Image "DSCF1943.jpg" ( 2012, Aug ) Paris Nothing 0.587 "The Obelisk of Luxor at the center of the Place de la Concorde."
    , Image "DSCF2369.jpg" ( 2012, Aug ) Paris Nothing 0.703 ""
    , Image "DSCF2057.jpg" ( 2012, Aug ) Paris Nothing 1.129 ""
    , Image "DSCF1809.jpg" ( 2012, Aug ) Paris Nothing 1.179 ""
    , Image "DSCF2485.jpg" ( 2012, Aug ) Paris Nothing 0.389 ""
    , Image "20121208_145750.jpg" ( 2012, Dec ) Sydney Nothing 1.833 ""
    , Image "20121208_133354.jpg" ( 2012, Dec ) Sydney Nothing 1.333 ""
    , Image "20121208_130103.jpg" ( 2012, Dec ) Sydney Nothing 1.874 ""
    , Image "20121208_130006.jpg" ( 2012, Dec ) Sydney Nothing 1.754 ""
    , Image "20121208_150722.jpg" ( 2012, Dec ) Sydney Nothing 1.83 "Bondi beach"
    , Image "20121208_114132.jpg" ( 2012, Dec ) Sydney Nothing 1.639 "Sculpture at UNSW"
    , Image "20121209_133041.jpg" ( 2012, Dec ) Sydney Nothing 2.442 ""
    , Image "20121208_132944.jpg" ( 2012, Dec ) Sydney Nothing 1.333 ""
    , Image "P7265169.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.818 ""
    , Image "DSCF0063.jpg" ( 2012, Jul ) Karlsruhe Nothing 0.936 ""
    , Image "IMAG0344.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.627 ""
    , Image "IMAG0340.jpg" ( 2012, Jul ) Karlsruhe Nothing 2.268 ""
    , Image "IMAG0367.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.5 ""
    , Image "075.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.494 ""
    , Image "P7185159.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.59 ""
    , Image "DSCF0014.jpg" ( 2012, Jul ) Karlsruhe Nothing 0.645 ""
    , Image "DSCF0015.jpg" ( 2012, Jul ) Karlsruhe Nothing 0.671 ""
    , Image "P7185158.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.797 ""
    , Image "DSCF0027.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.402 ""
    , Image "DSCF0022.jpg" ( 2012, Jul ) Karlsruhe Nothing 2.915 ""
    , Image "IMAG0359.jpg" ( 2012, Jul ) Karlsruhe Nothing 2.047 ""
    , Image "DSCF0085.jpg" ( 2012, Jul ) Karlsruhe Nothing 1.149 ""
    , Image "IMAG0370.jpg" ( 2012, Jul ) Heidelberg Nothing 1.552 ""
    , Image "DSCF0099.jpg" ( 2012, Jul ) Heidelberg Nothing 1.682 ""
    , Image "DSCF0101.jpg" ( 2012, Jul ) Heidelberg Nothing 1.972 ""
    , Image "P1050947.jpg" ( 2012, Jul ) Munich Nothing 1.008 ""
    , Image "P1050941.jpg" ( 2012, Jul ) Munich Nothing 1.244 ""
    , Image "P7295178.jpg" ( 2012, Jul ) Munich Nothing 1.427 ""
    , Image "P1050988.jpg" ( 2012, Jul ) Munich Nothing 1.5 ""
    , Image "P1050945.jpg" ( 2012, Jul ) Munich Nothing 1.331 ""
    , Image "P1050949.jpg" ( 2012, Jul ) Munich Nothing 1.249 ""
    , Image "IMG_20180330_143852.jpg" ( 2018, Mar ) Gothenburg Nothing 1.771 ""
    , Image "DSC_1741.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "DSC_1693.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "20180102_141636.jpg" ( 2018, Jan ) Revsund Nothing 0.75 ""
    , Image "DSC_1711.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "DSC_1722.jpg" ( 2018, Jan ) Revsund Nothing 1.905 ""
    , Image "DSC_1739.jpg" ( 2018, Jan ) Revsund Nothing 1.373 ""
    , Image "DSC_1720.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "DSC_1700.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "20180102_142312.jpg" ( 2018, Jan ) Revsund Nothing 0.9 ""
    , Image "DSC_1715.jpg" ( 2018, Jan ) Revsund Nothing 1.496 ""
    , Image "20180102_141540.jpg" ( 2018, Jan ) Revsund Nothing 0.75 ""
    , Image "DSC_1706.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "DSC_1696.jpg" ( 2018, Jan ) Revsund Nothing 1.496 ""
    , Image "DSC_1684.jpg" ( 2018, Jan ) Revsund Nothing 1.778 ""
    , Image "DSC_1738.jpg" ( 2018, Jan ) Revsund Nothing 0.562 ""
    , Image "DSC_1732.jpg" ( 2018, Jan ) Revsund Nothing 0.562 ""
    , Image "DSC_1758.jpg" ( 2018, Jan ) Gothenburg Nothing 1.778 ""
    , Image "20180103_144708.jpg" ( 2018, Jan ) Ostersund Nothing 0.656 ""
    , Image "DSC_1744.jpg" ( 2018, Jan ) Ostersund Nothing 1.778 ""
    , Image "DSC_1785.jpg" ( 2018, Feb ) Stockholm Nothing 1.548 ""
    , Image "IMG_20180410_172032.jpg" ( 2018, Apr ) Exeter Nothing 1.735 ""
    , Image "IMG_20180412_181630.jpg" ( 2018, Apr ) Exeter Nothing 0.706 ""
    , Image "IMG_20180411_195352.jpg" ( 2018, Apr ) Exeter Nothing 0.565 ""
    , Image "IMG_20180412_173719.jpg" ( 2018, Apr ) Exeter Nothing 0.867 ""
    , Image "IMG_20180412_084834.jpg" ( 2018, Apr ) Exeter Nothing 0.55 ""
    , Image "IMG_20180412_181208.jpg" ( 2018, Apr ) Exeter Nothing 1.771 ""
    , Image "IMG_20180412_181238.jpg" ( 2018, Apr ) Exeter Nothing 1.515 ""
    , Image "IMG_20180411_113253.jpg" ( 2018, Apr ) Exeter Nothing 1.075 ""
    , Image "IMG_20180412_181652.jpg" ( 2018, Apr ) Exeter Nothing 0.566 ""
    , Image "IMG_20180412_162741.jpg" ( 2018, Apr ) Exeter Nothing 1.771 ""
    , Image "IMG_20180412_173902.jpg" ( 2018, Apr ) Exeter Nothing 0.565 ""
    , Image "IMG_20180412_174846.jpg" ( 2018, Apr ) Exeter Nothing 1.351 ""
    , Image "IMG_20180402_153200_1.jpg" ( 2018, Apr ) Gothenburg Nothing 1.771 ""
    , Image "IMG_20180510_162204.jpg" ( 2018, May ) Gothenburg Nothing 1.336 ""
    , Image "IMG_20180510_132752.jpg" ( 2018, May ) Gothenburg Nothing 2.632 ""
    , Image "IMG_20180510_145837.jpg" ( 2018, May ) Gothenburg Nothing 1.771 ""
    , Image "IMG_20180510_134356.jpg" ( 2018, May ) Gothenburg Nothing 1.745 ""
    , Image "IMG_20180510_142626.jpg" ( 2018, May ) Gothenburg Nothing 0.839 ""
    , Image "P6105510.jpg" ( 2014, Jun ) Skaftafell Nothing 1.833 ""
    , Image "P6115513.jpg" ( 2014, Jun ) Skaftafell Nothing 1.333 ""
    , Image "P6115519.jpg" ( 2014, Jun ) Skaftafell Nothing 1.52 "Close to the entrance of Skaftafell national park. Home of the highest mountain and largest glacier in Iceland."
    , Image "P6105462.jpg" ( 2014, Jun ) Reykjavik Nothing 2.439 ""
    , Image "P6105455.jpg" ( 2014, Jun ) Geysir Nothing 2.345 ""
    , Image "P6105452.jpg" ( 2014, Jun ) Geysir Nothing 1.333 "Gullfoss"
    , Image "P6105473.jpg" ( 2014, Jun ) Skogarfoss Nothing 1.434 ""
    , Image "P6105472.jpg" ( 2014, Jun ) Skogarfoss Nothing 1.098 "Skógarfoss. The small white dots all over are seagulls nesting."
    , Image "P6105497.jpg" ( 2014, Jun ) Skogarfoss Nothing 1.778 "What the lava flows look like up close."
    , Image "P6115591.jpg" ( 2014, Jun ) Vik Nothing 1.333 "Seljalandsfoss"
    , Image "P6115586.jpg" ( 2014, Jun ) Vik Nothing 1.515 ""
    , Image "P6115553.jpg" ( 2014, Jun ) Jokulsarlon Nothing 1.534 ""
    , Image "P6115575.jpg" ( 2014, Jun ) Jokulsarlon Nothing 1.842 "Jökulsárlón. A glacial lagoon that was probably the most memorable part of my travels in Iceland."
    , Image "P6115567.jpg" ( 2014, Jun ) Jokulsarlon Nothing 2.358 "The blue colour means it's flipped over within the last 24 hours or so."
    , Image "P6155690.jpg" ( 2014, Jun ) Rorvik Nothing 1.744 ""
    , Image "DSCF2667.jpg" ( 2014, Jun ) Rorvik Nothing 2.389 ""
    , Image "DSCF2662.jpg" ( 2014, Jun ) Rorvik Nothing 1.823 ""
    , Image "P6155680.jpg" ( 2014, Jun ) Rorvik Nothing 1.696 ""
    , Image "DSCF2669.jpg" ( 2014, Jun ) Rorvik Nothing 1.813 ""
    , Image "P6155684.jpg" ( 2014, Jun ) Rorvik Nothing 1.161 "My very expensive home for a week."
    , Image "P6155785.jpg" ( 2014, Jun ) Bodo Nothing 1.775 "A sea eagle. After finally seeing one the American guy stopped correcting the guide: 'It's not sea eagle, it's sea gull.'"
    , Image "P6155771.jpg" ( 2014, Jun ) Bodo Nothing 1.333 "Largest tidal current in the world. Doesn't look like much in photos, but this thing was pretty powerful."
    , Image "P6155701.jpg" ( 2014, Jun ) Bodo Nothing 1.855 "Tectonic plate action."
    , Image "P6145644.jpg" ( 2014, Jun ) Trondheim Nothing 1.131 "A statue Olav Tryggvason that's also a sun dial."
    , Image "P6145665.jpg" ( 2014, Jun ) Trondheim Nothing 1.575 ""
    , Image "P6145662.jpg" ( 2014, Jun ) Trondheim Nothing 1.333 ""
    , Image "P6145656.jpg" ( 2014, Jun ) Trondheim Nothing 1.522 "View from the Old Town bridge."
    , Image "P6145651.jpg" ( 2014, Jun ) Trondheim Nothing 1.689 ""
    , Image "P6145671.jpg" ( 2014, Jun ) Trondheim Nothing 1.241 ""
    , Image "P6145648.jpg" ( 2014, Jun ) Trondheim Nothing 1.168 "Nidaros Domkirke. Complete with very annoying sun position."
    , Image "P6145640.jpg" ( 2014, Jun ) Trondheim Nothing 1.398 ""
    , Image "DSCF2712.jpg" ( 2014, Jun ) Svolvaer Nothing 1.591 ""
    , Image "P6165837.jpg" ( 2014, Jun ) Svolvaer Nothing 1.333 ""
    , Image "P6155696.jpg" ( 2014, Jun ) Svolvaer Nothing 2.286 ""
    , Image "P6165809.jpg" ( 2014, Jun ) Svolvaer Nothing 1.564 "Warden of the north."
    , Image "P6165854.jpg" ( 2014, Jun ) Svolvaer Nothing 1.526 "Trollfjorden. Only 90 m wide at its entrance, the mountains rocket up wither side over 200 m almost vertically. The ship had very dramatic classical music playing over the loudspeakers as we traveled though it."
    , Image "P6196034.jpg" ( 2014, Jun ) Tromso Nothing 1.333 ""
    , Image "P6195999.jpg" ( 2014, Jun ) Tromso Nothing 1.333 ""
    , Image "P6195984.jpg" ( 2014, Jun ) Tromso Nothing 1.581 ""
    , Image "P6196025.jpg" ( 2014, Jun ) Tromso Nothing 1.642 ""
    , Image "DSCF2756.jpg" ( 2014, Jun ) Tromso Nothing 1.333 ""
    , Image "P6196046.jpg" ( 2014, Jun ) Tromso Nothing 1.398 ""
    , Image "P6185974.jpg" ( 2014, Jun ) Tromso Nothing 1.645 ""
    , Image "P6195975.jpg" ( 2014, Jun ) Tromso Nothing 1.77 ""
    , Image "P6185943.jpg" ( 2014, Jun ) Tromso Nothing 1.333 "Arctic botanical garden was in full bloom."
    , Image "P6175878.jpg" ( 2014, Jun ) Tromso Nothing 1.679 ""
    , Image "DSCF2704.jpg" ( 2014, Jun ) Tromso Nothing 1.333 ""
    , Image "P6185972.jpg" ( 2014, Jun ) Tromso Nothing 1.537 ""
    , Image "DSCF2764.jpg" ( 2014, Jun ) Tromso Nothing 1.415 ""
    , Image "DSCF2640.jpg" ( 2014, Jun ) Kristiansund Nothing 1.333 ""
    , Image "P6135625.jpg" ( 2014, Jun ) Kristiansund Nothing 1.736 "The terribly boring view from my friends' front yard."
    , Image "DSCF2654.jpg" ( 2014, Jun ) Kristiansund Nothing 1.599 ""
    , Image "DSCF2620.jpg" ( 2014, Jun ) Kristiansund Nothing 1.333 ""
    , Image "P6145636.jpg" ( 2014, Jun ) Kristiansund Nothing 1.679 ""
    , Image "P6135626.jpg" ( 2014, Jun ) Kristiansund Nothing 1.191 "Zita!"
    , Image "DSCF2634.jpg" ( 2014, Jun ) Kristiansund Nothing 0.75 "We had to crawl through a set of caves bored out over eons by this waterfall to take this shot."
    , Image "DSCF2613.jpg" ( 2014, Jun ) Kristiansund Nothing 2.179 "A part of the Atlantic Ocean Road. Search google for some aerial photos of this place - utterly amazing."
    , Image "DSCF2627.jpg" ( 2014, Jun ) Kristiansund Nothing 1.404 ""
    , Image "P6135629.jpg" ( 2014, Jun ) Kristiansund Nothing 0.965 ""
    , Image "DSCF2622.jpg" ( 2014, Jun ) Kristiansund Nothing 1.333 ""
    , Image "IMG_20140606_184606.jpg" ( 2014, Jun ) Copenhagen Nothing 0.871 ""
    , Image "DSCF2578.jpg" ( 2014, Jun ) Roskilde Nothing 1.333 ""
    , Image "DSCF2569.jpg" ( 2014, Jun ) Roskilde Nothing 1.333 ""
    , Image "DSCF2526.jpg" ( 2014, Jun ) Roskilde Nothing 1.333 ""
    , Image "DSCF2563.jpg" ( 2014, Jun ) Roskilde Nothing 1.333 ""
    , Image "DSCF2556.jpg" ( 2014, Jun ) Roskilde Nothing 1.333 "Rowing a replica viking boat"
    , Image "DSCF2546.jpg" ( 2014, Jun ) Roskilde Nothing 1.333 ""
    , Image "DSCF2583.jpg" ( 2014, Jun ) Roskilde Nothing 1.175 ""
    , Image "DSCF2821.jpg" ( 2014, Jun ) Stockholm Nothing 1.333 "The Vasa."
    , Image "DSCF2861.jpg" ( 2014, Jun ) Stockholm Nothing 1.585 ""
    , Image "DSCF2846.jpg" ( 2014, Jun ) Stockholm Nothing 1.333 "A warship built by the Swedes in 1626-1628. Sank 20 minutes into its maiden voyage because it was a 7 story behemoth that was way too tall. Salvaged in 1961 and is 98% original."
    , Image "DSCF2774.jpg" ( 2014, Jun ) Stockholm Nothing 1.245 ""
    , Image "DSCF2899.jpg" ( 2014, Jun ) Stockholm Nothing 1.333 ""
    , Image "DSCF2863.jpg" ( 2014, Jun ) Stockholm Nothing 1.595 ""
    , Image "DSCF2920.jpg" ( 2014, Jun ) Stockholm Nothing 2.09 ""
    , Image "DSCF2798.jpg" ( 2014, Jun ) Stockholm Nothing 1.483 ""
    , Image "DSCF2799.jpg" ( 2014, Jun ) Stockholm Nothing 1.604 ""
    , Image "DSCF3284.jpg" ( 2014, Jun ) Pushkin Nothing 1.266 ""
    , Image "DSCF3234.jpg" ( 2014, Jun ) Pushkin Nothing 1.333 ""
    , Image "DSCF3322.jpg" ( 2014, Jun ) Pushkin Nothing 1.212 ""
    , Image "DSCF3289.jpg" ( 2014, Jun ) Pushkin Nothing 0.981 ""
    , Image "DSCF3314.jpg" ( 2014, Jun ) Pushkin Nothing 1.188 ""
    , Image "DSCF3306.jpg" ( 2014, Jun ) Pushkin Nothing 1.333 ""
    , Image "DSCF3177.jpg" ( 2014, Jun ) Petergof Nothing 1.379 "'Spared no expense'"
    , Image "DSCF3204.jpg" ( 2014, Jun ) Petergof Nothing 1.43 ""
    , Image "DSCF3192.jpg" ( 2014, Jun ) Petergof Nothing 0.75 ""
    , Image "DSCF3237.jpg" ( 2014, Jun ) Petergof Nothing 1.564 ""
    , Image "DSCF3171.jpg" ( 2014, Jun ) Petergof Nothing 1.333 ""
    , Image "DSCF3140.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.346 ""
    , Image "DSCF3102.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.333 ""
    , Image "DSCF3327.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.333 "Most beautiful Lenin."
    , Image "DSCF3328.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.029 "Hanging out with all the Germans; 'Who is this guy? Why are there flowers at his feet?' 'He's the general that lead to the death of over a million German conscripts and drove us out of Russia.' 'Oh...'"
    , Image "DSCF3106.jpg" ( 2014, Jun ) SaintPetersburg Nothing 0.977 ""
    , Image "DSCF2930.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.31 ""
    , Image "DSCF2937.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.589 ""
    , Image "DSCF3071.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.284 ""
    , Image "DSCF2949.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.029 ""
    , Image "DSCF3055.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.852 ""
    , Image "DSCF2977.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.577 ""
    , Image "DSCF3122.jpg" ( 2014, Jun ) SaintPetersburg Nothing 0.588 ""
    , Image "DSCF3128.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.702 ""
    , Image "DSCF2941.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.589 "The Hermitage is perhaps the biggest collection of important objects anywhere in the world. The Louvre hasn't got shit on this place."
    , Image "DSCF3051.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.333 ""
    , Image "DSCF2943.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.333 ""
    , Image "DSCF3154.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.467 "Kazan Cathedral"
    , Image "DSCF3028.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.195 "The pilar is 50 tonnes of solid marble, so it's not actually attached to anything; but it's certainly not going anywhere either."
    , Image "DSCF3030.jpg" ( 2014, Jun ) SaintPetersburg Nothing 0.709 ""
    , Image "DSCF3046.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.587 ""
    , Image "DSCF2935.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.311 ""
    , Image "DSCF3120.jpg" ( 2014, Jun ) SaintPetersburg Nothing 2.793 ""
    , Image "DSCF2996.jpg" ( 2014, Jun ) SaintPetersburg Nothing 0.75 ""
    , Image "DSCF3150.jpg" ( 2014, Jun ) SaintPetersburg Nothing 1.333 ""
    , Image "DSC_1431.jpg" ( 2017, Aug ) Copenhagen Nothing 0.562 ""
    , Image "DSC_1430.jpg" ( 2017, Aug ) Copenhagen Nothing 0.692 ""
    , Image "DSC_1434.jpg" ( 2017, Aug ) Gothenburg Nothing 1.778 ""
    , Image "DSC_1435.jpg" ( 2017, Aug ) Gothenburg Nothing 1.778 ""
    , Image "DSC_1442.jpg" ( 2017, Aug ) Gothenburg Nothing 0.752 ""
    , Image "DSC_1460.jpg" ( 2017, Aug ) Gothenburg Nothing 1.778 ""
    , Image "DSC_1457.jpg" ( 2017, Aug ) Gothenburg Nothing 1.778 ""
    , Image "DSC_1455.jpg" ( 2017, Aug ) Gothenburg Nothing 1.622 ""
    , Image "DSC_1440.jpg" ( 2017, Aug ) Gothenburg Nothing 0.562 ""
    , Image "DSC_1438.jpg" ( 2017, Aug ) Gothenburg Nothing 1.609 ""
    , Image "DSC_1464.jpg" ( 2017, Aug ) Gothenburg Nothing 1.992 ""
    , Image "DSC_0086.jpg" ( 2017, Mar ) Umea Nothing 1.778 ""
    , Image "DSC_0092.jpg" ( 2017, Mar ) Umea Nothing 1.476 ""
    , Image "DSC_0096.jpg" ( 2017, Mar ) Umea Nothing 1.778 ""
    , Image "DSC_0099.jpg" ( 2017, Mar ) Umea Nothing 2.11 ""
    , Image "DSC_0108.jpg" ( 2017, Mar ) Umea Nothing 1.778 ""
    , Image "DSC_1564.jpg" ( 2017, Dec ) Trollhattan Nothing 1.287 ""
    , Image "DSC_1587.jpg" ( 2017, Dec ) Gothenburg Nothing 1.592 ""
    , Image "DSC_1614.jpg" ( 2017, Dec ) Gothenburg Nothing 1.692 ""
    , Image "DSC_1561.jpg" ( 2017, Dec ) Gothenburg Nothing 2.247 ""
    , Image "20171229_090421.jpg" ( 2017, Dec ) Are Nothing 0.701 ""
    , Image "DSC_1642.jpg" ( 2017, Dec ) Are Nothing 1.778 ""
    , Image "DSC_1649.jpg" ( 2017, Dec ) Are Nothing 1.778 ""
    , Image "DSC_1630.jpg" ( 2017, Dec ) Are Nothing 1.617 ""
    , Image "DSC_1627.jpg" ( 2017, Dec ) Are Nothing 1.555 ""
    , Image "DSC_1650.jpg" ( 2017, Dec ) Are Nothing 1.778 ""
    , Image "DSC_1634.jpg" ( 2017, Dec ) Are Nothing 1.778 ""
    , Image "DSC_1641.jpg" ( 2017, Dec ) Are Nothing 0.562 ""
    , Image "20171229_091333.jpg" ( 2017, Dec ) Are Nothing 1.271 ""
    , Image "DSC_1661.jpg" ( 2017, Dec ) Ostersund Nothing 1.778 ""
    , Image "20171230_132522.jpg" ( 2017, Dec ) Ostersund Nothing 0.768 ""
    , Image "DSC_1669.jpg" ( 2017, Dec ) Ostersund Nothing 2.157 ""
    , Image "DSC_1660.jpg" ( 2017, Dec ) Ostersund Nothing 1.778 ""
    , Image "20171230_134614.jpg" ( 2017, Dec ) Ostersund Nothing 0.75 ""
    , Image "DSC_1664.jpg" ( 2017, Dec ) Ostersund Nothing 0.562 ""
    , Image "20171230_134130.jpg" ( 2017, Dec ) Ostersund Nothing 0.75 ""
    , Image "DSC_1319.jpg" ( 2017, Jul ) Vidoy Nothing 1.705 ""
    , Image "DSC_1172.jpg" ( 2017, Jul ) Eysturoy Nothing 1.723 ""
    , Image "DSC_1187.jpg" ( 2017, Jul ) Eysturoy Nothing 1.671 ""
    , Image "DSC_1180.jpg" ( 2017, Jul ) Eysturoy Nothing 0.607 ""
    , Image "DSC_1191.jpg" ( 2017, Jul ) Eysturoy Nothing 1.702 ""
    , Image "DSC_1176.jpg" ( 2017, Jul ) Eysturoy Nothing 0.562 ""
    , Image "DSC_1171.jpg" ( 2017, Jul ) Eysturoy Nothing 1.94 ""
    , Image "DSC_1183.jpg" ( 2017, Jul ) Eysturoy Nothing 0.562 ""
    , Image "DSC_1174.jpg" ( 2017, Jul ) Eysturoy Nothing 1.778 ""
    , Image "DSC_1307.jpg" ( 2017, Jul ) Bordoy Nothing 1.778 ""
    , Image "DSC_1308.jpg" ( 2017, Jul ) Bordoy Nothing 1.97 ""
    , Image "DSC_1126.jpg" ( 2017, Jul ) Streymoy Nothing 1.107 ""
    , Image "DSC_1160.jpg" ( 2017, Jul ) Streymoy Nothing 1.778 ""
    , Image "DSC_1159.jpg" ( 2017, Jul ) Streymoy Nothing 0.561 ""
    , Image "DSC_1166.jpg" ( 2017, Jul ) Streymoy Nothing 0.562 ""
    , Image "DSC_1154.jpg" ( 2017, Jul ) Streymoy Nothing 1.778 ""
    , Image "DSC_1137.jpg" ( 2017, Jul ) Streymoy Nothing 1.974 ""
    , Image "DSC_1367.jpg" ( 2017, Jul ) Hestur Nothing 0.637 ""
    , Image "DSC_1417.jpg" ( 2017, Jul ) Hestur Nothing 1.778 ""
    , Image "DSC_1402.jpg" ( 2017, Jul ) Hestur Nothing 0.606 ""
    , Image "DSC_1426.jpg" ( 2017, Jul ) Hestur Nothing 1.42 ""
    , Image "DSC_1383.jpg" ( 2017, Jul ) Hestur Nothing 1.881 ""
    , Image "DSC_1397.jpg" ( 2017, Jul ) Hestur Nothing 1.992 ""
    , Image "DSC_1375.jpg" ( 2017, Jul ) Hestur Nothing 1.778 ""
    , Image "DSC_1363.jpg" ( 2017, Jul ) Hestur Nothing 1.923 ""
    , Image "DSC_1377.jpg" ( 2017, Jul ) Hestur Nothing 0.59 ""
    , Image "DSC_1411.jpg" ( 2017, Jul ) Hestur Nothing 0.491 ""
    , Image "DSC_1387.jpg" ( 2017, Jul ) Hestur Nothing 0.702 ""
    , Image "DSC_1218.jpg" ( 2017, Jul ) Vagar Nothing 0.562 ""
    , Image "DSC_1213.jpg" ( 2017, Jul ) Vagar Nothing 0.676 ""
    , Image "DSC_1076.jpg" ( 2017, Jul ) Vagar Nothing 1.778 ""
    , Image "DSC_1203.jpg" ( 2017, Jul ) Vagar Nothing 1.961 ""
    , Image "DSC_1085.jpg" ( 2017, Jul ) Vagar Nothing 0.562 ""
    , Image "DSC_1326.jpg" ( 2017, Jul ) Vagar Nothing 1.481 ""
    , Image "DSC_1044.jpg" ( 2017, Jul ) Vagar Nothing 2.004 ""
    , Image "DSC_1084.jpg" ( 2017, Jul ) Vagar Nothing 1.778 ""
    , Image "DSC_1284.jpg" ( 2017, Jul ) Vagar Nothing 1.778 ""
    , Image "DSC_1089.jpg" ( 2017, Jul ) Vagar Nothing 1.778 ""
    , Image "DSC_1042.jpg" ( 2017, Jul ) Vagar Nothing 1.969 ""
    , Image "DSC_1061.jpg" ( 2017, Jul ) Vagar Nothing 1.957 ""
    , Image "DSC_1066.jpg" ( 2017, Jul ) Vagar Nothing 1.759 ""
    , Image "DSC_1048.jpg" ( 2017, Jul ) Vagar Nothing 1.652 ""
    , Image "DSC_1289.jpg" ( 2017, Jul ) Vagar Nothing 0.562 ""
    , Image "DSC_1221.jpg" ( 2017, Jul ) Vagar Nothing 0.562 ""
    , Image "DSC_1195.jpg" ( 2017, Jul ) Vagar Nothing 2.538 ""
    , Image "DSC_1104.jpg" ( 2017, Jul ) Vagar Nothing 1.883 ""
    , Image "DSC_1223.jpg" ( 2017, Jul ) Vagar Nothing 2.119 ""
    , Image "DSC_1060.jpg" ( 2017, Jul ) Vagar Nothing 0.562 ""
    , Image "DSC_1064.jpg" ( 2017, Jul ) Vagar Nothing 1.901 ""
    , Image "DSC_1081.jpg" ( 2017, Jul ) Vagar Nothing 1.778 ""
    , Image "DSC_1037.jpg" ( 2017, Jul ) Vagar Nothing 1.739 ""
    , Image "DSC_1329.jpg" ( 2017, Jul ) Vagar Nothing 1.13 ""
    , Image "DSC_1201.jpg" ( 2017, Jul ) Vagar Nothing 1.339 ""
    , Image "DSC_1249.jpg" ( 2017, Jul ) Torshavn Nothing 2.035 ""
    , Image "DSC_1242.jpg" ( 2017, Jul ) Torshavn Nothing 0.587 ""
    , Image "DSC_1255.jpg" ( 2017, Jul ) Torshavn Nothing 0.702 ""
    , Image "DSC_1264.jpg" ( 2017, Jul ) Torshavn Nothing 1.778 ""
    , Image "DSC_1260.jpg" ( 2017, Jul ) Torshavn Nothing 0.74 ""
    , Image "DSC_1333.jpg" ( 2017, Jul ) Torshavn Nothing 0.652 ""
    , Image "DSC_1263.jpg" ( 2017, Jul ) Torshavn Nothing 0.721 ""
    , Image "DSC_1276.jpg" ( 2017, Jul ) Torshavn Nothing 0.575 ""
    , Image "DSC_1273.jpg" ( 2017, Jul ) Torshavn Nothing 1.778 ""
    , Image "DSC_1272.jpg" ( 2017, Jul ) Torshavn Nothing 1.413 ""
    , Image "DSC_0922.jpg" ( 2017, Jul ) Fjaerland Nothing 0.562 ""
    , Image "DSC_0896.jpg" ( 2017, Jul ) Fjaerland Nothing 1.778 ""
    , Image "DSC_0893.jpg" ( 2017, Jul ) Fjaerland Nothing 1.778 ""
    , Image "DSC_0905.jpg" ( 2017, Jul ) Fjaerland Nothing 0.618 ""
    , Image "DSC_0917.jpg" ( 2017, Jul ) Fjaerland Nothing 1.778 ""
    , Image "DSC_0901.jpg" ( 2017, Jul ) Fjaerland Nothing 1.778 ""
    , Image "DSC_0928.jpg" ( 2017, Jul ) Fjaerland Nothing 1.898 ""
    , Image "DSC_0855.jpg" ( 2017, Jul ) Flam Nothing 0.562 ""
    , Image "DSC_0834.jpg" ( 2017, Jul ) Flam Nothing 0.562 ""
    , Image "DSC_0837.jpg" ( 2017, Jul ) Flam Nothing 2.024 ""
    , Image "DSC_0841.jpg" ( 2017, Jul ) Flam Nothing 1.778 ""
    , Image "DSC_0850.jpg" ( 2017, Jul ) Flam Nothing 0.678 ""
    , Image "DSC_0854.jpg" ( 2017, Jul ) Flam Nothing 0.583 ""
    , Image "DSC_0848.jpg" ( 2017, Jul ) Flam Nothing 0.562 ""
    , Image "DSC_0999.jpg" ( 2017, Jul ) Bergen Nothing 1.609 ""
    , Image "DSC_0935.jpg" ( 2017, Jul ) Bergen Nothing 2.212 ""
    , Image "DSC_0991.jpg" ( 2017, Jul ) Bergen Nothing 1.963 ""
    , Image "DSC_1005.jpg" ( 2017, Jul ) Bergen Nothing 1.671 ""
    , Image "DSC_0995.jpg" ( 2017, Jul ) Bergen Nothing 2.312 ""
    , Image "DSC_0943.jpg" ( 2017, Jul ) Bergen Nothing 1.778 ""
    , Image "DSC_0975.jpg" ( 2017, Jul ) Bergen Nothing 0.606 ""
    , Image "DSC_0960.jpg" ( 2017, Jul ) Bergen Nothing 2.051 ""
    , Image "DSC_1003.jpg" ( 2017, Jul ) Bergen Nothing 0.603 ""
    , Image "DSC_0986.jpg" ( 2017, Jul ) Bergen Nothing 1.778 ""
    , Image "DSC_0944.jpg" ( 2017, Jul ) Bergen Nothing 0.7 ""
    , Image "DSC_0978.jpg" ( 2017, Jul ) Bergen Nothing 1.674 ""
    , Image "DSC_0863.jpg" ( 2017, Jul ) Balestrand Nothing 0.562 ""
    , Image "DSC_0879.jpg" ( 2017, Jul ) Balestrand Nothing 1.778 ""
    , Image "DSC_0887.jpg" ( 2017, Jul ) Balestrand Nothing 0.696 ""
    , Image "DSC_0877.jpg" ( 2017, Jul ) Balestrand Nothing 0.618 ""
    , Image "DSC_0885.jpg" ( 2017, Jul ) Balestrand Nothing 1.778 ""
    , Image "DSC_0861.jpg" ( 2017, Jul ) Balestrand Nothing 1.998 ""
    , Image "DSC_0875.jpg" ( 2017, Jul ) Balestrand Nothing 1.778 ""
    , Image "DSC_0881.jpg" ( 2017, Jul ) Balestrand Nothing 1.778 ""
    , Image "DSC_0813.jpg" ( 2017, Jul ) Oslo Nothing 1.086 ""
    , Image "DSC_0824.jpg" ( 2017, Jul ) Oslo Nothing 0.588 ""
    , Image "DSC_0820.jpg" ( 2017, Jul ) Oslo Nothing 1.817 ""
    , Image "DSC_0816.jpg" ( 2017, Jul ) Oslo Nothing 1.795 ""
    , Image "DSC_0829.jpg" ( 2017, Jul ) Oslo Nothing 0.67 ""
    , Image "DSC_0817.jpg" ( 2017, Jul ) Oslo Nothing 0.556 ""
    , Image "DSC_0799.jpg" ( 2017, Jul ) Stockholm Nothing 1.988 ""
    , Image "DSC_0804.jpg" ( 2017, Jul ) Stockholm Nothing 1.637 ""
    , Image "DSC_0800.jpg" ( 2017, Jul ) Stockholm Nothing 1.778 ""
    , Image "DSC_0796.jpg" ( 2017, Jul ) Stockholm Nothing 1.778 ""
    , Image "DSC_0797.jpg" ( 2017, Jul ) Stockholm Nothing 0.594 ""
    , Image "DSC_0047.jpg" ( 2017, Jan ) Are Nothing 1.778 ""
    , Image "DSC_0011.jpg" ( 2017, Jan ) Are Nothing 2.016 ""
    , Image "DSC_0066.jpg" ( 2017, Jan ) Are Nothing 1.778 ""
    , Image "DSC_0027.jpg" ( 2017, Jan ) Are Nothing 2.312 ""
    , Image "DSC_0067.jpg" ( 2017, Jan ) Are Nothing 2.488 ""
    , Image "DSC_0062.jpg" ( 2017, Jan ) Are Nothing 2.167 ""
    , Image "DSC_0068.jpg" ( 2017, Jan ) Are Nothing 2.051 ""
    , Image "DSC_0052.jpg" ( 2017, Jan ) Are Nothing 1.258 ""
    , Image "DSC_0045.jpg" ( 2017, Jan ) Are Nothing 1.778 ""
    , Image "DSC_0072.jpg" ( 2017, Jan ) Are Nothing 1.778 ""
    , Image "DSC_0054.jpg" ( 2017, Jan ) Are Nothing 1.778 ""
    , Image "DSC_0018.jpg" ( 2017, Jan ) Are Nothing 1.742 ""
    , Image "DSC_0051.jpg" ( 2017, Jan ) Are Nothing 1.778 ""
    , Image "DSC_0076.jpg" ( 2017, Feb ) Trollhattan Nothing 1.778 ""
    , Image "DSC_1476.jpg" ( 2017, Sep ) Gothenburg Nothing 1.778 ""
    , Image "DSC_1489.jpg" ( 2017, Sep ) Gothenburg Nothing 0.562 ""
    , Image "DSC_1478.jpg" ( 2017, Sep ) Gothenburg Nothing 1.72 ""
    , Image "DSC_1487.jpg" ( 2017, Sep ) Gothenburg Nothing 1.778 ""
    , Image "DSC_1474.jpg" ( 2017, Sep ) Gothenburg Nothing 1.161 ""
    , Image "DSC_0129.jpg" ( 2017, Apr ) Gothenburg Nothing 1.693 ""
    , Image "DSC_0135.jpg" ( 2017, Apr ) Gothenburg Nothing 1.778 ""
    , Image "DSC_0140.jpg" ( 2017, Apr ) Gothenburg Nothing 1.778 ""
    , Image "DSC_0125.jpg" ( 2017, Apr ) Gothenburg Nothing 1.778 ""
    , Image "DSC_0136.jpg" ( 2017, Apr ) Gothenburg Nothing 2.004 ""
    , Image "DSC_1539.jpg" ( 2017, Oct ) Helsingborg Nothing 1.668 ""
    , Image "DSC_1538.jpg" ( 2017, Oct ) Helsingborg Nothing 1.558 ""
    , Image "DSC_0764.jpg" ( 2017, May ) Trollhattan Nothing 1.778 ""
    , Image "DSC_0760.jpg" ( 2017, May ) Kinnekulle Nothing 0.692 ""
    , Image "DSC_0746.jpg" ( 2017, May ) Kinnekulle Nothing 1.778 ""
    , Image "DSC_0761.jpg" ( 2017, May ) Kinnekulle Nothing 0.562 ""
    , Image "DSC_0739.jpg" ( 2017, May ) Kinnekulle Nothing 1.779 ""
    , Image "DSC_0742.jpg" ( 2017, May ) Kinnekulle Nothing 1.778 ""
    , Image "DSC_0757.jpg" ( 2017, May ) Kinnekulle Nothing 1.867 ""
    , Image "DSC_0754.jpg" ( 2017, May ) Kinnekulle Nothing 1.784 ""
    , Image "DSC_0740.jpg" ( 2017, May ) Kinnekulle Nothing 2.265 ""
    , Image "DSC_0759.jpg" ( 2017, May ) Kinnekulle Nothing 1.996 ""
    , Image "DSC_0686.jpg" ( 2017, May ) Chernobyl Nothing 2.27 ""
    , Image "DSC_0676.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0632.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0401.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0413.jpg" ( 2017, May ) Chernobyl Nothing 1.134 ""
    , Image "DSC_0638.jpg" ( 2017, May ) Chernobyl Nothing 0.652 ""
    , Image "DSC_0711.jpg" ( 2017, May ) Chernobyl Nothing 0.678 ""
    , Image "DSC_0423.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0419.jpg" ( 2017, May ) Chernobyl Nothing 1.426 ""
    , Image "DSC_0593.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0657.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0725.jpg" ( 2017, May ) Chernobyl Nothing 0.562 "For a while I couldn't work out what this was. Scavangers have torn the copper right out of the walls."
    , Image "DSC_0412.jpg" ( 2017, May ) Chernobyl Nothing 1.385 "All of the towns that used to exist, but are now permanently evacuated."
    , Image "DSC_0724.jpg" ( 2017, May ) Chernobyl Nothing 1.778 "I remember the serenity here. Wildflowers, butterfies. It was so warm and homely. Then I went inside and my radiation alarm went crazy."
    , Image "DSC_0714.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0670.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0694.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0684.jpg" ( 2017, May ) Chernobyl Nothing 1.778 "An actual summer camp. Each hut has Russian cartoon characters painted on it."
    , Image "DSC_0721.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0704.jpg" ( 2017, May ) Chernobyl Nothing 2.11 ""
    , Image "DSC_0635.jpg" ( 2017, May ) Chernobyl Nothing 1.754 ""
    , Image "DSC_0600.jpg" ( 2017, May ) Chernobyl Nothing 1.635 ""
    , Image "DSC_0696.jpg" ( 2017, May ) Chernobyl Nothing 1.778 "Birds were catching thermals in this unfinished cooling tower. A girl was singing old babushka songs under the exceptional acoustics. It was great."
    , Image "DSC_0703.jpg" ( 2017, May ) Chernobyl Nothing 1.992 ""
    , Image "DSC_0687.jpg" ( 2017, May ) Chernobyl Nothing 1.94 ""
    , Image "DSC_0701.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0653.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0599.jpg" ( 2017, May ) Chernobyl Nothing 1.612 ""
    , Image "DSC_0650.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0693.jpg" ( 2017, May ) Chernobyl Nothing 1.344 ""
    , Image "DSC_0409.jpg" ( 2017, May ) Chernobyl Nothing 1.942 ""
    , Image "DSC_0664.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0417.jpg" ( 2017, May ) Chernobyl Nothing 1.33 "Most Beautiful Lennin."
    , Image "DSC_0718.jpg" ( 2017, May ) Chernobyl Nothing 0.715 ""
    , Image "DSC_0624.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0660.jpg" ( 2017, May ) Chernobyl Nothing 0.562 "The road to the school childrens' summer camp aka Chernobyl 2: the secret military base. The road is constructed from concrete blocks to keep the children safe aka support tanks and heavy military vehicles."
    , Image "DSC_0406.jpg" ( 2017, May ) Chernobyl Nothing 1.335 ""
    , Image "DSC_0699.jpg" ( 2017, May ) Chernobyl Nothing 1.919 ""
    , Image "DSC_0609.jpg" ( 2017, May ) Chernobyl Nothing 2.096 ""
    , Image "DSC_0626.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0436.jpg" ( 2017, May ) Chernobyl Nothing 1.778 "The new sarcophagus. Largest arch in the world, largest moving stucture in the world. Built to withstand 100 years of radiation. The site needs to be covered for longer than the pyramids have existed."
    , Image "DSC_0666.jpg" ( 2017, May ) Chernobyl Nothing 1.778 "Believe it or not, this is the main street of this town."
    , Image "DSC_0623.jpg" ( 2017, May ) Chernobyl Nothing 0.462 ""
    , Image "DSC_0411.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0671.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0628.jpg" ( 2017, May ) Chernobyl Nothing 2.103 ""
    , Image "DSC_0636.jpg" ( 2017, May ) Chernobyl Nothing 1.778 ""
    , Image "DSC_0629.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0717.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0709.jpg" ( 2017, May ) Chernobyl Nothing 0.626 "You can't touch any of the dogs because they roll in the dirt and get radioactive particles in their fur. Touch them and you probably die, don't touch them and you die on the inside."
    , Image "DSC_0647.jpg" ( 2017, May ) Chernobyl Nothing 1.676 ""
    , Image "DSC_0640.jpg" ( 2017, May ) Chernobyl Nothing 0.617 ""
    , Image "DSC_0433.jpg" ( 2017, May ) Chernobyl Nothing 0.562 ""
    , Image "DSC_0726.jpg" ( 2017, May ) Chernobyl Nothing 1.589 ""
    , Image "DSC_0408.jpg" ( 2017, May ) Chernobyl Nothing 2.653 ""
    , Image "DSC_0524.jpg" ( 2017, May ) Pripyat Nothing 0.589 ""
    , Image "DSC_0472.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0583.jpg" ( 2017, May ) Pripyat Nothing 0.662 ""
    , Image "DSC_0505.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0554.jpg" ( 2017, May ) Pripyat Nothing 2.039 ""
    , Image "DSC_0476.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0506.jpg" ( 2017, May ) Pripyat Nothing 0.894 ""
    , Image "DSC_0580.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0480.jpg" ( 2017, May ) Pripyat Nothing 2.331 "The carnival was set to open just 4 days after the disaster happened. It was never used."
    , Image "DSC_0454.jpg" ( 2017, May ) Pripyat Nothing 1.715 ""
    , Image "DSC_0456.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0442.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0486.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0555.jpg" ( 2017, May ) Pripyat Nothing 2.336 ""
    , Image "DSC_0479.jpg" ( 2017, May ) Pripyat Nothing 1.908 ""
    , Image "DSC_0586.jpg" ( 2017, May ) Pripyat Nothing 1.723 ""
    , Image "DSC_0556.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0497.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0484.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0546.jpg" ( 2017, May ) Pripyat Nothing 2.035 ""
    , Image "DSC_0559.jpg" ( 2017, May ) Pripyat Nothing 2.242 ""
    , Image "DSC_0551.jpg" ( 2017, May ) Pripyat Nothing 1.778 "By far the highlight of the trip was climbing up to the top of this 16 storey building and seeing the town."
    , Image "DSC_0566.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0576.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0473.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0451.jpg" ( 2017, May ) Pripyat Nothing 1.537 ""
    , Image "DSC_0441.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0519.jpg" ( 2017, May ) Pripyat Nothing 1.778 "No-one knows why all these are on the floor in the school..."
    , Image "DSC_0466.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0465.jpg" ( 2017, May ) Pripyat Nothing 1.778 ""
    , Image "DSC_0571.jpg" ( 2017, May ) Pripyat Nothing 0.678 "The cards on the floor here are arrest records in the police station. Mostly for intoxication."
    , Image "DSC_0437.jpg" ( 2017, May ) Pripyat Nothing 2.012 ""
    , Image "DSC_0582.jpg" ( 2017, May ) Pripyat Nothing 1.66 "Amazing sheet glass work still intact in a fancy cafe on the water."
    , Image "DSC_0528.jpg" ( 2017, May ) Pripyat Nothing 0.562 "Rooms and rooms of books on the floor, sometimes almost a meter deep."
    , Image "DSC_0510.jpg" ( 2017, May ) Pripyat Nothing 0.562 ""
    , Image "DSC_0577.jpg" ( 2017, May ) Pripyat Nothing 0.662 "A vending machine that has one glass cup you wash, select your drink (cherry or orange) and drink it right there, then put the cup back for the next person."
    , Image "DSC_0534.jpg" ( 2017, May ) Pripyat Nothing 0.609 ""
    , Image "DSC_0342.jpg" ( 2017, May ) Kiev Nothing 1.577 ""
    , Image "DSC_0222.jpg" ( 2017, May ) Kiev Nothing 2.073 "This used to be a symbol of friendship between Russia and Ukraine (the aluminium arch). Since that's not really happening much right now they decided to rebrand in the name of unity for all Ukrainians. Some people questioned how the LBGT community, which is a fraction of the population, represents Ukraine as a whole. The solution was to just stop the facelift and now it's going to just stay unfinished."
    , Image "DSC_0193.jpg" ( 2017, May ) Kiev Nothing 0.533 ""
    , Image "DSC_0361.jpg" ( 2017, May ) Kiev Nothing 1.778 "On the way up to the surface from the world's deepest subway."
    , Image "DSC_0263.jpg" ( 2017, May ) Kiev Nothing 1.934 ""
    , Image "DSC_0218.jpg" ( 2017, May ) Kiev Nothing 2.055 ""
    , Image "DSC_0217.jpg" ( 2017, May ) Kiev Nothing 1.365 ""
    , Image "DSC_0354.jpg" ( 2017, May ) Kiev Nothing 1.778 "This is a part of what's called 'The millionairs' ghetto'. Basically this was build for all the corrupt rich people to live in just before the revolution and susequent recession. Now no-one can afford to live here so it's mostly a fancy ghost town."
    , Image "DSC_0199.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0205.jpg" ( 2017, May ) Kiev Nothing 2.167 "The old palace, now the 'Corruption Museum' showcasing supercars and a private zoo."
    , Image "DSC_0223.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0344.jpg" ( 2017, May ) Kiev Nothing 1.733 ""
    , Image "DSC_0343.jpg" ( 2017, May ) Kiev Nothing 1.323 ""
    , Image "DSC_0376.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0730.jpg" ( 2017, May ) Kiev Nothing 0.562 ""
    , Image "DSC_0187.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0329.jpg" ( 2017, May ) Kiev Nothing 0.456 ""
    , Image "DSC_0221.jpg" ( 2017, May ) Kiev Nothing 2.227 ""
    , Image "DSC_0366.jpg" ( 2017, May ) Kiev Nothing 1.446 ""
    , Image "DSC_0312.jpg" ( 2017, May ) Kiev Nothing 2.172 ""
    , Image "DSC_0214.jpg" ( 2017, May ) Kiev Nothing 0.562 "The story goes that these guys lost each other during the Soviet Era and we reunited 35 years later and were still in love."
    , Image "DSC_0226.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0384.jpg" ( 2017, May ) Kiev Nothing 0.631 "She might not look like it here, but she's twice as tall as the statue of liberty."
    , Image "DSC_0340.jpg" ( 2017, May ) Kiev Nothing 1.733 "Some lady's business burnt down and the towns people gave her money to rebuild. Instead she brought this bronze statue of her cat that died in the fire."
    , Image "DSC_0327.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0264.jpg" ( 2017, May ) Kiev Nothing 2.0 ""
    , Image "DSC_0186.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0393.jpg" ( 2017, May ) Kiev Nothing 1.34 ""
    , Image "DSC_0379.jpg" ( 2017, May ) Kiev Nothing 1.399 ""
    , Image "DSC_0176.jpg" ( 2017, May ) Kiev Nothing 0.562 ""
    , Image "DSC_0224.jpg" ( 2017, May ) Kiev Nothing 1.778 "The security force at Eurovision was impressive."
    , Image "DSC_0352.jpg" ( 2017, May ) Kiev Nothing 0.562 ""
    , Image "DSC_0197.jpg" ( 2017, May ) Kiev Nothing 1.778 "A blockade from the 2012 revolution."
    , Image "DSC_0325.jpg" ( 2017, May ) Kiev Nothing 1.701 ""
    , Image "DSC_0370.jpg" ( 2017, May ) Kiev Nothing 0.562 ""
    , Image "DSC_0367.jpg" ( 2017, May ) Kiev Nothing 0.687 ""
    , Image "DSC_0333.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0362.jpg" ( 2017, May ) Kiev Nothing 1.927 ""
    , Image "DSC_0227.jpg" ( 2017, May ) Kiev Nothing 3.11 ""
    , Image "DSC_0319.jpg" ( 2017, May ) Kiev Nothing 1.778 ""
    , Image "DSC_0309.jpg" ( 2017, May ) Kiev Nothing 1.669 ""
    ]
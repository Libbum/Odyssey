#![recursion_limit = "256"]
extern crate failure;
extern crate globwalk;
extern crate image;
extern crate indicatif;
extern crate reqwest;
#[macro_use]
extern crate enum_derive;
#[macro_use]
extern crate macro_attr;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate serde_yaml;

use failure::Error;
use globwalk::DirEntry;
use image::FilterType::Lanczos3;
use image::GenericImageView;
use indicatif::{ProgressBar, ProgressStyle};
use reqwest::header::USER_AGENT;
use std::collections::BTreeMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::iter::FromIterator;
use std::process::Command;
use std::str::FromStr;
use std::{fmt, thread, time};

static NOMINATIM_ENDPOINT: &str = "http://nominatim.openstreetmap.org";
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Serialize, Deserialize)]
pub struct CountryCodes {
    #[serde(with = "codes")]
    pub codes: BTreeMap<String, String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct CountryCodeStruct {
    name: String,
    #[serde(rename = "alpha-3")]
    alpha3: String,
}

mod codes {
    use super::CountryCodeStruct;
    use std::collections::BTreeMap;

    use serde::de::{Deserialize, Deserializer};
    use serde::ser::Serializer;

    pub fn serialize<S>(map: &BTreeMap<String, String>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(map.values())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<BTreeMap<String, String>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let mut map = BTreeMap::new();
        for item in Vec::<CountryCodeStruct>::deserialize(deserializer)? {
            map.insert(item.name, item.alpha3);
        }
        Ok(map)
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    places: BTreeMap<Country, BTreeMap<Location, Option<String>>>,
    trips: Vec<Trip>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Trip {
    name: String,
    description: String,
    cities: Vec<Location>,
    dates: Vec<String>,
}

impl Trip {
    fn id_string(&self) -> String {
        let mut id = self.description.to_string();
        id.retain(|c| c != ' ' && c != '/');
        id
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct FeatureCollection {
    #[serde(rename = "type")]
    type_: String,
    features: Vec<Feature>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Feature {
    #[serde(rename = "type")]
    type_: String,
    properties: Properties,
    geometry: Geometry,
}

#[derive(Debug, Serialize, Deserialize)]
struct Properties {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    localname: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    country: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Geometry {
    #[serde(rename = "type")]
    type_: String,
    coordinates: Coordinates,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum Coordinates {
    Point(Vec<f32>),
    LineString(Vec<Vec<f32>>),
}

#[derive(Deserialize, Debug)]
struct LatLon {
    lat: String,
    lon: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct LocationInformation {
    id: Location,
    name: String,
    country: Country,
    coordinates: Vec<f32>,
}

fn get_query_string(params: Vec<(&str, &str)>) -> String {
    let pairs: Vec<String> = params
        .into_iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect();
    pairs.join("&")
}

fn search(place_name: &str) -> Result<LatLon, Error> {
    let params = vec![("format", "jsonv2"), ("q", place_name), ("limit", "1")];
    let query_string = get_query_string(params);
    let url = format!("{}/search?{}", NOMINATIM_ENDPOINT, query_string);
    let client = reqwest::Client::new();
    let mut res = client
        .get(&url)
        .header(USER_AGENT, format!("{} v{} - {}", NAME, VERSION, AUTHORS))
        .send()?;
    let mut results = res.json::<Vec<LatLon>>()?;
    results.reverse();
    let first = results.pop().ok_or_else(|| {
        failure::err_msg(format!(
            "Search for {} did not find coordinates",
            place_name
        ))
    })?;
    Ok(first)
}

fn to_location_identfier_string(from: &str) -> String {
    let mut identifier = from.clone().to_string();
    identifier.retain(|c| c != ' ' && c != '_');
    if identifier == "Singapore" || identifier == "HongKong" {
        identifier.push_str("City");
    }
    identifier
}

fn get_new_places(
    cities: &FeatureCollection,
    config: &Config,
    cca3: &BTreeMap<String, String>,
) -> (Vec<Country>, Vec<Location>) {
    let cities_country_codes = cities
        .features
        .iter()
        .map(|f| f.properties.country.clone().unwrap())
        .collect::<Vec<String>>();
    let mut cities_countries: Vec<String> = Vec::new();
    for c in cities_country_codes {
        for (name, code) in cca3 {
            if c == *code {
                let mut identifier = name.clone().to_string();
                identifier.retain(|c| c != ' ');
                cities_countries.push(identifier);
                break;
            }
        }
    }
    let config_countries = config
        .places
        .iter()
        .map(|(name, _)| name)
        .cloned()
        .collect::<Vec<Country>>();
    let mut new_countries: Vec<Country> = Vec::new();
    for ctry in config_countries {
        if !cities_countries.contains(&ctry.to_string()) {
            new_countries.push(ctry);
        }
    }
    let cities_locations = cities
        .features
        .iter()
        .map(|f| to_location_identfier_string(&f.properties.name))
        .collect::<Vec<String>>();
    let config_locations = config
        .places
        .iter()
        .map(|(_, locations)| {
            locations
                .iter()
                .map(|(name, _)| name)
                .cloned()
                .collect::<Vec<Location>>()
        })
        .flatten()
        .collect::<Vec<Location>>();
    let mut new_locations: Vec<Location> = Vec::new();
    for loc in config_locations {
        if !cities_locations.contains(&loc.to_string()) && loc != Location::Local {
            new_locations.push(loc);
        }
    }
    (new_countries, new_locations)
}

fn write_trip(config: &Config, features: &[Feature]) -> Result<(), Error> {
    let mut trip_features: Vec<Feature> = Vec::new();
    for trip in &config.trips {
        let properties = Properties {
            name: trip.name.clone(),
            localname: None,
            country: None,
        };
        let mut coords: Vec<Vec<f32>> = Vec::new();
        for city in &trip.cities {
            coords.push(city.feature_coordinates(&features)?);
        }
        let geometry = Geometry {
            type_: "LineString".to_string(),
            coordinates: Coordinates::LineString(coords),
        };
        trip_features.push(Feature {
            type_: "Feature".to_string(),
            properties,
            geometry,
        });
    }
    let trips = FeatureCollection {
        type_: "FeatureCollection".to_string(),
        features: trip_features,
    };
    let trips_buffer = File::create("world/trips.json")?;
    serde_json::to_writer(&trips_buffer, &trips)?;
    Ok(())
}

fn construct_world(
    config: &Config,
    cca3: &BTreeMap<String, String>,
) -> Result<Vec<LocationInformation>, Error> {
    let pause = time::Duration::from_secs(1);
    let cities_buffer = OpenOptions::new()
        .read(true)
        .write(true)
        .create(false) //Fail if we need to create the file
        .open("world/cities.json");

    let mut locations_details: Vec<LocationInformation> = Vec::new();
    match cities_buffer {
        Ok(buffer) => {
            // Add new info to cities.json
            let mut cities: FeatureCollection = serde_json::from_reader(&buffer)?;
            let (new_countries, new_locations) = get_new_places(&cities, config, cca3);
            for (country, locations) in &config.places {
                if new_countries.contains(&country) {
                    for (location, local_name) in
                        locations.iter().filter(|(l, _)| **l != Location::Local)
                    {
                        let coordinates = if new_locations.contains(location) {
                            let coords =
                                search(&format!("{}, {}", location.name(), country.name()))?;
                            thread::sleep(pause); //We can't hammer the Nominatim server.

                            let properties = Properties {
                                name: location.name(),
                                localname: local_name.to_owned(),
                                country: Some(country.code(&cca3)?),
                            };
                            let coordinates =
                                vec![coords.lon.parse::<f32>()?, coords.lat.parse::<f32>()?];
                            let geometry = Geometry {
                                type_: "Point".to_string(),
                                coordinates: Coordinates::Point(coordinates.clone()),
                            };

                            println!("{} {:?}", location.to_string(), geometry.coordinates);
                            cities.features.push(Feature {
                                type_: "Feature".to_string(),
                                properties,
                                geometry,
                            });
                            coordinates
                        } else {
                            location.feature_coordinates(&cities.features)?
                        };
                        locations_details.push(LocationInformation {
                            id: location.clone(),
                            name: location.name(),
                            country: country.clone(),
                            coordinates,
                        });
                    }
                } else {
                    for (location, _) in locations.iter().filter(|(l, _)| **l != Location::Local) {
                        let coordinates = location.feature_coordinates(&cities.features)?;
                        locations_details.push(LocationInformation {
                            id: location.clone(),
                            name: location.name(),
                            country: country.clone(),
                            coordinates,
                        });
                    }
                }
            }
            if new_countries.len() + new_locations.len() > 0 {
                serde_json::to_writer(&buffer, &cities)?;
            }
            //It's quicker to just redo the trips rather than checking them.
            write_trip(&config, &cities.features)?;
        }
        Err(_) => {
            // Create a new cities.json
            println!("No world/cities.json found, building one");
            let mut features: Vec<Feature> = Vec::new();
            for (country, locations) in &config.places {
                for (location, local_name) in
                    locations.iter().filter(|(l, _)| **l != Location::Local)
                {
                    let coords = search(&format!("{}, {}", location.name(), country.name()))?;
                    thread::sleep(pause); //We can't hammer the Nominatim server.

                    let properties = Properties {
                        name: location.name(),
                        localname: local_name.to_owned(),
                        country: Some(country.code(&cca3)?),
                    };
                    let coordinates = vec![coords.lon.parse::<f32>()?, coords.lat.parse::<f32>()?];
                    let geometry = Geometry {
                        type_: "Point".to_string(),
                        coordinates: Coordinates::Point(coordinates.clone()),
                    };

                    println!("{} {:?}", location.to_string(), geometry.coordinates);
                    features.push(Feature {
                        type_: "Feature".to_string(),
                        properties,
                        geometry,
                    });
                    locations_details.push(LocationInformation {
                        id: location.clone(),
                        name: location.name(),
                        country: country.clone(),
                        coordinates,
                    });
                }
            }
            //It's quicker to just redo the trips rather than checking them.
            write_trip(&config, &features)?;

            let cities = FeatureCollection {
                type_: "FeatureCollection".to_string(),
                features,
            };
            let cities_create = File::create("world/cities.json")?;
            serde_json::to_writer(&cities_create, &cities)?;
        }
    };

    println!("Building world.");
    Command::new("topojson")
        .arg("-o")
        .arg("../dist/assets/world.json")
        .arg("--id-property")
        .arg("su_a3")
        .arg("--properties")
        .arg("name,localname,country")
        .arg("--")
        .arg("world/countries.json")
        .arg("world/cities.json")
        .arg("world/trips.json")
        .status()?;
    Ok(locations_details)
}

fn construct_manifest(
    config: &Config,
    cca3: &BTreeMap<String, String>,
    locations_information: &[LocationInformation],
) -> Result<(), Error> {
    println!("Building Manifest.");
    let mut manifest = File::create("../src/Manifest.elm")?;
    writeln!(manifest, "module Manifest exposing (Country(..), Date, Image, Location(..), Month(..), Trip(..), Year, countryId, countryList, countryLocalName, countryName, locationInformation, locationList, locationLocalName, manifest, stringToCountry, stringToLocation, stringToTrip, tripInformation, tripList)")?;

    writeln!(manifest, "-- COUNTRIES")?;
    write_countries(&mut manifest, config, cca3)?;

    writeln!(manifest, "-- LOCATIONS")?;
    write_locations(&mut manifest, config, locations_information)?;

    writeln!(manifest, "-- TRIPS")?;
    write_trips(&mut manifest, config)?;

    writeln!(manifest, "-- MANIFEST")?;
    write_manifest(&mut manifest)?;

    Command::new("elm-format")
        .arg("--elm-version=0.19")
        .arg("--yes")
        .arg("../src/Manifest.elm")
        .status()?;
    Ok(())
}

fn write_countries(
    manifest: &mut File,
    config: &Config,
    cca3: &BTreeMap<String, String>,
) -> Result<(), Error> {
    writeln!(manifest, "type Country")?;
    let mut idx = 0;
    for (cntry, _) in &config.places {
        if idx != 0 {
            writeln!(manifest, "    | {}", cntry)?;
        } else {
            writeln!(manifest, "    = {}", cntry)?;
        }
        idx += 1;
    }

    writeln!(manifest, "countryList : List Country")?;
    writeln!(manifest, "countryList =")?;
    idx = 0;
    for (cntry, _) in &config.places {
        if idx != 0 {
            writeln!(manifest, "    , {}", cntry)?;
        } else {
            writeln!(manifest, "    [ {}", cntry)?;
        }
        idx += 1;
    }
    writeln!(manifest, "    ]")?;

    writeln!(manifest, "countryId : Country -> String")?;
    writeln!(manifest, "countryId country =")?;
    writeln!(manifest, "    case country of")?;
    for (cntry, _) in &config.places {
        writeln!(manifest, "        {} ->", cntry)?;
        writeln!(manifest, "            \"{}\"", cntry.code(&cca3)?)?;
    }

    writeln!(manifest, "countryName : Country -> String")?;
    writeln!(manifest, "countryName country =")?;
    writeln!(manifest, "    case country of")?;
    for (cntry, _) in &config.places {
        writeln!(manifest, "        {} ->", cntry)?;
        writeln!(manifest, "            \"{}\"", cntry.name())?;
    }

    writeln!(manifest, "stringToCountry : String -> Maybe Country")?;
    writeln!(manifest, "stringToCountry country =")?;
    writeln!(manifest, "    case country of")?;
    for (cntry, _) in &config.places {
        writeln!(manifest, "        \"{}\" ->", cntry.name())?;
        writeln!(manifest, "            Just {}", cntry)?;
    }
    writeln!(manifest, "        _ ->")?;
    writeln!(manifest, "            Nothing")?;

    writeln!(manifest, "countryLocalName : Country -> Maybe String")?;
    writeln!(manifest, "countryLocalName country =")?;
    writeln!(manifest, "    case country of")?;
    for (cntry, locations) in &config.places {
        if let Some(Some(local)) = locations.get(&Location::Local) {
            writeln!(manifest, "        {} ->", cntry)?;
            writeln!(manifest, "            Just \"{}\"", local)?;
        };
    }
    writeln!(manifest, "        _ ->")?;
    writeln!(manifest, "            Nothing")?;

    Ok(())
}

fn write_locations(
    manifest: &mut File,
    config: &Config,
    locations_information: &[LocationInformation],
) -> Result<(), Error> {
    let mut config_locations = config
        .places
        .iter()
        .map(|(_, locations)| {
            locations
                .iter()
                .map(|(name, loc)| (name.clone(), loc.clone()))
                .collect::<Vec<(Location, Option<String>)>>()
        })
        .flatten()
        .filter(|(l, _)| *l != Location::Local)
        .collect::<Vec<(Location, Option<String>)>>();
    config_locations.sort();
    writeln!(manifest, "type Location")?;
    let mut idx = 0;
    for (loc, _) in &config_locations {
        if idx != 0 {
            writeln!(manifest, "    | {}", loc)?;
        } else {
            writeln!(manifest, "    = {}", loc)?;
        }
        idx += 1;
    }

    writeln!(manifest, "locationList : List Location")?;
    writeln!(manifest, "locationList =")?;
    idx = 0;
    for (loc, _) in &config_locations {
        if idx != 0 {
            writeln!(manifest, "    , {}", loc)?;
        } else {
            writeln!(manifest, "    [ {}", loc)?;
        }
        idx += 1;
    }
    writeln!(manifest, "    ]")?;

    writeln!(manifest, "stringToLocation : String -> Maybe Location")?;
    writeln!(manifest, "stringToLocation location =")?;
    writeln!(manifest, "    case location of")?;
    for (loc, _) in &config_locations {
        writeln!(manifest, "        \"{}\" ->", loc.name())?;
        writeln!(manifest, "            Just {}", loc)?;
    }
    writeln!(manifest, "        _ ->")?;
    writeln!(manifest, "            Nothing")?;

    writeln!(manifest, "locationLocalName : Location -> Maybe String")?;
    writeln!(manifest, "locationLocalName location =")?;
    writeln!(manifest, "    case location of")?;
    for (loc, local_name) in &config_locations {
        if let Some(local) = local_name {
            writeln!(manifest, "        {} ->", loc)?;
            writeln!(manifest, "            Just \"{}\"", local)?;
        };
    }
    writeln!(manifest, "        _ ->")?;
    writeln!(manifest, "            Nothing")?;

    writeln!(manifest, "type alias LocationInformation =")?;
    writeln!(manifest, "    {{ name : String")?;
    writeln!(manifest, "    , country : Country")?;
    writeln!(manifest, "    , coordinates : ( Float, Float )")?;
    writeln!(manifest, "    }}")?;

    writeln!(
        manifest,
        "locationInformation : Location -> LocationInformation"
    )?;
    writeln!(manifest, "locationInformation location =")?;
    writeln!(manifest, "    case location of")?;
    for info in locations_information {
        let lon = info
            .coordinates
            .get(0)
            .ok_or_else(|| failure::err_msg("No longitude value in coordinates"))?;
        let lat = info
            .coordinates
            .get(1)
            .ok_or_else(|| failure::err_msg("No latitude value in coordinates"))?;
        writeln!(manifest, "    {} ->", info.id)?;
        writeln!(manifest, "    {{ name = \"{}\"", info.name)?;
        writeln!(manifest, "    , country = {}", info.country)?;
        writeln!(manifest, "    , coordinates = ( {:.3}, {:.3} )", lon, lat)?;
        writeln!(manifest, "    }}")?;
    }
    Ok(())
}

fn write_trips(manifest: &mut File, config: &Config) -> Result<(), Error> {
    writeln!(manifest, "type Trip")?;
    let mut idx = 0;
    for trip in &config.trips {
        if idx != 0 {
            writeln!(manifest, "    | {}", trip.id_string())?;
        } else {
            writeln!(manifest, "    = {}", trip.id_string())?;
        }
        idx += 1;
    }

    writeln!(manifest, "tripList : List Trip")?;
    writeln!(manifest, "tripList =")?;
    idx = 0;
    for trip in &config.trips {
        if idx != 0 {
            writeln!(manifest, "    , {}", trip.id_string())?;
        } else {
            writeln!(manifest, "    [ {}", trip.id_string())?;
        }
        idx += 1;
    }
    writeln!(manifest, "    ]")?;

    writeln!(manifest, "stringToTrip : String -> Maybe Trip")?;
    writeln!(manifest, "stringToTrip trip =")?;
    writeln!(manifest, "    case trip of")?;
    for trip in &config.trips {
        writeln!(manifest, "        \"{}\" ->", trip.description)?;
        writeln!(manifest, "            Just {}", trip.id_string())?;
    }
    writeln!(manifest, "        _ ->")?;
    writeln!(manifest, "            Nothing")?;

    writeln!(manifest, "type alias TripInformation =")?;
    writeln!(manifest, "    {{ name : String")?;
    writeln!(manifest, "    , description : String")?;
    writeln!(manifest, "    , locations : List Location")?;
    writeln!(manifest, "    , dates : List Date")?;
    writeln!(manifest, "    }}")?;

    writeln!(manifest, "tripInformation : Trip -> TripInformation")?;
    writeln!(manifest, "tripInformation trip =")?;
    writeln!(manifest, "    case trip of")?;
    for trip in &config.trips {
        writeln!(manifest, "        {} ->", trip.id_string())?;
        writeln!(manifest, "            {{ name = \"{}\"", trip.name)?;
        writeln!(
            manifest,
            "            , description = \"{}\"",
            trip.description
        )?;
        write!(manifest, "            , locations = [ ")?;
        idx = 0;
        for place in &trip.cities {
            if idx != 0 {
                write!(manifest, ", {}", place)?;
            } else {
                write!(manifest, "{}", place)?;
            }
            idx += 1;
        }
        writeln!(manifest, " ]")?;
        write!(manifest, "            , dates = [ ")?;
        idx = 0;
        for date in &trip.dates {
            let splitidx = date.find('/').ok_or_else(|| {
                failure::err_msg(format!("{} has a malformed date string", trip.id_string()))
            })?;
            let (year, month_str) = date.split_at(splitidx);
            let mut month_string = month_str.to_string();
            month_string.retain(|c| c != '/');
            if idx != 0 {
                write!(
                    manifest,
                    ", Date {} {}",
                    year,
                    Month::from_str(&month_string)?
                )?;
            } else {
                write!(
                    manifest,
                    "Date {} {}",
                    year,
                    Month::from_str(&month_string)?
                )?;
            }
            idx += 1;
        }
        writeln!(manifest, " ]")?;
        writeln!(manifest, "            }}")?;
    }

    // Extras, just to keep Date contained.
    writeln!(manifest, "type alias Year =")?;
    writeln!(manifest, "    Int")?;

    writeln!(manifest, "type Month")?;
    // No point in making Month an iterator.
    writeln!(manifest, "    = Jan")?;
    writeln!(manifest, "    | Feb")?;
    writeln!(manifest, "    | Mar")?;
    writeln!(manifest, "    | Apr")?;
    writeln!(manifest, "    | May")?;
    writeln!(manifest, "    | Jun")?;
    writeln!(manifest, "    | Jul")?;
    writeln!(manifest, "    | Aug")?;
    writeln!(manifest, "    | Sep")?;
    writeln!(manifest, "    | Oct")?;
    writeln!(manifest, "    | Nov")?;
    writeln!(manifest, "    | Dec")?;
    writeln!(manifest, "type alias Date =")?;
    writeln!(manifest, "    {{ year : Year")?;
    writeln!(manifest, "    , month : Month")?;
    writeln!(manifest, "    }}")?;

    Ok(())
}

fn write_manifest(manifest: &mut File) -> Result<(), Error> {
    // Ignore the thumbnails and blurs at this point. We will check for them later.
    let walker = globwalk::GlobWalkerBuilder::from_patterns(
        "../dist/gallery/",
        &["*.{png,jpg,jpeg,PNG,JPG,JPEG}", "!*_small*", "!*_blur*"],
    )
    .follow_links(true)
    .build()?
    .into_iter()
    .filter_map(Result::ok)
    .collect::<Vec<DirEntry>>();

    let progcount = walker.len() as u64;
    let bar = ProgressBar::new(progcount);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("[{elapsed_precise}] {bar:25.cyan/blue} {pos:>5}/{len:5} {msg}"),
    );

    writeln!(manifest, "type alias Image =")?;
    writeln!(manifest, "    {{ file : String")?;
    writeln!(manifest, "    , date : Date")?;
    writeln!(manifest, "    , location : Location")?;
    writeln!(manifest, "    , aspectRatio : Float")?;
    writeln!(manifest, "    , description : String")?;
    writeln!(manifest, "    }}")?;

    writeln!(manifest, "manifest : List Image")?;
    writeln!(manifest, "manifest =")?;

    for (idx, file) in bar.wrap_iter(walker.iter().enumerate()) {
        bar.set_message(
            &file
                .path()
                .strip_prefix("../dist/gallery/")?
                .to_str()
                .unwrap_or_default(),
        );
        // Open image and grab its dimensions.
        let img = image::open(&file.path())?;
        let (width, height) = img.dimensions();
        let ratio = width as f64 / height as f64;

        // Generate a thumbnail and blur if they doesn't already exist.
        let stem = file
            .path()
            .file_stem()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("File stem unwrap issue."))?;
        let ext = file
            .path()
            .extension()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Extension unwrap issue."))?;
        let thumbnail = format!("{}_small.{}", stem, ext);
        let blur = format!("{}_blur.{}", stem, ext);
        let thumb_width = if ratio < 3.0 { 500 } else { 900 };
        if !file.path().with_file_name(&thumbnail).exists()
            && !file.path().with_file_name(&blur).exists()
        {
            let thumb = img.resize(thumb_width, 500, Lanczos3);
            thumb.save(file.path().with_file_name(thumbnail))?;
            thumb.blur(30.0).save(file.path().with_file_name(blur))?;
        } else if !file.path().with_file_name(&thumbnail).exists() {
            img.resize(thumb_width, 500, Lanczos3)
                .save(file.path().with_file_name(thumbnail))?;
        } else {
            img.resize(thumb_width, 500, Lanczos3)
                .blur(30.0)
                .save(file.path().with_file_name(blur))?;
        }

        // Get image decription if it exists, create file if not.
        let mut description = String::new();
        let _ = File::open(file.path().with_extension("desc"))
            .or_else(|_| File::create(file.path().with_extension("desc")))
            .and_then(|mut f| f.read_to_string(&mut description));

        // Build a manifest of all files. We do this entirely each time as descriptions or filenames may have changed.
        let mut path_iter = file.path().strip_prefix("../dist/gallery/")?.iter().rev();

        let name = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("File name unwrap issue."))?;
        let location_str = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Location unwrap issue."))?;
        let location = to_location_identfier_string(&location_str).parse::<Location>()?;
        let _country = path_iter.next();
        let month = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Month unwrap issue."))?
            .parse::<Month>()?;
        let year = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Year unwrap issue."))?;

        if idx != 0 {
            write!(
                manifest,
                "    , Image \"{}\" (Date {} {:?}) {:?} {:.3} \"{}\"\n",
                name,
                year,
                month,
                location,
                ratio,
                description.trim()
            )?;
        } else {
            write!(
                manifest,
                "    [ Image \"{}\" (Date {} {:?}) {:?} {:.3} \"{}\"\n",
                name,
                year,
                month,
                location,
                ratio,
                description.trim()
            )?;
        }
    }
    writeln!(manifest, "    ]")?;
    bar.finish();

    Ok(())
}

fn main() -> Result<(), Error> {
    let config_file = File::open("odyssey.yaml")?;
    let config: Config = serde_yaml::from_reader(config_file)?;

    let cca3_file = File::open("world/cca3.json")?;
    let cca3_read: CountryCodes = serde_json::from_reader(cca3_file)?;
    let cca3 = &cca3_read.codes;

    let locations_information = construct_world(&config, &cca3)?;

    construct_manifest(&config, &cca3, &locations_information)?;

    println!("World and Manifest builds complete.");

    Ok(())
}

#[derive(Debug, Serialize, Deserialize)]
enum Month {
    Jan,
    Feb,
    Mar,
    Apr,
    May,
    Jun,
    Jul,
    Aug,
    Sep,
    Oct,
    Nov,
    Dec,
}

impl FromStr for Month {
    type Err = Error;

    fn from_str(s: &str) -> Result<Month, Error> {
        match s {
            "01" => Ok(Month::Jan),
            "02" => Ok(Month::Feb),
            "03" => Ok(Month::Mar),
            "04" => Ok(Month::Apr),
            "05" => Ok(Month::May),
            "06" => Ok(Month::Jun),
            "07" => Ok(Month::Jul),
            "08" => Ok(Month::Aug),
            "09" => Ok(Month::Sep),
            "10" => Ok(Month::Oct),
            "11" => Ok(Month::Nov),
            "12" => Ok(Month::Dec),
            err => Err(failure::err_msg(format!(
                "{} makes no sense to be a month.",
                err
            ))),
        }
    }
}

impl fmt::Display for Month {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

macro_attr! {
    #[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, EnumFromStr!, Serialize, Deserialize)]
    enum Country {
        Armenia,
        Australia,
        Austria,
        CzechRepublic,
        Denmark,
        Estonia,
        FaeroeIslands,
        Finland,
        France,
        Germany,
        Greece,
        HongKong,
        Hungary,
        Iceland,
        Japan,
        Latvia,
        Netherlands,
        Norway,
        Poland,
        Qatar,
        Russia,
        Singapore,
        Slovakia,
        Sweden,
        Thailand,
        Ukraine,
        UnitedKingdom,
        Vietnam,
    }
}

impl fmt::Display for Country {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Country {
    fn name(&self) -> String {
        let mut name: Vec<char> = Vec::new();
        for (idx, c) in self.to_string().char_indices() {
            if idx > 0 && c.is_uppercase() {
                name.push(' ');
            }
            name.push(c);
        }
        String::from_iter(name)
    }

    fn code(&self, cca3: &BTreeMap<String, String>) -> Result<String, Error> {
        let country_code = cca3.get(&self.name()).ok_or_else(|| {
            failure::err_msg(format!("{} does not exist in cca3.json", self.name()))
        })?;
        Ok(country_code.to_string())
    }
}

macro_attr! {
    #[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, EnumFromStr!, Serialize, Deserialize)]
    enum Location {
        Local,
        Amsterdam,
        Are,
        Athens,
        Auschwitz,
        Ayutthaya,
        Balestrand,
        Bangkok,
        Bergen,
        Berlin,
        Bodo,
        Bordoy,
        Bratislava,
        Bremen,
        Budapest,
        Chernobyl,
        Copenhagen,
        Crete,
        Doha,
        Dronningmolle,
        Exeter,
        Eysturoy,
        Fjaerland,
        Flam,
        Frankfurt,
        Freiburg,
        Geysir,
        Gothenburg,
        HaLongBay,
        Hanoi,
        Heidelberg,
        Helsingborg,
        Helsingor,
        Helsinki,
        Hestur,
        Himeji,
        Hiroshima,
        HoChiMinhCity,
        HongKongCity,
        Jokulsarlon,
        Kanchanaburi,
        Karlsruhe,
        Katowice,
        Kiev,
        Kinnekulle,
        KoSamui,
        KoTao,
        Koyasan,
        Krakow,
        Kristiansund,
        Kyoto,
        London,
        Lund,
        Melbourne,
        Munich,
        Osaka,
        Oslo,
        Ostersund,
        Paris,
        Petergof,
        Potsdam,
        Prague,
        Pripyat,
        Pushkin,
        Revsund,
        Reykjavik,
        Riga,
        Rorvik,
        Roskilde,
        SaintPetersburg,
        SingaporeCity,
        Skaftafell,
        Skogarfoss,
        Stockholm,
        Streymoy,
        Svolvaer,
        Sydney,
        Tallinn,
        Thingvellir,
        Tokyo,
        Torshavn,
        Trollhattan,
        Tromso,
        Trondheim,
        Trysil,
        Umea,
        Vagar,
        Vidoy,
        Vienna,
        Vik,
        Warsaw,
        Yerevan,
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Location {
    fn name(&self) -> String {
        let mut name: Vec<char> = Vec::new();
        for (idx, c) in self.to_string().char_indices() {
            if idx > 0 && c.is_uppercase() {
                name.push(' ');
            }
            name.push(c);
        }
        let mut location_name = String::from_iter(name);
        if location_name.ends_with("City") && *self != Location::HoChiMinhCity {
            location_name.truncate(location_name.len() - 5);
        }
        location_name
    }

    fn feature_coordinates(&self, features: &[Feature]) -> Result<Vec<f32>, Error> {
        for feature in features {
            if to_location_identfier_string(&feature.properties.name) == self.to_string() {
                match &feature.geometry.coordinates {
                    Coordinates::Point(coords) => return Ok(coords.clone()),
                    _ => {
                        return Err(failure::err_msg(format!(
                            "{} does not have Point coordinates.",
                            self
                        )));
                    }
                };
            }
        }
        Err(failure::err_msg(format!(
            "Could not find coordinates for {}.",
            self
        )))
    }
}

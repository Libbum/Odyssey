#![recursion_limit = "256"]
extern crate failure;
extern crate globwalk;
extern crate hashbrown;
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
use hashbrown::HashMap;
use image::FilterType::Lanczos3;
use image::GenericImageView;
use indicatif::{ProgressBar, ProgressStyle};
use reqwest::header::USER_AGENT;
use std::fs::File;
use std::io::{Read, Write};
use std::iter::FromIterator;
use std::str::FromStr;
use std::{fmt, thread, time};

static NOMINATIM_ENDPOINT: &str = "http://nominatim.openstreetmap.org";
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Serialize, Deserialize)]
pub struct CountryCodes {
    #[serde(with = "codes")]
    pub codes: HashMap<String, String>,
}

type Config = HashMap<String, HashMap<String, Option<String>>>;

#[derive(Debug, Serialize, Deserialize)]
struct CountryCodeStruct {
    name: String,
    #[serde(rename = "alpha-3")]
    alpha3: String,
}

mod codes {
    use super::CountryCodeStruct;
    use hashbrown::HashMap;

    use serde::de::{Deserialize, Deserializer};
    use serde::ser::Serializer;

    pub fn serialize<S>(map: &HashMap<String, String>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(map.values())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<HashMap<String, String>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let mut map = HashMap::new();
        for item in Vec::<CountryCodeStruct>::deserialize(deserializer)? {
            map.insert(item.name, item.alpha3);
        }
        Ok(map)
    }
}

#[derive(Serialize, Deserialize)]
struct FeatureCollection {
    #[serde(rename = "type")]
    type_: String,
    features: Vec<Feature>,
}

#[derive(Serialize, Deserialize)]
struct Feature {
    #[serde(rename = "type")]
    type_: String,
    properties: Properties,
    geometry: Geometry,
}

#[derive(Serialize, Deserialize)]
struct Properties {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    localname: Option<String>,
    country: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Geometry {
    #[serde(rename = "type")]
    type_: String,
    coordinates: Vec<f32>,
}

#[derive(Debug)]
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

macro_attr! {
    #[derive(Debug, PartialEq, EnumFromStr!)]
    enum Country {
        Armenia,
        Australia,
        Austria,
        CzechRepublic,
        Germany,
        Denmark,
        Estonia,
        Finland,
        France,
        FaeroeIslands,
        UnitedKingdom,
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
        Vietnam,
    }
}

macro_attr! {
    #[derive(Debug, PartialEq, EnumFromStr!)]
    enum Location {
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

impl fmt::Display for Country {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Deserialize, Debug)]
struct LatLon {
    lat: String,
    lon: String,
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

fn name_from_string(from: &str) -> String {
    let mut name: Vec<char> = Vec::new();
    for (idx, c) in from.char_indices() {
        if idx > 0 && c.is_uppercase() {
            name.push(' ');
        }
        name.push(c);
    }
    String::from_iter(name)
}

fn main() -> Result<(), Error> {
    // Ignore the thumbnails and blurs at this point. We will check for them later.
    let walker = globwalk::GlobWalkerBuilder::from_patterns(
        "../dist/gallery/",
        &["*.{png,jpg,jpeg,PNG,JPG,JPEG}", "!*_small*", "!*_blur*"],
    )
    .follow_links(true)
    .build()?
    .into_iter()
    .filter_map(Result::ok);

    // This is a little annoying, we can't size_hint this iterator, so we must count it.
    let progcount = globwalk::GlobWalkerBuilder::from_patterns(
        "../dist/gallery/",
        &["*.{png,jpg,jpeg,PNG,JPG,JPEG}", "!*_small*", "!*_blur*"],
    )
    .follow_links(true)
    .build()?
    .into_iter()
    .filter_map(Result::ok)
    .count() as u64;
    let bar = ProgressBar::new(progcount);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("[{elapsed_precise}] {bar:25.cyan/blue} {pos:>5}/{len:5} {msg}"),
    );

    let cca3_file = File::open("cca3.json")?;
    let cca3_read: CountryCodes = serde_json::from_reader(cca3_file)?;
    let cca3 = &cca3_read.codes;

    let config_file = File::open("odyssey.yaml")?;
    let config: Config = serde_yaml::from_reader(config_file)?;
    let mut features: Vec<Feature> = Vec::new();
    let pause = time::Duration::from_secs(1);
    for (country_string, locations) in &config {
        //let country = country_string.parse::<Country>()?;
        let country_name = name_from_string(&country_string);
        let country_code = cca3.get(&country_name).ok_or_else(|| {
            failure::err_msg(format!("{} does not exist in cca3.json", country_name))
        })?;

        //let mut country_local_name = None;
        //if let Some(local) = locations.get("local") {
        //    country_local_name = local.to_owned();
        //}
        //        println!("{}: {}, {:?}, {}", country.to_string(), country_name, country_local_name, country_code);

        for (location_string, local_name) in locations.iter().filter(|(l, _)| *l != "local") {
            let location = location_string.parse::<Location>()?;
            let mut location_name = name_from_string(&location_string);
            if location_name.ends_with("City") && location != Location::HoChiMinhCity {
                location_name.truncate(location_name.len() - 5);
            }
            let coords = search(&format!("{}, {}", location_name, country_name))?;
            let properties = Properties {
                name: location_name,
                localname: local_name.to_owned(),
                country: country_code.to_string(),
            };
            let geometry = Geometry {
                type_: "Point".to_string(),
                coordinates: vec![coords.lon.parse::<f32>()?, coords.lat.parse::<f32>()?],
            };

            println!("{} {:?}", location.to_string(), geometry.coordinates);
            features.push(Feature {
                type_: "Feature".to_string(),
                properties,
                geometry,
            });
            thread::sleep(pause); //We can't hammer the Nominatim server.

            //           println!("    {}: {}, {:?}", location, location_name, local_name);
        }
    }

    let cities = FeatureCollection {
        type_: "FeatureCollection".to_string(),
        features,
    };
    let buffer = File::create("cities.json")?;
    serde_json::to_writer(&buffer, &cities)?;

    let mut manifest = File::create("Manifest.elm")?;

    for file in bar.wrap_iter(walker) {
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
        let location = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Location unwrap issue."))?
            .parse::<Location>()?;
        let _country = path_iter.next(); // We can parse this later if we choose to generate location data here too. Don't need it now.
        let month = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Month unwrap issue."))?
            .parse::<Month>()?;
        let year = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Year unwrap issue."))?;

        write!(
            manifest,
            ",Image \"{}\" (Date {} {:?}) {:?} {:.3} \"{}\"\n",
            name,
            year,
            month,
            location,
            ratio,
            description.trim()
        )?;
    }
    bar.finish();

    Ok(())
}

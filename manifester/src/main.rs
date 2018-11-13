extern crate failure;
extern crate globwalk;
extern crate image;
extern crate indicatif;

use failure::Error;
use image::FilterType::Lanczos3;
use image::GenericImageView;
use indicatif::ProgressBar;
use std::fs::File;
use std::io::{Read, Write};
use std::str::FromStr;

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
            err => Err(failure::err_msg(format!("{} makes no sense to be a month.", err))),
        }
    }
}

#[derive(Debug)]
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

impl FromStr for Location {
    type Err = Error;

    fn from_str(s: &str) -> Result<Location, Error> {
        match s {
            "Amsterdam" => Ok(Location::Amsterdam),
            "Are" => Ok(Location::Are),
            "Athens" => Ok(Location::Athens),
            "Auschwitz" => Ok(Location::Auschwitz),
            "Ayutthaya" => Ok(Location::Ayutthaya),
            "Balestrand" => Ok(Location::Balestrand),
            "Bangkok" => Ok(Location::Bangkok),
            "Bergen" => Ok(Location::Bergen),
            "Berlin" => Ok(Location::Berlin),
            "Bodo" => Ok(Location::Bodo),
            "Bordoy" => Ok(Location::Bordoy),
            "Bratislava" => Ok(Location::Bratislava),
            "Budapest" => Ok(Location::Budapest),
            "Chernobyl" => Ok(Location::Chernobyl),
            "Copenhagen" => Ok(Location::Copenhagen),
            "Crete" => Ok(Location::Crete),
            "Doha" => Ok(Location::Doha),
            "Dronningmolle" => Ok(Location::Dronningmolle),
            "Exeter" => Ok(Location::Exeter),
            "Eysturoy" => Ok(Location::Eysturoy),
            "Fjaerland" => Ok(Location::Fjaerland),
            "Flam" => Ok(Location::Flam),
            "Frankfurt" => Ok(Location::Frankfurt),
            "Freiburg" => Ok(Location::Freiburg),
            "Geysir" => Ok(Location::Geysir),
            "Gothenburg" => Ok(Location::Gothenburg),
            "Ha_Long_Bay" => Ok(Location::HaLongBay),
            "Hanoi" => Ok(Location::Hanoi),
            "Heidelberg" => Ok(Location::Heidelberg),
            "Helsingborg" => Ok(Location::Helsingborg),
            "Helsingor" => Ok(Location::Helsingor),
            "Helsinki" => Ok(Location::Helsinki),
            "Hestur" => Ok(Location::Hestur),
            "Himeji" => Ok(Location::Himeji),
            "Hiroshima" => Ok(Location::Hiroshima),
            "Ho_Chi_Minh_City" => Ok(Location::HoChiMinhCity),
            "Hong Kong" => Ok(Location::HongKongCity),
            "Jokulsarlon" => Ok(Location::Jokulsarlon),
            "Kanchanaburi" => Ok(Location::Kanchanaburi),
            "Karlsruhe" => Ok(Location::Karlsruhe),
            "Katowice" => Ok(Location::Katowice),
            "Kiev" => Ok(Location::Kiev),
            "Kinnekulle" => Ok(Location::Kinnekulle),
            "Ko_Samui" => Ok(Location::KoSamui),
            "Ko_Tao" => Ok(Location::KoTao),
            "Koyasan" => Ok(Location::Koyasan),
            "Krakow" => Ok(Location::Krakow),
            "Kristiansund" => Ok(Location::Kristiansund),
            "Kyoto" => Ok(Location::Kyoto),
            "London" => Ok(Location::London),
            "Lund" => Ok(Location::Lund),
            "Melbourne" => Ok(Location::Melbourne),
            "Munich" => Ok(Location::Munich),
            "Osaka" => Ok(Location::Osaka),
            "Oslo" => Ok(Location::Oslo),
            "Ostersund" => Ok(Location::Ostersund),
            "Paris" => Ok(Location::Paris),
            "Petergof" => Ok(Location::Petergof),
            "Prague" => Ok(Location::Prague),
            "Pripyat" => Ok(Location::Pripyat),
            "Pushkin" => Ok(Location::Pushkin),
            "Revsund" => Ok(Location::Revsund),
            "Reykjavik" => Ok(Location::Reykjavik),
            "Riga" => Ok(Location::Riga),
            "Rorvik" => Ok(Location::Rorvik),
            "Roskilde" => Ok(Location::Roskilde),
            "Saint_Petersburg" => Ok(Location::SaintPetersburg),
            "Singapore" => Ok(Location::SingaporeCity),
            "Skaftafell" => Ok(Location::Skaftafell),
            "Skogarfoss" => Ok(Location::Skogarfoss),
            "Stockholm" => Ok(Location::Stockholm),
            "Streymoy" => Ok(Location::Streymoy),
            "Svolvaer" => Ok(Location::Svolvaer),
            "Sydney" => Ok(Location::Sydney),
            "Tallinn" => Ok(Location::Tallinn),
            "Thingvellir" => Ok(Location::Thingvellir),
            "Tokyo" => Ok(Location::Tokyo),
            "Torshavn" => Ok(Location::Torshavn),
            "Trollhattan" => Ok(Location::Trollhattan),
            "Tromso" => Ok(Location::Tromso),
            "Trondheim" => Ok(Location::Trondheim),
            "Trysil" => Ok(Location::Trysil),
            "Umea" => Ok(Location::Umea),
            "Vagar" => Ok(Location::Vagar),
            "Vidoy" => Ok(Location::Vidoy),
            "Vienna" => Ok(Location::Vienna),
            "Vik" => Ok(Location::Vik),
            "Warsaw" => Ok(Location::Warsaw),
            "Yerevan" => Ok(Location::Yerevan),
            err => Err(failure::err_msg(format!("{} is currently not a location in the system.", err))),
        }
    }
}

fn main() -> Result<(), Error> {
    // Ignore the thumbnails at this point. We will check for them later.
    let walker = globwalk::GlobWalkerBuilder::from_patterns(
        "../gallery/",
        &["*.{png,jpg,jpeg,PNG,JPG,JPEG}", "!*_small*"],
    )
    .follow_links(true)
    .build()?
    .into_iter()
    .filter_map(Result::ok);

    // This is a little annoying, we can't size_hint this iterator, so we must count it.
    let progcount = globwalk::GlobWalkerBuilder::from_patterns(
        "../gallery/",
        &["*.{png,jpg,jpeg,PNG,JPG,JPEG}", "!*_small*"],
    )
    .follow_links(true)
    .build()?
    .into_iter()
    .filter_map(Result::ok)
    .count() as u64;
    let bar = ProgressBar::new(progcount);


    let mut manifest = File::create("Manifest.elm")?;

    for file in walker {
        // Open image and grab its dimensions.
        let img = image::open(&file.path())?;
        let (width, height) = img.dimensions();
        let ratio = width as f64 / height as f64;

        // Generate a thumbnail if one doesn't already exist.
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
        if !file.path().with_file_name(&thumbnail).exists() {
            let thumb_width = if ratio < 3.0 { 500 } else { 900 };

            img.resize(thumb_width, 500, Lanczos3)
                .save(file.path().with_file_name(thumbnail))?;
        }

        // Get image decription if it exists.
        let mut description = String::new();
        File::open(file.path().with_extension("desc"))
            .and_then(|mut f| f.read_to_string(&mut description))?;

        // Build a manifest of all files. We do this entirely each time as descriptions or filenames may have changed.
        let mut path_iter = file.path().strip_prefix("../gallery/")?.iter().rev();

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

        write!(manifest, ",Image \"{}\" (Date {} {:?}) {:?} {:.3} \"{}\"\n", name, year, month, location, ratio, description.trim())?;

        bar.inc(1);
    }
    bar.finish();

    Ok(())
}

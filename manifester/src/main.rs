extern crate failure;
extern crate globwalk;
extern crate image;
extern crate indicatif;
extern crate serde;
extern crate serde_json;

use failure::Error;
use hashbrown::HashMap;
use image::FilterType::Lanczos3;
use image::GenericImageView;
use indicatif::ProgressBar;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Image {
    file: String,
    description: String,
    locale: String,
    aspect_ratio: f64,
}

impl Serialize for Image {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Image", 4)?;
        state.serialize_field("file", &self.file)?;
        state.serialize_field("desc", &self.description)?;
        state.serialize_field("loc", &self.locale)?;
        state.serialize_field("ar", &format!("{:.3}", self.aspect_ratio))?;
        state.end()
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

    // For each key of years we have a map of months, with a map of cities with a map of locations.
    // The location map holds a vector of images.
    let mut years: HashMap<
        String,
        HashMap<String, HashMap<String, HashMap<String, Vec<Image>>>>,
    > = HashMap::new();

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
            .ok_or_else(|| failure::err_msg("Location unwrap issue."))?;
        let country = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Country unwrap issue."))?;
        let month = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Month unwrap issue."))?;
        let year = path_iter
            .next()
            .and_then(|p| p.to_str())
            .ok_or_else(|| failure::err_msg("Year unwrap issue."))?;

        // Gather all the needed information for this image.
        // TODO: Locale generation comes from external files I should also revamp..
        let image = Image {
            file: name.to_string(),
            description: description,
            locale: "".to_string(),
            aspect_ratio: ratio,
        };

        // Recurse down the hashmap to insert the image data into the correct location.
        // This looks a bit absurd, but it keeps the manifest file size small.
        if let Some(months) = years.get_mut(year) {
            if let Some(countries) = months.get_mut(month) {
                if let Some(locations) = countries.get_mut(country) {
                    if let Some(imgs) = locations.get_mut(location) {
                        imgs.push(image);
                    } else {
                        locations.insert(location.to_string(), vec![image]);
                    }
                } else {
                    countries.insert(country.to_string(), HashMap::new());
                }
            } else {
                months.insert(month.to_string(), HashMap::new());
            }
        } else {
            years.insert(year.to_string(), HashMap::new());
        }
        bar.inc(1);
    }
    bar.finish();

    let manifest = serde_json::to_string(&years)?;
    std::fs::write("manifest.json", manifest)?;

    Ok(())
}

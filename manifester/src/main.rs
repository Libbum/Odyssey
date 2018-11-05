extern crate globwalk;
extern crate serde;
extern crate serde_json;
extern crate failure;
extern crate image;

use failure::Error;
use serde::ser::{Serialize, Serializer, SerializeStruct};
use image::GenericImageView;
use hashbrown::HashMap;

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
    let walker = globwalk::GlobWalkerBuilder::from_patterns(
        "../gallery/",
        &["*.{png,jpg,jpeg,PNG,JPG,JPEG}", "!*_small*"],
    )
    .follow_links(true)
    .build()?
    .into_iter()
    .filter_map(Result::ok);

    let mut structure = HashMap::new();
    structure.insert(2017, 06);

    for file in walker {
        let img = image::open(&file.path())?;

        let (width, height) = img.dimensions();

        let name = file.path().file_name().unwrap().to_str().unwrap();

        for folder in file.path().strip_prefix("../gallery/")?.iter() {
            println!("{:?}", folder);
        }

        let image = Image { file: name.to_string(), description: "".to_string(), locale: "".to_string(), aspect_ratio: width as f64 / height as f64 };
        println!("{}", serde_json::to_string(&image)?);
    }

    Ok(())
}

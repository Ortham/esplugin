extern crate cbindgen;

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Don't run cbindgen if it's not cargo being run
    // (i.e. not for cargo coveralls)
    if !env::var("CARGO").unwrap().ends_with("cargo") {
        return;
    }

    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    fs::create_dir_all("include").expect("could not create include directory");

    cbindgen::generate(&crate_dir).unwrap().write_to_file(
        "include/libespm.h",
    );

    let mut config = cbindgen::Config::from_root_or_default(PathBuf::from(&crate_dir).as_path());
    config.language = cbindgen::Language::Cxx;
    cbindgen::generate_with_config(&crate_dir, &config)
        .unwrap()
        .write_to_file("include/libespm.hpp");
}

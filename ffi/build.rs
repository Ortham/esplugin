#[cfg(feature = "ffi-headers")]
mod ffi_headers {
    extern crate cbindgen;

    use std::env;
    use std::fs;

    use self::cbindgen::generate;
    use self::cbindgen::Builder;
    use self::cbindgen::Language;

    pub fn generate_headers() {
        let crate_dir = env::var("CARGO_MANIFEST_DIR")
            .expect("could not get value of CARGO_MANIFEST_DIR env var");

        fs::create_dir_all("include").expect("could not create include directory");

        Builder::new()
            .with_crate(&crate_dir)
            .with_language(Language::C)
            .with_parse_deps(true)
            .with_parse_include(&["esplugin"])
            .generate()
            .expect("could not generate C header file")
            .write_to_file("include/esplugin.h");

        generate(&crate_dir)
            .expect("could not generate C++ header file")
            .write_to_file("include/esplugin.hpp");
    }
}

fn main() {
    #[cfg(feature = "ffi-headers")]
    ffi_headers::generate_headers();
}

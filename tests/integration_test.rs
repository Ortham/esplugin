extern crate libespm;

use std::path::Path;

use libespm::{FormId, GameId, Plugin};

#[test]
fn parse_should_error_if_plugin_does_not_exist() {
    let mut plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));

    assert!(plugin.parse_file(false).is_err());
}

#[test]
fn parse_should_error_if_plugin_is_not_valid() {
    let mut plugin = Plugin::new(GameId::Skyrim, Path::new("README.md"));

    assert!(plugin.parse_file(false).is_err());
}

#[test]
fn parse_should_succeed_for_skyrim_plugin() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    assert!(plugin.parse_file(false).is_ok());
    let masters = plugin.masters().unwrap();

    assert_eq!(4, plugin.data.header_record.subrecords.len());
    assert_eq!("HEDR",
               plugin.data.header_record.subrecords[0].subrecord_type);
    assert_eq!("CNAM",
               plugin.data.header_record.subrecords[1].subrecord_type);
    assert_eq!("SNAM",
               plugin.data.header_record.subrecords[2].subrecord_type);
    assert_eq!("ONAM",
               plugin.data.header_record.subrecords[3].subrecord_type);

    assert!(plugin
                .data
                .form_ids
                .contains(&FormId::new("Blank.esm", &masters, 0xCF9)));
    assert!(plugin
                .data
                .form_ids
                .contains(&FormId::new("Blank.esm", &masters, 0xCF0)));
}

#[test]
fn parse_mmapped_file_should_succeed() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    unsafe {
        assert!(plugin.parse_mmapped_file(false).is_ok());
    }
    let masters = plugin.masters().unwrap();

    assert_eq!(4, plugin.data.header_record.subrecords.len());
    assert_eq!("HEDR",
               plugin.data.header_record.subrecords[0].subrecord_type);
    assert_eq!("CNAM",
               plugin.data.header_record.subrecords[1].subrecord_type);
    assert_eq!("SNAM",
               plugin.data.header_record.subrecords[2].subrecord_type);
    assert_eq!("ONAM",
               plugin.data.header_record.subrecords[3].subrecord_type);

    assert!(plugin
                .data
                .form_ids
                .contains(&FormId::new("Blank.esm", &masters, 0xCF9)));
    assert!(plugin
                .data
                .form_ids
                .contains(&FormId::new("Blank.esm", &masters, 0xCF0)));
}

#[test]
fn parse_should_succeed_for_skyrim_plugin_header_only() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    assert!(plugin.parse_file(true).is_ok());

    assert_eq!(4, plugin.data.header_record.subrecords.len());
    assert_eq!("HEDR",
               plugin.data.header_record.subrecords[0].subrecord_type);
    assert_eq!("CNAM",
               plugin.data.header_record.subrecords[1].subrecord_type);
    assert_eq!("SNAM",
               plugin.data.header_record.subrecords[2].subrecord_type);
    assert_eq!("ONAM",
               plugin.data.header_record.subrecords[3].subrecord_type);

    assert_eq!(0, plugin.data.form_ids.len());
}

#[test]
fn is_valid_should_return_true_for_a_valid_plugin() {
    let is_valid = Plugin::is_valid(GameId::Skyrim,
                                    Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"),
                                    true);

    assert!(is_valid);
}

#[test]
fn is_valid_should_return_false_for_an_invalid_plugin() {
    let is_valid = Plugin::is_valid(GameId::Skyrim, Path::new("README.md"), true);

    assert!(!is_valid);
}

#[test]
fn filename_should_return_filename_in_given_path() {
    let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));

    assert_eq!("Blank.esm", plugin.filename().unwrap());

    let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp"));

    assert_eq!("Blank.esp", plugin.filename().unwrap());
}

#[test]
fn filename_should_trim_dot_ghost_extension() {
    let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp.ghost"));

    assert_eq!("Blank.esp", plugin.filename().unwrap());
}

#[test]
fn is_master_should_be_true_for_master_file() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    assert!(plugin.parse_file(true).is_ok());
    assert!(plugin.is_master_file());

    let mut plugin = Plugin::new(GameId::Morrowind,
                                 Path::new("tests/testing-plugins/Morrowind/Data \
                                            Files/Blank.esm"));

    assert!(plugin.parse_file(true).is_ok());
    assert!(plugin.is_master_file());
}

#[test]
fn is_master_should_be_false_for_non_master_file() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esp"));

    assert!(plugin.parse_file(true).is_ok());
    assert!(!plugin.is_master_file());

    let mut plugin = Plugin::new(GameId::Morrowind,
                                 Path::new("tests/testing-plugins/Morrowind/Data \
                                            Files/Blank.esp"));

    assert!(plugin.parse_file(true).is_ok());
    assert!(!plugin.is_master_file());
}

#[test]
fn masters_should_be_empty_for_blank_esm() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    assert!(plugin.parse_file(true).is_ok());
    assert_eq!(0, plugin.masters().unwrap().len());

    let mut plugin = Plugin::new(GameId::Morrowind,
                                 Path::new("tests/testing-plugins/Morrowind/Data \
                                            Files/Blank.esm"));

    assert!(plugin.parse_file(true).is_ok());
    assert_eq!(0, plugin.masters().unwrap().len());
}

#[test]
fn masters_should_not_be_empty_for_master_dependent_plugin() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank - \
                                            Master Dependent.esm"));

    assert!(plugin.parse_file(true).is_ok());

    let masters = plugin.masters().unwrap();
    assert_eq!(1, masters.len());
    assert_eq!("Blank.esm", masters[0]);

    let mut plugin = Plugin::new(GameId::Morrowind,
                                 Path::new("tests/testing-plugins/Morrowind/Data \
                                            Files/Blank - Master Dependent.esm"));

    assert!(plugin.parse_file(true).is_ok());

    let masters = plugin.masters().unwrap();
    assert_eq!(1, masters.len());
    assert_eq!("Blank.esm", masters[0]);
}

#[test]
fn description_should_return_plugin_description_field_content() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    assert!(plugin.parse_file(true).is_ok());
    assert_eq!("v5.0", plugin.description().unwrap().unwrap());

    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esp"));

    assert!(plugin.parse_file(true).is_ok());
    assert_eq!("€ƒŠ", plugin.description().unwrap().unwrap());

    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank - \
                                            Master Dependent.esm"));

    assert!(plugin.parse_file(true).is_ok());
    assert_eq!("", plugin.description().unwrap().unwrap());
}

#[test]
fn record_and_group_count_should_be_non_zero() {
    let mut plugin = Plugin::new(GameId::Skyrim,
                                 Path::new("tests/testing-plugins/Skyrim/Data/Blank.esm"));

    assert!(plugin.record_and_group_count().is_none());
    assert!(plugin.parse_file(true).is_ok());
    assert_ne!(0, plugin.record_and_group_count().unwrap());
}

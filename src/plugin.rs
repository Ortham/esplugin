/*
 * This file is part of esplugin
 *
 * Copyright (C) 2017 Oliver Hamlet
 *
 * esplugin is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * esplugin is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with esplugin. If not, see <http://www.gnu.org/licenses/>.
 */

use std::fs::File;
use std::io::{BufReader, Read};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::str;

use byteorder::{ByteOrder, LittleEndian};

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};

use nom::{self, ErrorKind, IResult};

use memmap::Mmap;

use unicase::eq;

use error::Error;
use form_id::HashedFormId;
use game_id::GameId;
use group::Group;
use record::Record;

// 1 MB is around the file size at which memory-mapping becomes more performant.
const MIN_MMAP_FILE_SIZE: u64 = 1_000_000;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
struct PluginData {
    header_record: Record,
    form_ids: Vec<HashedFormId>,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Plugin {
    game_id: GameId,
    path: PathBuf,
    data: PluginData,
}

impl Plugin {
    pub fn new(game_id: GameId, filepath: &Path) -> Plugin {
        Plugin {
            game_id,
            path: filepath.to_path_buf(),
            data: PluginData::default(),
        }
    }

    pub fn parse(&mut self, input: &[u8], load_header_only: bool) -> Result<(), Error> {
        match self.filename() {
            None => Err(Error::NoFilename),
            Some(filename) => {
                self.data = parse_plugin(input, self.game_id, &filename, load_header_only)?.1;

                Ok(())
            }
        }
    }

    pub fn parse_open_file(&mut self, file: File, load_header_only: bool) -> Result<(), Error> {
        let mut reader = BufReader::new(&file);

        if load_header_only {
            let content = Record::read_and_validate(&mut reader, self.game_id, self.header_type())?;
            self.parse(&content, load_header_only)
        } else {
            let file_size = file.metadata().map(|m| m.len()).unwrap_or(0);

            if file_size > MIN_MMAP_FILE_SIZE {
                let mmap = unsafe { Mmap::map(&file)? };

                if &mmap[0..4] != self.header_type() {
                    Err(Error::ParsingError)
                } else {
                    self.parse(&mmap, false)
                }
            } else {
                let mut content = vec![0; 4];
                reader.read_exact(&mut content)?;
                if &content[0..4] != self.header_type() {
                    return Err(Error::ParsingError);
                }

                content.reserve(file_size as usize - 3);
                reader.read_to_end(&mut content)?;

                self.parse(&content, false)
            }
        }
    }

    pub fn parse_file(&mut self, load_header_only: bool) -> Result<(), Error> {
        let file = File::open(&self.path)?;

        self.parse_open_file(file, load_header_only)
    }

    pub fn game_id(&self) -> &GameId {
        &self.game_id
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn filename(&self) -> Option<String> {
        self.path
            .file_name()
            .and_then(|filename| filename.to_str())
            .map(|filename| filename.to_string())
    }

    pub fn masters(&self) -> Result<Vec<String>, Error> {
        masters(&self.data.header_record)
    }

    fn has_extension(&self, extension: &str) -> bool {
        if extension.is_empty() {
            return false;
        }

        match self.path.extension() {
            Some(x) if eq(x.to_string_lossy().deref(), &extension[1..]) => true,
            Some(x) if eq(x.to_string_lossy().deref(), "ghost") => {
                let file_stem = self
                    .path
                    .file_stem()
                    .and_then(|file_stem| file_stem.to_str())
                    .map(|f| f.to_lowercase());

                match file_stem {
                    Some(file_stem) => file_stem.ends_with(extension),
                    None => false,
                }
            }
            _ => false,
        }
    }

    pub fn is_master_file(&self) -> bool {
        match self.game_id {
            GameId::Morrowind => self.has_extension(".esm"),
            GameId::Fallout4 | GameId::SkyrimSE => {
                self.is_master_flag_set()
                    || self.has_extension(".esm")
                    || self.has_extension(".esl")
            }
            _ => self.is_master_flag_set(),
        }
    }

    pub fn is_light_master_file(&self) -> bool {
        match self.game_id {
            GameId::Fallout4 | GameId::SkyrimSE => {
                self.is_light_master_flag_set() || self.has_extension(".esl")
            }
            _ => false,
        }
    }

    pub fn is_valid(game_id: GameId, filepath: &Path, load_header_only: bool) -> bool {
        let mut plugin = Plugin::new(game_id, &filepath.to_path_buf());

        plugin.parse_file(load_header_only).is_ok()
    }

    pub fn description(&self) -> Result<Option<String>, Error> {
        let (target_subrecord_type, description_offset) = match self.game_id {
            GameId::Morrowind => ("HEDR", 40),
            _ => ("SNAM", 0),
        };

        for subrecord in self.data.header_record.subrecords() {
            if subrecord.subrecord_type() == target_subrecord_type {
                if subrecord.data().len() <= description_offset {
                    return Err(Error::ParsingError);
                }

                let data = &subrecord.data()[description_offset..(subrecord.data().len() - 1)];

                return WINDOWS_1252
                    .decode(data, DecoderTrap::Strict)
                    .map(Some)
                    .map_err(Error::DecodeError);
            }
        }

        Ok(None)
    }

    pub fn header_version(&self) -> Option<f32> {
        self.data
            .header_record
            .subrecords()
            .iter()
            .find(|s| s.subrecord_type() == "HEDR" && s.data().len() > 3)
            .map(|s| LittleEndian::read_f32(&s.data()[..4]))
    }

    pub fn record_and_group_count(&self) -> Option<u32> {
        let count_offset = match self.game_id {
            GameId::Morrowind => 296,
            _ => 4,
        };

        self.data
            .header_record
            .subrecords()
            .iter()
            .find(|s| s.subrecord_type() == "HEDR" && s.data().len() > count_offset)
            .map(|s| LittleEndian::read_u32(&s.data()[count_offset..count_offset + 4]))
    }

    pub fn count_override_records(&self) -> usize {
        self.data
            .form_ids
            .iter()
            .filter(|f| f.is_overridden_record())
            .count()
    }

    pub fn overlaps_with(&self, other: &Self) -> bool {
        let form_ids = &self.data.form_ids;
        let other_form_ids = &other.data.form_ids;

        let mut index = 0;
        let mut other_index = 0;

        while index != form_ids.len() && other_index != other_form_ids.len() {
            if form_ids[index] < other_form_ids[other_index] {
                index += 1;
            } else {
                if other_form_ids[other_index] == form_ids[index] {
                    return true;
                }
                other_index += 1;
            }
        }

        false
    }

    // A valid light master is one for which all the new records it adds have
    // object indices in the range 0x800 to 0xFFF.
    pub fn is_valid_as_light_master(&self) -> bool {
        match self.game_id {
            GameId::Fallout4 | GameId::SkyrimSE => self
                .data
                .form_ids
                .iter()
                .filter(|f| !f.is_overridden_record())
                .all(HashedFormId::valid_in_light_master),
            _ => false,
        }
    }

    fn header_type(&self) -> &'static [u8] {
        match self.game_id {
            GameId::Morrowind => b"TES3",
            _ => b"TES4",
        }
    }

    fn is_master_flag_set(&self) -> bool {
        self.data.header_record.header().flags() & 0x1 != 0
    }

    fn is_light_master_flag_set(&self) -> bool {
        self.data.header_record.header().flags() & 0x200 != 0
    }
}

fn hashed_form_ids(form_ids: &[u32], filename: &str, masters: &[String]) -> Vec<HashedFormId> {
    let hashed_filename = hash(filename);
    let hashed_masters: Vec<_> = masters.iter().map(|m| hash(&m)).collect();

    let mut form_ids: Vec<_> = form_ids
        .iter()
        .map(|form_id| HashedFormId::new(hashed_filename, &hashed_masters, *form_id))
        .collect();

    form_ids.sort();

    form_ids
}

fn hash(string: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    string.to_lowercase().hash(&mut hasher);
    hasher.finish()
}

fn masters(header_record: &Record) -> Result<Vec<String>, Error> {
    header_record
        .subrecords()
        .iter()
        .filter(|s| s.subrecord_type() == "MAST")
        .map(|s| &s.data()[0..(s.data().len() - 1)])
        .map(|d| {
            WINDOWS_1252
                .decode(d, DecoderTrap::Strict)
                .map_err(Error::DecodeError)
        })
        .collect::<Result<Vec<String>, Error>>()
}

fn parse_form_ids<'a>(input: &'a [u8], game_id: GameId) -> IResult<&'a [u8], Vec<u32>> {
    let mut form_ids = Vec::new();
    let mut remaining_input = input;

    if game_id != GameId::Morrowind {
        while !remaining_input.is_empty() {
            let (input, _) = Group::parse_for_form_ids(remaining_input, game_id, &mut form_ids)?;
            remaining_input = input;
        }
    }

    Ok((remaining_input, form_ids))
}

fn parse_plugin<'a>(
    input: &'a [u8],
    game_id: GameId,
    filename: &str,
    load_header_only: bool,
) -> IResult<&'a [u8], PluginData> {
    let (input1, header_record) = try_parse!(input, apply!(Record::parse, game_id, false));

    if load_header_only {
        return Ok((
            input1,
            PluginData {
                header_record,
                form_ids: Vec::new(),
            },
        ));
    }

    let (input2, form_ids) = try_parse!(input1, apply!(parse_form_ids, game_id));

    let masters = masters(&header_record)
        .map_err(|_| nom::Err::Error(nom::Context::Code(input, ErrorKind::Custom(1))))?;

    let form_ids = hashed_form_ids(&form_ids, filename, &masters);

    Ok((
        input2,
        PluginData {
            header_record,
            form_ids,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    mod morrowind {
        use super::super::*;

        #[test]
        fn parse_file_should_succeed() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());
        }

        #[test]
        fn game_id_should_return_the_plugins_associated_game_id() {
            let plugin = Plugin::new(GameId::Morrowind, &Path::new("Data/Blank.esm"));

            assert_eq!(&GameId::Morrowind, plugin.game_id());
        }

        #[test]
        fn is_master_file_should_be_true_for_master_file() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new(
                    "testing-plugins/Morrowind/Data \
                     Files/Blank.esm",
                ),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_master_file_should_check_file_extension_case_insensitively() {
            let plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Skyrim/Data/Blank.EsM"),
            );

            assert!(plugin.is_master_file());

            let plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Skyrim/Data/Blank.EsM.GHOST"),
            );

            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_master_file_should_be_false_for_non_master_file() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new(
                    "testing-plugins/Morrowind/Data \
                     Files/Blank.esp",
                ),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_false() {
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esl"));
            assert!(!plugin.is_light_master_file());
        }

        #[test]
        fn description_should_return_plugin_header_hedr_subrecord_content() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            let expected_description = format!("{:\0<218}{:\0<38}\n\0\0", "v5.0", "\r");
            assert_eq!(expected_description, plugin.description().unwrap().unwrap());
        }

        #[test]
        fn header_version_should_return_plugin_header_hedr_subrecord_field() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(1.2, plugin.header_version().unwrap());
        }

        #[test]
        fn record_and_group_count_should_read_correct_offset() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.record_and_group_count().is_none());
            assert!(plugin.parse_file(true).is_ok());
            assert_eq!(10, plugin.record_and_group_count().unwrap());
        }

        #[test]
        fn is_valid_as_light_master_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_master());
        }
    }

    mod oblivion {
        use super::super::*;

        #[test]
        fn is_light_master_file_should_be_false() {
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esl"));
            assert!(!plugin.is_light_master_file());
        }

        #[test]
        fn header_version_should_return_plugin_header_hedr_subrecord_field() {
            let mut plugin = Plugin::new(
                GameId::Oblivion,
                Path::new("testing-plugins/Oblivion/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(0.8, plugin.header_version().unwrap());
        }

        #[test]
        fn is_valid_as_light_master_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Oblivion,
                Path::new("testing-plugins/Oblivion/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_master());
        }
    }

    mod skyrim {
        use super::super::*;

        #[test]
        fn parse_file_should_succeed() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            assert_eq!(plugin.data.form_ids.len(), 10);
        }

        #[test]
        fn parse_file_header_only_should_not_store_form_ids() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(0, plugin.data.form_ids.len());
        }

        #[test]
        fn game_id_should_return_the_plugins_associated_game_id() {
            let plugin = Plugin::new(GameId::Skyrim, &Path::new("Data/Blank.esm"));

            assert_eq!(&GameId::Skyrim, plugin.game_id());
        }

        #[test]
        fn is_master_file_should_be_true_for_master_file() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_master_file_should_be_false_for_non_master_file() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_false() {
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esl"));
            assert!(!plugin.is_light_master_file());
        }

        #[test]
        fn description_should_return_plugin_description_field_content() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert_eq!("v5.0", plugin.description().unwrap().unwrap());

            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert_eq!("€ƒŠ", plugin.description().unwrap().unwrap());

            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new(
                    "testing-plugins/Skyrim/Data/Blank - \
                     Master Dependent.esm",
                ),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert_eq!("", plugin.description().unwrap().unwrap());
        }

        #[test]
        fn header_version_should_return_plugin_header_hedr_subrecord_field() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(0.94, plugin.header_version().unwrap());
        }

        #[test]
        fn record_and_group_count_should_be_non_zero_for_a_plugin_with_records() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.record_and_group_count().is_none());
            assert!(plugin.parse_file(true).is_ok());
            assert_eq!(15, plugin.record_and_group_count().unwrap());
        }

        #[test]
        fn count_override_records_should_count_how_many_records_come_from_masters() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Different Master Dependent.esp"),
            );

            assert!(plugin.parse_file(false).is_ok());
            assert_eq!(2, plugin.count_override_records());
        }

        #[test]
        fn overlaps_with_should_detect_when_two_plugins_have_a_record_from_the_same_master() {
            let mut plugin1 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Different.esm"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());

            assert!(plugin1.overlaps_with(&plugin1));
            assert!(!plugin1.overlaps_with(&plugin2));
        }

        #[test]
        fn is_valid_as_light_master_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_master());
        }
    }

    mod skyrimse {
        use super::super::*;

        use std::fs::read;

        #[test]
        fn is_master_file_should_use_file_extension_and_flag() {
            use std::fs::copy;
            copy(
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
                Path::new("testing-plugins/Skyrim/Data/Blank.esm.esp"),
            )
            .unwrap();

            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esp"));
            assert!(!plugin.is_master_file());

            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esm"));
            assert!(plugin.is_master_file());

            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl"));
            assert!(plugin.is_master_file());

            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());

            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm.esp"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_true_for_plugins_with_an_esl_file_extension() {
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl"));
            assert!(plugin.is_light_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_true_for_a_ghosted_esl_file() {
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl.ghost"));
            assert!(plugin.is_light_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_true_for_an_esp_file_with_the_light_master_flag_set() {
            use std::fs::copy;
            copy(
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl"),
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl.esp"),
            )
            .unwrap();

            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl.esp"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_light_master_file());
            assert!(!plugin.is_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_true_for_an_esm_file_with_the_light_master_flag_set() {
            use std::fs::copy;
            copy(
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl"),
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl.esm"),
            )
            .unwrap();

            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl.esm"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_light_master_file());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn header_version_should_return_plugin_header_hedr_subrecord_field() {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(0.94, plugin.header_version().unwrap());
        }

        #[test]
        fn is_valid_as_light_master_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.is_valid_as_light_master());
        }

        #[test]
        fn is_valid_as_light_master_should_be_true_if_the_plugin_has_an_override_form_id_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            let mut bytes = read(plugin.path()).unwrap();

            assert_eq!(0xF0, bytes[0x7A]);
            assert_eq!(0x0C, bytes[0x7B]);
            bytes[0x7A] = 0xFF;
            bytes[0x7B] = 0x07;

            assert!(plugin.parse(&bytes, false).is_ok());

            assert!(plugin.is_valid_as_light_master());
        }

        #[test]
        fn is_valid_as_light_master_should_be_false_if_the_plugin_has_a_new_form_id_less_than_0x800(
        ) {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            let mut bytes = read(plugin.path()).unwrap();

            assert_eq!(0xEB, bytes[0x386]);
            assert_eq!(0x0C, bytes[0x387]);
            bytes[0x386] = 0xFF;
            bytes[0x387] = 0x07;

            assert!(plugin.parse(&bytes, false).is_ok());

            assert!(!plugin.is_valid_as_light_master());
        }

        #[test]
        fn is_valid_as_light_master_should_be_false_if_the_plugin_has_a_new_form_id_greater_than_0xfff(
        ) {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            let mut bytes = read(plugin.path()).unwrap();

            assert_eq!(0xEB, bytes[0x386]);
            assert_eq!(0x0C, bytes[0x387]);
            bytes[0x386] = 0x00;
            bytes[0x387] = 0x10;

            assert!(plugin.parse(&bytes, false).is_ok());

            assert!(!plugin.is_valid_as_light_master());
        }
    }

    mod fallout3 {
        use super::super::*;

        #[test]
        fn is_light_master_file_should_be_false() {
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esl"));
            assert!(!plugin.is_light_master_file());
        }

        #[test]
        fn is_valid_as_light_master_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Fallout3,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_master());
        }
    }

    mod falloutnv {
        use super::super::*;

        #[test]
        fn is_light_master_file_should_be_false() {
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esl"));
            assert!(!plugin.is_light_master_file());
        }

        #[test]
        fn is_valid_as_light_master_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::FalloutNV,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_master());
        }
    }

    mod fallout4 {
        use super::super::*;

        #[test]
        fn is_master_file_should_use_file_extension_and_flag() {
            use std::fs::copy;
            copy(
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
                Path::new("testing-plugins/Skyrim/Data/Blank.esm.esp"),
            )
            .unwrap();

            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esp"));
            assert!(!plugin.is_master_file());

            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esm"));
            assert!(plugin.is_master_file());

            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl"));
            assert!(plugin.is_master_file());

            let mut plugin = Plugin::new(
                GameId::Fallout4,
                Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());

            let mut plugin = Plugin::new(
                GameId::Fallout4,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm.esp"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_true_for_plugins_with_an_esl_file_extension() {
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esp"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esm"));
            assert!(!plugin.is_light_master_file());
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl"));
            assert!(plugin.is_light_master_file());
        }

        #[test]
        fn is_light_master_file_should_be_true_for_a_ghosted_esl_file() {
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl.ghost"));
            assert!(plugin.is_light_master_file());
        }

        #[test]
        fn is_valid_as_light_master_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::Fallout4,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.is_valid_as_light_master());
        }
    }

    fn write_invalid_plugin() {
        use std::io::Write;
        let mut file = File::create("testing-plugins/Skyrim/Data/Invalid.esm").unwrap();
        let bytes = [0; 24];
        file.write_all(&bytes).unwrap();
    }

    #[test]
    fn parse_file_should_error_if_plugin_does_not_exist() {
        let mut plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));

        assert!(plugin.parse_file(false).is_err());
    }

    #[test]
    fn parse_file_should_error_if_plugin_is_not_valid() {
        let mut plugin = Plugin::new(GameId::Skyrim, Path::new("README.md"));

        assert!(plugin.parse_file(false).is_err());
    }

    #[test]
    fn parse_file_header_only_should_fail_for_a_non_plugin_file() {
        write_invalid_plugin();

        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Invalid.esm"),
        );

        assert!(plugin.parse_file(true).is_err());
    }

    #[test]
    fn parse_file_should_fail_for_a_non_plugin_file() {
        write_invalid_plugin();

        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Invalid.esm"),
        );

        assert!(plugin.parse_file(false).is_err());
    }

    #[test]
    fn is_valid_should_return_true_for_a_valid_plugin() {
        let is_valid = Plugin::is_valid(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            true,
        );

        assert!(is_valid);
    }

    #[test]
    fn is_valid_should_return_false_for_an_invalid_plugin() {
        let is_valid = Plugin::is_valid(GameId::Skyrim, Path::new("README.md"), true);

        assert!(!is_valid);
    }

    #[test]
    fn path_should_return_the_full_plugin_path() {
        let path = Path::new("Data/Blank.esm");
        let plugin = Plugin::new(GameId::Skyrim, path);

        assert_eq!(path, plugin.path());
    }

    #[test]
    fn filename_should_return_filename_in_given_path() {
        let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));

        assert_eq!("Blank.esm", plugin.filename().unwrap());

        let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp"));

        assert_eq!("Blank.esp", plugin.filename().unwrap());
    }

    #[test]
    fn filename_should_not_trim_dot_ghost_extension() {
        let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp.ghost"));

        assert_eq!("Blank.esp.ghost", plugin.filename().unwrap());
    }

    #[test]
    fn has_extension_should_be_false_if_passed_an_empty_string() {
        let plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(!plugin.has_extension(""));
    }

    #[test]
    fn masters_should_be_empty_for_a_plugin_with_no_masters() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(plugin.parse_file(true).is_ok());
        assert_eq!(0, plugin.masters().unwrap().len());
    }

    #[test]
    fn masters_should_not_be_empty_for_a_plugin_with_one_or_more_masters() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new(
                "testing-plugins/Skyrim/Data/Blank - \
                 Master Dependent.esm",
            ),
        );

        assert!(plugin.parse_file(true).is_ok());

        let masters = plugin.masters().unwrap();
        assert_eq!(1, masters.len());
        assert_eq!("Blank.esm", masters[0]);
    }

    #[test]
    fn description_should_error_for_a_plugin_header_subrecord_that_is_too_small() {
        let mut plugin = Plugin::new(
            GameId::Morrowind,
            Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
        );

        let mut data =
            include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[..0x20].to_vec();
        data[0x04] = 16;
        data[0x05] = 0;
        data[0x14] = 8;
        data[0x15] = 0;

        assert!(plugin.parse(&data, true).is_ok());
        assert!(plugin.description().is_err());
    }

    #[test]
    fn header_version_should_be_none_for_a_plugin_header_hedr_subrecord_that_is_too_small() {
        let mut plugin = Plugin::new(
            GameId::Morrowind,
            Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
        );

        let mut data =
            include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[..0x1B].to_vec();
        data[0x04] = 11;
        data[0x05] = 0;
        data[0x14] = 3;
        data[0x15] = 0;

        assert!(plugin.parse(&data, true).is_ok());
        assert!(plugin.header_version().is_none());
    }

    #[test]
    fn record_and_group_count_should_be_none_for_a_plugin_hedr_subrecord_that_is_too_small() {
        let mut plugin = Plugin::new(
            GameId::Morrowind,
            Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
        );

        let mut data =
            include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[..0x140].to_vec();
        data[0x04] = 0x30;
        data[0x14] = 0x28;

        assert!(plugin.parse(&data, true).is_ok());
        assert!(plugin.record_and_group_count().is_none());
    }

    #[test]
    fn hashed_form_ids_should_use_plugin_names_case_insensitively() {
        let raw_form_ids = vec![0x0000_0001, 0x0100_0002];

        let masters = vec!["tést.esm".to_string()];
        let form_ids = hashed_form_ids(&raw_form_ids, "Blàñk.esp", &masters);

        let other_masters = vec!["TÉST.ESM".to_string()];
        let other_form_ids = hashed_form_ids(&raw_form_ids, "BLÀÑK.ESP", &other_masters);

        assert_eq!(form_ids, other_form_ids);
    }
}

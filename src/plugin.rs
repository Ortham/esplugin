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

use std::collections::BTreeSet;
use std::fs::File;
use std::io::{BufReader, Cursor, Read};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::str;

use byteorder::{LittleEndian, ReadBytesExt};

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};

use nom::{self, ErrorKind, IResult};

use memmap::Mmap;

use unicase::eq;

use error::Error;
use form_id::FormId;
use game_id::GameId;
use group::Group;
use record::Record;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
struct PluginData {
    header_record: Record,
    form_ids: BTreeSet<FormId>,
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

        let mut content: Vec<u8>;
        if load_header_only {
            content = Record::read_and_validate(&mut reader, self.game_id, self.header_type())?;
        } else {
            content = vec![0; 4];
            reader.read_exact(&mut content)?;
            if &content[0..4] != self.header_type() {
                return Err(Error::ParsingError);
            }

            content.reserve(file.metadata().map(|m| m.len() as usize - 3).unwrap_or(0));

            reader.read_to_end(&mut content)?;
        }

        self.parse(&content, load_header_only)
    }

    pub fn parse_file(&mut self, load_header_only: bool) -> Result<(), Error> {
        let file = File::open(&self.path)?;

        self.parse_open_file(file, load_header_only)
    }

    pub unsafe fn parse_mmapped_file(&mut self, load_header_only: bool) -> Result<(), Error> {
        let file = File::open(&self.path)?;
        let mmap = Mmap::map(&file)?;

        if &mmap[0..4] != self.header_type() {
            Err(Error::ParsingError)
        } else {
            self.parse(&mmap, load_header_only)
        }
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
        match self.path.extension() {
            Some(x) if eq(x.to_string_lossy().deref(), &extension[1..]) => true,
            Some(x) if eq(x.to_string_lossy().deref(), "ghost") => {
                let file_stem = self.path
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
                self.is_master_flag_set() || self.has_extension(".esm")
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

        match plugin.parse_file(load_header_only) {
            Ok(_) => true,
            Err(_) => false,
        }
    }

    pub fn description(&self) -> Result<Option<String>, Error> {
        let (target_subrecord_type, description_offset) = if self.game_id == GameId::Morrowind {
            ("HEDR", 40)
        } else {
            ("SNAM", 0)
        };

        for subrecord in self.data.header_record.subrecords() {
            if subrecord.subrecord_type() == target_subrecord_type {
                let data = &subrecord.data()[description_offset..(subrecord.data().len() - 1)];

                return WINDOWS_1252
                    .decode(data, DecoderTrap::Strict)
                    .map(Option::Some)
                    .map_err(Error::DecodeError);
            }
        }

        Ok(Option::None)
    }

    pub fn record_and_group_count(&self) -> Option<u32> {
        let count_offset = if self.game_id == GameId::Morrowind {
            296
        } else {
            4
        };

        for subrecord in self.data.header_record.subrecords() {
            if subrecord.subrecord_type() == "HEDR" {
                let data = &subrecord.data()[count_offset..count_offset + 4];
                let mut cursor = Cursor::new(data);
                return cursor.read_u32::<LittleEndian>().ok();
            }
        }

        Option::None
    }

    pub fn form_ids(&self) -> &BTreeSet<FormId> {
        &self.data.form_ids
    }

    pub fn count_override_records(&self) -> usize {
        if let Some(n) = self.filename() {
            self.form_ids()
                .iter()
                .filter(|f| !eq(f.plugin_name(), &n))
                .count()
        } else {
            0
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

fn parse_form_ids<'a>(
    input: &'a [u8],
    game_id: GameId,
    filename: &str,
    masters: &[String],
) -> IResult<&'a [u8], BTreeSet<FormId>> {
    let mut form_ids: BTreeSet<FormId> = BTreeSet::new();
    let mut remaining_input = input;

    if game_id == GameId::Morrowind {
        while !remaining_input.is_empty() {
            let (input, form_id) = Record::parse_form_id(remaining_input, game_id)?;
            remaining_input = input;

            form_ids.insert(FormId::new(filename, masters, form_id));
        }
    } else {
        while !remaining_input.is_empty() {
            let (input, group) = Group::new(remaining_input, game_id)?;
            remaining_input = input;

            form_ids.extend(
                group
                    .form_ids()
                    .into_iter()
                    .map(|form_id| FormId::new(filename, masters, form_id)),
            );
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
                form_ids: BTreeSet::new(),
            },
        ));
    }

    let masters = masters(&header_record)
        .map_err(|_| nom::Err::Error(nom::Context::Code(input, ErrorKind::Custom(1))))?;

    let (input2, form_ids) =
        try_parse!(input1, apply!(parse_form_ids, game_id, filename, &masters));

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
    fn parse_file_should_succeed_for_skyrim_plugin() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(plugin.parse_file(false).is_ok());
        let masters = plugin.masters().unwrap();
        let form_ids = plugin.form_ids();

        assert!(form_ids.contains(&FormId::new("Blank.esm", &masters, 0xCF9)));
        assert!(form_ids.contains(&FormId::new("Blank.esm", &masters, 0xCF0)));
    }

    #[test]
    fn parse_file_should_succeed_for_skyrim_plugin_header_only() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(plugin.parse_file(true).is_ok());

        assert_eq!(0, plugin.form_ids().len());
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
    fn parse_mmapped_file_should_succeed() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        unsafe {
            assert!(plugin.parse_mmapped_file(false).is_ok());
        }
        let masters = plugin.masters().unwrap();
        let form_ids = plugin.form_ids();

        assert!(form_ids.contains(&FormId::new("Blank.esm", &masters, 0xCF9)));
        assert!(form_ids.contains(&FormId::new("Blank.esm", &masters, 0xCF0)));
    }

    #[test]
    fn parse_mmapped_file_should_fail_for_a_non_plugin_file() {
        write_invalid_plugin();

        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Invalid.esm"),
        );

        unsafe {
            assert!(plugin.parse_mmapped_file(true).is_err());
        }
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
    fn game_id_should_return_the_plugins_associated_game_id() {
        let plugin = Plugin::new(GameId::Skyrim, &Path::new("Data/Blank.esm"));

        assert_eq!(&GameId::Skyrim, plugin.game_id());
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
    fn is_master_file_should_be_true_for_master_file() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(plugin.parse_file(true).is_ok());
        assert!(plugin.is_master_file());

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
    fn is_master_file_should_check_morrowind_plugin_extensions_case_insensitively() {
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
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
        );

        assert!(plugin.parse_file(true).is_ok());
        assert!(!plugin.is_master_file());

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
    fn is_master_file_should_also_use_file_extension_for_fallout4_and_skyrim_se() {
        use std::fs::copy;
        copy(
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            Path::new("testing-plugins/Skyrim/Data/Blank.esm.esp"),
        ).unwrap();

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
    fn is_light_master_file_should_be_false_for_all_plugins_for_all_pre_fallout4_games() {
        let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esl"));
        assert!(!plugin.is_light_master_file());

        let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esl"));
        assert!(!plugin.is_light_master_file());

        let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esl"));
        assert!(!plugin.is_light_master_file());

        let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esl"));
        assert!(!plugin.is_light_master_file());

        let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esl"));
        assert!(!plugin.is_light_master_file());
    }

    #[test]
    fn is_light_master_file_should_be_true_for_fallout4_plugins_with_an_esl_file_extension() {
        let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl"));
        assert!(plugin.is_light_master_file());
    }

    #[test]
    fn is_light_master_file_should_be_true_for_skyrimse_plugins_with_an_esl_file_extension() {
        let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esp"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esm"));
        assert!(!plugin.is_light_master_file());
        let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl"));
        assert!(plugin.is_light_master_file());
    }

    #[test]
    fn is_light_master_file_should_be_true_for_a_ghosted_fallout4_esl_file() {
        let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl.ghost"));
        assert!(plugin.is_light_master_file());
    }

    #[test]
    fn is_light_master_file_should_be_true_for_a_ghosted_skyrimse_esl_file() {
        let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl.ghost"));
        assert!(plugin.is_light_master_file());
    }

    #[test]
    fn is_light_master_file_should_be_true_for_an_esp_file_with_the_light_master_flag_set() {
        use std::fs::copy;
        copy(
            Path::new("testing-plugins/SkyrimSE/Data/Blank.esl"),
            Path::new("testing-plugins/SkyrimSE/Data/Blank.esl.esp"),
        ).unwrap();

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
        ).unwrap();

        let mut plugin = Plugin::new(
            GameId::SkyrimSE,
            Path::new("testing-plugins/SkyrimSE/Data/Blank.esl.esm"),
        );
        assert!(plugin.parse_file(true).is_ok());
        assert!(plugin.is_light_master_file());
        assert!(plugin.is_master_file());
    }

    #[test]
    fn masters_should_be_empty_for_blank_esm() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(plugin.parse_file(true).is_ok());
        assert_eq!(0, plugin.masters().unwrap().len());

        let mut plugin = Plugin::new(
            GameId::Morrowind,
            Path::new(
                "testing-plugins/Morrowind/Data \
                 Files/Blank.esm",
            ),
        );

        assert!(plugin.parse_file(true).is_ok());
        assert_eq!(0, plugin.masters().unwrap().len());
    }

    #[test]
    fn masters_should_not_be_empty_for_master_dependent_plugin() {
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

        let mut plugin = Plugin::new(
            GameId::Morrowind,
            Path::new(
                "testing-plugins/Morrowind/Data \
                 Files/Blank - Master Dependent.esm",
            ),
        );

        assert!(plugin.parse_file(true).is_ok());

        let masters = plugin.masters().unwrap();
        assert_eq!(1, masters.len());
        assert_eq!("Blank.esm", masters[0]);
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
    fn record_and_group_count_should_be_non_zero() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
        );

        assert!(plugin.record_and_group_count().is_none());
        assert!(plugin.parse_file(true).is_ok());
        assert_ne!(0, plugin.record_and_group_count().unwrap());
    }

    #[test]
    fn count_override_records() {
        let mut plugin = Plugin::new(
            GameId::Skyrim,
            Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
        );

        assert!(plugin.parse_file(false).is_ok());
        assert_eq!(4, plugin.count_override_records());
    }
}

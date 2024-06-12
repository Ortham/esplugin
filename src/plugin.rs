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
use std::io::{BufRead, BufReader, Seek};
use std::ops::RangeInclusive;
use std::path::{Path, PathBuf};
use std::str;

use encoding_rs::WINDOWS_1252;

use nom::{self, IResult};

use crate::error::{Error, ParsingErrorKind};
use crate::form_id::HashedFormId;
use crate::game_id::GameId;
use crate::group::Group;
use crate::record::Record;
use crate::record_id::{NamespacedId, RecordId};

#[derive(Copy, Clone, PartialEq, Eq)]
enum FileExtension {
    Esm,
    Esl,
}

impl PartialEq<&std::borrow::Cow<'_, str>> for FileExtension {
    fn eq(&self, other: &&std::borrow::Cow<'_, str>) -> bool {
        const ESM: &str = "esm";
        const ESL: &str = "esl";

        // Lowercasing strings isn't generally a good way to compare them, but
        // here the strings are of the correct length and they're being compared
        // against ASCII literals.
        match self {
            FileExtension::Esm => other.len() == ESM.len() && other.to_lowercase() == ESM,
            FileExtension::Esl => other.len() == ESL.len() && other.to_lowercase() == ESL,
        }
    }
}

fn is_ghost_file_extension(extension: &str) -> bool {
    const GHOST: &str = "ghost";

    // Generally not a good idea to lowercase strings for case-insensitive
    // comparison, but this is ASCII, and we're checking that the string is of
    // the correct length first, so lowercasing won't be expensive.
    extension.len() == GHOST.len() && extension.to_lowercase() == GHOST
}

#[derive(Clone, Default, PartialEq, Eq, Debug, Hash)]
enum RecordIds {
    #[default]
    None,
    FormIds(Vec<HashedFormId>),
    NamespacedIds(Vec<NamespacedId>),
}

impl From<Vec<NamespacedId>> for RecordIds {
    fn from(record_ids: Vec<NamespacedId>) -> RecordIds {
        RecordIds::NamespacedIds(record_ids)
    }
}

impl From<Vec<HashedFormId>> for RecordIds {
    fn from(form_ids: Vec<HashedFormId>) -> RecordIds {
        RecordIds::FormIds(form_ids)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
struct PluginData {
    header_record: Record,
    record_ids: RecordIds,
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
            None => Err(Error::NoFilename(self.path.clone())),
            Some(filename) => {
                self.data = parse_plugin(input, self.game_id, &filename, load_header_only)?.1;

                Ok(())
            }
        }
    }

    fn read<R: BufRead + Seek>(
        &mut self,
        mut reader: R,
        load_header_only: bool,
        expected_header_type: &'static [u8],
    ) -> Result<(), Error> {
        match self.filename() {
            None => Err(Error::NoFilename(self.path.clone())),
            Some(filename) => {
                self.data = read_plugin(
                    &mut reader,
                    self.game_id,
                    &filename,
                    load_header_only,
                    expected_header_type,
                )?;

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
            self.read(reader, load_header_only, self.header_type())
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
            .and_then(std::ffi::OsStr::to_str)
            .map(std::string::ToString::to_string)
    }

    pub fn masters(&self) -> Result<Vec<String>, Error> {
        masters(&self.data.header_record)
    }

    fn has_extension(&self, extension: FileExtension) -> bool {
        let path_extension = self.path.extension().map(|e| e.to_string_lossy());
        match path_extension {
            Some(ref e) if extension == e => true,
            Some(ref e) if is_ghost_file_extension(e) => {
                let path_extension = self
                    .path
                    .file_stem()
                    .map(Path::new)
                    .and_then(|p| p.extension())
                    .map(|e| e.to_string_lossy());

                matches!(path_extension, Some(ref e) if extension == e)
            }
            _ => false,
        }
    }

    pub fn is_master_file(&self) -> bool {
        match self.game_id {
            GameId::Fallout4 | GameId::SkyrimSE | GameId::Starfield => {
                self.is_master_flag_set()
                    || self.has_extension(FileExtension::Esm)
                    // The .esl extension implies the master flag, but the light and medium flag do not.
                    || self.has_extension(FileExtension::Esl)
            }
            _ => self.is_master_flag_set(),
        }
    }

    #[deprecated = "This has been renamed to Plugin::is_light_plugin() for clarity."]
    pub fn is_light_master_file(&self) -> bool {
        self.is_light_plugin()
    }

    pub fn is_light_plugin(&self) -> bool {
        if self.game_id.supports_light_plugins() {
            if self.game_id == GameId::Starfield {
                // If the inject flag is set, it prevents the .esl extension from
                // causing the light flag to be forcibly set on load.
                self.is_light_flag_set()
                    || (!self.is_override_flag_set() && self.has_extension(FileExtension::Esl))
            } else {
                self.is_light_flag_set() || self.has_extension(FileExtension::Esl)
            }
        } else {
            false
        }
    }

    pub fn is_medium_plugin(&self) -> bool {
        // If the medium flag is set in a light plugin then the medium flag is ignored.
        self.is_medium_flag_set() && !self.is_light_plugin()
    }

    pub fn is_override_plugin(&self) -> bool {
        // The override flag is unset by the game if the plugin has no masters or
        // if the plugin's light flag is set.
        self.is_override_flag_set()
            && !self.is_light_flag_set()
            && self.masters().map(|m| !m.is_empty()).unwrap_or(false)
    }

    pub fn is_valid(game_id: GameId, filepath: &Path, load_header_only: bool) -> bool {
        let mut plugin = Plugin::new(game_id, filepath);

        plugin.parse_file(load_header_only).is_ok()
    }

    pub fn description(&self) -> Result<Option<String>, Error> {
        let (target_subrecord_type, description_offset) = match self.game_id {
            GameId::Morrowind => (b"HEDR", 40),
            _ => (b"SNAM", 0),
        };

        for subrecord in self.data.header_record.subrecords() {
            if subrecord.subrecord_type() == target_subrecord_type {
                if subrecord.data().len() <= description_offset {
                    return Err(Error::ParsingError(
                        subrecord.data().to_vec(),
                        ParsingErrorKind::SubrecordDataTooShort(description_offset),
                    ));
                }

                let data = &subrecord.data()[description_offset..(subrecord.data().len() - 1)];

                return WINDOWS_1252
                    .decode_without_bom_handling_and_without_replacement(data)
                    .map(|s| Some(s.to_string()))
                    .ok_or(Error::DecodeError(data.to_vec()));
            }
        }

        Ok(None)
    }

    pub fn header_version(&self) -> Option<f32> {
        self.data
            .header_record
            .subrecords()
            .iter()
            .find(|s| s.subrecord_type() == b"HEDR" && s.data().len() > 3)
            .map(|s| crate::le_slice_to_f32(s.data()))
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
            .find(|s| s.subrecord_type() == b"HEDR" && s.data().len() > count_offset)
            .map(|s| crate::le_slice_to_u32(&s.data()[count_offset..]))
    }

    pub fn count_override_records(&self) -> usize {
        if let RecordIds::FormIds(form_ids) = &self.data.record_ids {
            form_ids.iter().filter(|f| f.is_overridden_record()).count()
        } else {
            0
        }
    }

    pub fn overlaps_with(&self, other: &Self) -> bool {
        use self::RecordIds::*;
        match (&self.data.record_ids, &other.data.record_ids) {
            (FormIds(left), FormIds(right)) => sorted_slices_intersect(left, right),
            (NamespacedIds(left), NamespacedIds(right)) => sorted_slices_intersect(left, right),
            _ => false,
        }
    }

    /// Count the number of records that appear in this plugin and one or more
    /// the others passed. If more than one other contains the same record, it
    /// is only counted once.
    pub fn overlap_size(&self, others: &[&Self]) -> usize {
        use self::RecordIds::*;

        match &self.data.record_ids {
            FormIds(ids) => ids
                .iter()
                .filter(|id| {
                    others.iter().any(|other| match &other.data.record_ids {
                        FormIds(master_ids) => master_ids.binary_search(id).is_ok(),
                        _ => false,
                    })
                })
                .count(),
            NamespacedIds(ids) => ids
                .iter()
                .filter(|id| {
                    others.iter().any(|other| match &other.data.record_ids {
                        NamespacedIds(master_ids) => master_ids.binary_search(id).is_ok(),
                        _ => false,
                    })
                })
                .count(),
            None => 0,
        }
    }

    #[deprecated = "This has been renamed to Plugin::is_valid_as_light_plugin() for clarity."]
    pub fn is_valid_as_light_master(&self) -> bool {
        self.is_valid_as_light_plugin()
    }

    pub fn is_valid_as_light_plugin(&self) -> bool {
        if self.game_id.supports_light_plugins() {
            match &self.data.record_ids {
                RecordIds::None => true,
                RecordIds::FormIds(form_ids) => {
                    let valid_range = self.valid_light_form_id_range();

                    form_ids
                        .iter()
                        .filter(|f| !f.is_overridden_record())
                        .all(|f| valid_range.contains(&f.object_index()))
                }
                RecordIds::NamespacedIds(_) => false, // this should never happen.
            }
        } else {
            false
        }
    }

    pub fn is_valid_as_override_plugin(&self) -> bool {
        if self.game_id == GameId::Starfield {
            // If an override plugin has a record that does not override an existing record, that
            // record is placed into the mod index of the plugin's first master, which risks
            // overwriting an unrelated record with the same object index, so treat that case as
            // invalid.
            match &self.data.record_ids {
                RecordIds::None => true,
                RecordIds::FormIds(form_ids) => {
                    form_ids.iter().all(HashedFormId::is_overridden_record)
                }
                RecordIds::NamespacedIds(_) => false, // this should never happen.
            }
        } else {
            false
        }
    }

    fn header_type(&self) -> &'static [u8] {
        match self.game_id {
            GameId::Morrowind => b"TES3",
            _ => b"TES4",
        }
    }

    fn is_master_flag_set(&self) -> bool {
        match self.game_id {
            GameId::Morrowind => self
                .data
                .header_record
                .subrecords()
                .iter()
                .find(|s| s.subrecord_type() == b"HEDR")
                .and_then(|s| s.data().get(4))
                .map(|b| b & 0x1 != 0)
                .unwrap_or(false),
            _ => self.data.header_record.header().flags() & 0x1 != 0,
        }
    }

    fn is_light_flag_set(&self) -> bool {
        let flag = match self.game_id {
            GameId::Starfield => 0x100,
            GameId::SkyrimSE | GameId::Fallout4 => 0x200,
            _ => return false,
        };

        self.data.header_record.header().flags() & flag != 0
    }

    fn is_medium_flag_set(&self) -> bool {
        let flag = match self.game_id {
            GameId::Starfield => 0x400,
            _ => return false,
        };

        self.data.header_record.header().flags() & flag != 0
    }

    fn is_override_flag_set(&self) -> bool {
        match self.game_id {
            GameId::Starfield => self.data.header_record.header().flags() & 0x200 != 0,
            _ => false,
        }
    }

    fn valid_light_form_id_range(&self) -> RangeInclusive<u32> {
        match self.game_id {
            GameId::SkyrimSE => match self.header_version() {
                Some(v) if v < 1.71 => 0x800..=0xFFF,
                Some(_) => 0..=0xFFF,
                None => 0..=0,
            },
            GameId::Fallout4 => match self.header_version() {
                Some(v) if v < 1.0 => 0x800..=0xFFF,
                Some(_) => 0x001..=0xFFF,
                None => 0..=0,
            },
            GameId::Starfield => 0..=0xFFF,
            _ => 0..=0,
        }
    }
}

fn sorted_slices_intersect<T: PartialOrd>(left: &[T], right: &[T]) -> bool {
    let mut left_iter = left.iter();
    let mut right_iter = right.iter();

    let mut left_element = left_iter.next();
    let mut right_element = right_iter.next();

    while let (Some(left_value), Some(right_value)) = (left_element, right_element) {
        if left_value < right_value {
            left_element = left_iter.next();
        } else if left_value > right_value {
            right_element = right_iter.next();
        } else {
            return true;
        }
    }

    false
}

fn hashed_form_ids(form_ids: &[u32], filename: &str, masters: &[String]) -> Vec<HashedFormId> {
    let hashed_filename = hash(filename);
    let hashed_masters: Vec<_> = masters.iter().map(|m| hash(m)).collect();

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
        .filter(|s| s.subrecord_type() == b"MAST")
        .map(|s| &s.data()[0..(s.data().len() - 1)])
        .map(|d| {
            WINDOWS_1252
                .decode_without_bom_handling_and_without_replacement(d)
                .map(|s| s.to_string())
                .ok_or(Error::DecodeError(d.to_vec()))
        })
        .collect::<Result<Vec<String>, Error>>()
}

fn parse_form_ids(input: &[u8], game_id: GameId) -> IResult<&[u8], Vec<u32>> {
    let mut form_ids = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        let (input, _) = Group::parse_for_form_ids(remaining_input, game_id, &mut form_ids)?;
        remaining_input = input;
    }

    Ok((remaining_input, form_ids))
}

fn read_form_ids<R: BufRead + Seek>(reader: &mut R, game_id: GameId) -> Result<Vec<u32>, Error> {
    let mut form_ids = Vec::new();
    let mut header_buf = [0; 24]; // 24 bytes is the largest group/record header size.

    while !reader.fill_buf()?.is_empty() {
        Group::read_form_ids(reader, game_id, &mut form_ids, &mut header_buf)?;
    }

    Ok(form_ids)
}

fn parse_morrowind_record_ids(input: &[u8]) -> IResult<&[u8], RecordIds> {
    let mut record_ids = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        let (input, record_id) = Record::parse_record_id(remaining_input, GameId::Morrowind)?;
        remaining_input = input;

        if let Some(RecordId::NamespacedId(record_id)) = record_id {
            record_ids.push(record_id);
        }
    }

    record_ids.sort();

    Ok((remaining_input, record_ids.into()))
}

fn read_morrowind_record_ids<R: BufRead + Seek>(reader: &mut R) -> Result<RecordIds, Error> {
    let mut record_ids = Vec::new();
    let mut header_buf = [0; 16]; // Morrowind record headers are 16 bytes long.

    while !reader.fill_buf()?.is_empty() {
        let (_, record_id) =
            Record::read_record_id(reader, GameId::Morrowind, &mut header_buf, false)?;

        if let Some(RecordId::NamespacedId(record_id)) = record_id {
            record_ids.push(record_id);
        }
    }

    record_ids.sort();

    Ok(record_ids.into())
}

fn parse_record_ids<'a>(
    input: &'a [u8],
    game_id: GameId,
    header_record: &Record,
    filename: &str,
) -> IResult<&'a [u8], RecordIds> {
    if game_id == GameId::Morrowind {
        parse_morrowind_record_ids(input)
    } else {
        // Map to the Alpha error kind even though it's misleading, because it
        // doesn't actually matter what is chosen, the detail is discarded.
        let masters = masters(header_record).map_err(|_| {
            nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alpha))
        })?;

        let (remaining_input, form_ids) = parse_form_ids(input, game_id)?;
        let form_ids = hashed_form_ids(&form_ids, filename, &masters).into();

        Ok((remaining_input, form_ids))
    }
}

fn read_record_ids<R: BufRead + Seek>(
    reader: &mut R,
    game_id: GameId,
    header_record: &Record,
    filename: &str,
) -> Result<RecordIds, Error> {
    if game_id == GameId::Morrowind {
        read_morrowind_record_ids(reader)
    } else {
        let masters = masters(header_record)?;

        let form_ids = read_form_ids(reader, game_id)?;
        let form_ids = hashed_form_ids(&form_ids, filename, &masters).into();

        Ok(form_ids)
    }
}

fn parse_plugin<'a>(
    input: &'a [u8],
    game_id: GameId,
    filename: &str,
    load_header_only: bool,
) -> IResult<&'a [u8], PluginData> {
    let (input1, header_record) = Record::parse(input, game_id, false)?;

    if load_header_only {
        return Ok((
            input1,
            PluginData {
                header_record,
                record_ids: RecordIds::None,
            },
        ));
    }

    let (input2, record_ids) = parse_record_ids(input1, game_id, &header_record, filename)?;

    Ok((
        input2,
        PluginData {
            header_record,
            record_ids,
        },
    ))
}

fn read_plugin<R: BufRead + Seek>(
    reader: &mut R,
    game_id: GameId,
    filename: &str,
    load_header_only: bool,
    expected_header_type: &'static [u8],
) -> Result<PluginData, Error> {
    let header_record = Record::read(reader, game_id, false)?;

    if header_record.header_type() != expected_header_type {
        return Err(Error::ParsingError(
            header_record.header_type().to_vec(),
            ParsingErrorKind::UnexpectedRecordType(expected_header_type.to_vec()),
        ));
    }

    if load_header_only {
        return Ok(PluginData {
            header_record,
            record_ids: RecordIds::None,
        });
    }

    let record_ids = read_record_ids(reader, game_id, &header_record, filename)?;

    Ok(PluginData {
        header_record,
        record_ids,
    })
}

#[cfg(test)]
mod tests {
    use std::fs::copy;
    use tempfile::tempdir;

    use super::*;

    mod morrowind {
        use super::*;

        use std::collections::HashSet;
        use std::iter::FromIterator;

        #[test]
        fn parse_file_should_succeed() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            match plugin.data.record_ids {
                RecordIds::NamespacedIds(ids) => assert_eq!(10, ids.len()),
                _ => panic!("Expected namespaced record IDs"),
            }
        }

        #[test]
        fn plugin_parse_should_read_a_unique_id_for_each_record() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            match plugin.data.record_ids {
                RecordIds::NamespacedIds(ids) => {
                    let set: HashSet<NamespacedId> = HashSet::from_iter(ids.iter().cloned());
                    assert_eq!(set.len(), ids.len());
                }
                _ => panic!("Expected namespaced record IDs"),
            }
        }

        #[test]
        fn parse_file_header_only_should_not_store_record_ids() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            let result = plugin.parse_file(true);

            assert!(result.is_ok());

            assert_eq!(RecordIds::None, plugin.data.record_ids);
        }

        #[test]
        fn game_id_should_return_the_plugins_associated_game_id() {
            let plugin = Plugin::new(GameId::Morrowind, &Path::new("Data/Blank.esm"));

            assert_eq!(&GameId::Morrowind, plugin.game_id());
        }

        #[test]
        fn is_master_file_should_be_true_for_plugin_with_master_flag_set() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_master_file_should_be_false_for_plugin_without_master_flag_set() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esp"),
            );

            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());
        }

        #[test]
        fn is_master_file_should_ignore_file_extension() {
            let tmp_dir = tempdir().unwrap();

            let esm = tmp_dir.path().join("Blank.esm");
            copy(
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esp"),
                &esm,
            )
            .unwrap();

            let mut plugin = Plugin::new(GameId::Morrowind, &esm);

            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());
        }

        #[test]
        fn is_light_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esl"));
            assert!(!plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Morrowind, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
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
        fn record_and_group_count_should_match_record_ids_length() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );

            assert!(plugin.record_and_group_count().is_none());
            assert!(plugin.parse_file(false).is_ok());
            assert_eq!(10, plugin.record_and_group_count().unwrap());
            match plugin.data.record_ids {
                RecordIds::NamespacedIds(ids) => assert_eq!(10, ids.len()),
                _ => panic!("Expected namespaced record IDs"),
            }
        }

        #[test]
        fn count_override_records_should_return_0_even_when_override_records_are_present() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());
            assert_eq!(0, plugin.count_override_records());
        }

        #[test]
        fn overlaps_with_should_detect_when_two_plugins_have_a_record_with_the_same_id() {
            let mut plugin1 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Different.esm"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());

            assert!(plugin1.overlaps_with(&plugin1));
            assert!(!plugin1.overlaps_with(&plugin2));
        }

        #[test]
        fn overlap_size_should_only_count_each_record_once() {
            let mut plugin1 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());

            assert_eq!(4, plugin1.overlap_size(&[&plugin2, &plugin2]));
        }

        #[test]
        fn overlap_size_should_check_against_all_given_plugins() {
            let mut plugin1 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esp"),
            );
            let mut plugin3 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin3.parse_file(false).is_ok());

            assert_eq!(4, plugin1.overlap_size(&[&plugin2, &plugin3]));
        }

        #[test]
        fn overlap_size_should_return_0_if_plugins_have_not_been_parsed() {
            let mut plugin1 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]));

            assert!(plugin1.parse_file(false).is_ok());

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]));

            assert!(plugin2.parse_file(false).is_ok());

            assert_ne!(0, plugin1.overlap_size(&[&plugin2]));
        }

        #[test]
        fn overlap_size_should_return_0_when_there_is_no_overlap() {
            let mut plugin1 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank.esp"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());

            assert!(!plugin1.overlaps_with(&plugin2));
            assert_eq!(0, plugin1.overlap_size(&[&plugin2]));
        }

        #[test]
        fn valid_light_form_id_range_should_be_empty() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Morrowind,
                Path::new("testing-plugins/Morrowind/Data Files/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_plugin());
        }
    }

    mod oblivion {
        use super::super::*;

        #[test]
        fn is_light_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esl"));
            assert!(!plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Oblivion, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
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
        fn valid_light_form_id_range_should_be_empty() {
            let mut plugin = Plugin::new(
                GameId::Oblivion,
                Path::new("testing-plugins/Oblivion/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Oblivion,
                Path::new("testing-plugins/Oblivion/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_plugin());
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

            match plugin.data.record_ids {
                RecordIds::FormIds(ids) => assert_eq!(10, ids.len()),
                _ => panic!("Expected hashed FormIDs"),
            }
        }

        #[test]
        fn parse_file_header_only_should_not_store_form_ids() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );

            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(RecordIds::None, plugin.data.record_ids);
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
        fn is_light_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esl"));
            assert!(!plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Skyrim, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
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
        fn overlap_size_should_only_count_each_record_once() {
            let mut plugin1 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());

            assert_eq!(4, plugin1.overlap_size(&[&plugin2, &plugin2]));
        }

        #[test]
        fn overlap_size_should_check_against_all_given_plugins() {
            let mut plugin1 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
            );
            let mut plugin3 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esp"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin3.parse_file(false).is_ok());

            assert_eq!(2, plugin1.overlap_size(&[&plugin2, &plugin3]));
        }

        #[test]
        fn overlap_size_should_return_0_if_plugins_have_not_been_parsed() {
            let mut plugin1 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]));

            assert!(plugin1.parse_file(false).is_ok());

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]));

            assert!(plugin2.parse_file(false).is_ok());

            assert_ne!(0, plugin1.overlap_size(&[&plugin2]));
        }

        #[test]
        fn overlap_size_should_return_0_when_there_is_no_overlap() {
            let mut plugin1 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank.esp"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());

            assert!(!plugin1.overlaps_with(&plugin2));
            assert_eq!(0, plugin1.overlap_size(&[&plugin2]));
        }

        #[test]
        fn valid_light_form_id_range_should_be_empty() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Skyrim,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_plugin());
        }
    }

    mod skyrimse {
        use super::*;

        use std::fs::read;

        #[test]
        fn is_master_file_should_use_file_extension_and_flag() {
            let tmp_dir = tempdir().unwrap();

            let master_flagged_esp = tmp_dir.path().join("Blank.esp");
            copy(
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
                &master_flagged_esp,
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

            let mut plugin = Plugin::new(GameId::SkyrimSE, &master_flagged_esp);
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_plugins_with_an_esl_file_extension() {
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl"));
            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_a_ghosted_esl_file() {
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl.ghost"));
            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_an_esp_file_with_the_light_flag_set() {
            let tmp_dir = tempdir().unwrap();

            let light_flagged_esp = tmp_dir.path().join("Blank.esp");
            copy(
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl"),
                &light_flagged_esp,
            )
            .unwrap();

            let mut plugin = Plugin::new(GameId::SkyrimSE, &light_flagged_esp);
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_light_plugin());
            assert!(!plugin.is_master_file());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_an_esm_file_with_the_light_flag_set() {
            let tmp_dir = tempdir().unwrap();

            let light_flagged_esm = tmp_dir.path().join("Blank.esm");
            copy(
                Path::new("testing-plugins/SkyrimSE/Data/Blank.esl"),
                &light_flagged_esm,
            )
            .unwrap();

            let mut plugin = Plugin::new(GameId::SkyrimSE, &light_flagged_esm);
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_light_plugin());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::SkyrimSE, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
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
        fn valid_light_form_id_range_should_be_0x800_to_0xfff_if_hedr_version_is_less_than_1_71() {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0x800, range.start());
            assert_eq!(&0xFFF, range.end());
        }

        #[test]
        fn valid_light_form_id_range_should_be_0_to_0xfff_if_hedr_version_is_1_71_or_greater() {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            let mut bytes = read(plugin.path()).unwrap();

            assert_eq!(0xD7, bytes[0x1E]);
            assert_eq!(0xA3, bytes[0x1F]);
            assert_eq!(0x70, bytes[0x20]);
            assert_eq!(0x3F, bytes[0x21]);
            bytes[0x1E] = 0x48;
            bytes[0x1F] = 0xE1;
            bytes[0x20] = 0xDA;
            bytes[0x21] = 0x3F;

            assert!(plugin.parse(&bytes, false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0xFFF, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::SkyrimSE,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.is_valid_as_light_plugin());
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_true_if_the_plugin_has_an_override_form_id_outside_the_valid_range(
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

            assert!(plugin.is_valid_as_light_plugin());
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_false_if_the_plugin_has_a_new_form_id_greater_than_0xfff(
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

            assert!(!plugin.is_valid_as_light_plugin());
        }
    }

    mod fallout3 {
        use super::super::*;

        #[test]
        fn is_light_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esl"));
            assert!(!plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Fallout3, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
        }

        #[test]
        fn valid_light_form_id_range_should_be_empty() {
            let mut plugin = Plugin::new(
                GameId::Fallout3,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::Fallout3,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_plugin());
        }
    }

    mod falloutnv {
        use super::super::*;

        #[test]
        fn is_light_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esl"));
            assert!(!plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::FalloutNV, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
        }

        #[test]
        fn valid_light_form_id_range_should_be_empty() {
            let mut plugin = Plugin::new(
                GameId::Fallout3,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_always_be_false() {
            let mut plugin = Plugin::new(
                GameId::FalloutNV,
                Path::new("testing-plugins/Skyrim/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(!plugin.is_valid_as_light_plugin());
        }
    }

    mod fallout4 {
        use super::*;

        use std::fs::read;

        #[test]
        fn is_master_file_should_use_file_extension_and_flag() {
            let tmp_dir = tempdir().unwrap();

            let master_flagged_esp = tmp_dir.path().join("Blank.esp");
            copy(
                Path::new("testing-plugins/Skyrim/Data/Blank.esm"),
                &master_flagged_esp,
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

            let mut plugin = Plugin::new(GameId::Fallout4, &master_flagged_esp);
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_plugins_with_an_esl_file_extension() {
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl"));
            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_a_ghosted_esl_file() {
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl.ghost"));
            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false() {
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Fallout4, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
        }

        #[test]
        fn valid_light_form_id_range_should_be_1_to_0xfff_if_hedr_version_is_less_than_1() {
            let mut plugin = Plugin::new(
                GameId::Fallout4,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0x800, range.start());
            assert_eq!(&0xFFF, range.end());
        }

        #[test]
        fn valid_light_form_id_range_should_be_1_to_0xfff_if_hedr_version_is_1_or_greater() {
            let mut plugin = Plugin::new(
                GameId::Fallout4,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            let mut bytes = read(plugin.path()).unwrap();

            assert_eq!(0xD7, bytes[0x1E]);
            assert_eq!(0xA3, bytes[0x1F]);
            assert_eq!(0x70, bytes[0x20]);
            assert_eq!(0x3F, bytes[0x21]);
            bytes[0x1E] = 0;
            bytes[0x1F] = 0;
            bytes[0x20] = 0x80;
            bytes[0x21] = 0x3F;

            assert!(plugin.parse(&bytes, false).is_ok());

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&1, range.start());
            assert_eq!(&0xFFF, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::Fallout4,
                Path::new("testing-plugins/SkyrimSE/Data/Blank - Master Dependent.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.is_valid_as_light_plugin());
        }
    }

    mod starfield {
        use super::*;

        #[test]
        fn is_master_file_should_use_file_extension_and_flag() {
            let tmp_dir = tempdir().unwrap();

            let master_flagged_esp = tmp_dir.path().join("Blank.esp");
            copy(
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
                &master_flagged_esp,
            )
            .unwrap();

            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esp"));
            assert!(!plugin.is_master_file());

            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esm"));
            assert!(plugin.is_master_file());

            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esl"));
            assert!(plugin.is_master_file());

            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.esp"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_master_file());

            let mut plugin = Plugin::new(GameId::Starfield, &master_flagged_esp);
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_master_file());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_plugins_with_an_esl_file_extension_if_the_override_flag_is_not_set(
        ) {
            // The flag won't be set because no data is loaded.
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esp"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esm"));
            assert!(!plugin.is_light_plugin());
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esl"));
            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_light_plugin_should_be_false_for_a_plugin_with_the_override_flag_set_and_an_esl_extension(
        ) {
            let tmp_dir = tempdir().unwrap();
            let esl_path = tmp_dir.path().join("Blank.esl");
            copy(
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
                &esl_path,
            )
            .unwrap();

            let mut plugin = Plugin::new(GameId::Starfield, &esl_path);
            plugin.parse_file(true).unwrap();

            assert!(!plugin.is_light_plugin());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_a_ghosted_esl_file() {
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esl.ghost"));
            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_light_plugin_should_be_true_for_a_plugin_with_the_light_flag_set() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.small.esm"),
            );
            plugin.parse_file(true).unwrap();

            assert!(plugin.is_light_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false_for_a_plugin_without_the_medium_flag_set() {
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esp"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esm"));
            assert!(!plugin.is_medium_plugin());
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esl"));
            assert!(!plugin.is_medium_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_true_for_a_plugin_with_the_medium_flag_set() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.medium.esm"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(plugin.is_medium_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false_for_a_plugin_with_the_medium_and_light_flags_set() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.small.esm"),
            );
            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_medium_plugin());
        }

        #[test]
        fn is_medium_plugin_should_be_false_for_an_esl_plugin_with_the_medium_flag_set() {
            let tmp_dir = tempdir().unwrap();
            let path = tmp_dir.path().join("Blank.esl");
            copy(
                Path::new("testing-plugins/Starfield/Data/Blank.medium.esm"),
                &path,
            )
            .unwrap();

            let mut plugin = Plugin::new(GameId::Starfield, &path);
            assert!(plugin.parse_file(true).is_ok());
            assert!(!plugin.is_medium_plugin());
        }

        #[test]
        fn is_override_plugin_should_be_true_for_a_plugin_with_the_override_flag_set_and_at_least_one_master_and_no_light_flag(
        ) {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.full.esm"),
            );
            plugin.parse_file(true).unwrap();

            assert!(plugin.is_override_plugin());
        }

        #[test]
        fn is_override_plugin_should_be_false_for_a_plugin_with_the_override_flag_set_and_no_masters(
        ) {
            let mut plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esm"));
            let file_data = &[
                0x54, 0x45, 0x53, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0xB2, 0x2E, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00,
            ];
            plugin.data.header_record = Record::parse(file_data, GameId::Starfield, false)
                .unwrap()
                .1;

            assert!(!plugin.is_override_plugin());
        }

        #[test]
        fn is_override_plugin_should_be_false_for_a_plugin_with_the_override_and_light_flags_set() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.small.esm"),
            );
            plugin.parse_file(true).unwrap();

            assert!(!plugin.is_override_plugin());
        }

        #[test]
        fn valid_light_form_id_range_should_be_0_to_0xfff() {
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esp"));

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0xFFF, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.is_valid_as_light_plugin());
        }

        #[test]
        fn is_valid_as_override_plugin_should_be_true_if_the_plugin_has_no_new_records() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.is_valid_as_override_plugin());
        }

        #[test]
        fn is_valid_as_override_plugin_should_be_false_if_the_plugin_has_new_records() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            assert!(!plugin.is_valid_as_override_plugin());
        }
    }

    fn write_invalid_plugin(path: &Path) {
        use std::io::Write;
        let mut file = File::create(path).unwrap();
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

        let result = plugin.parse_file(false);
        assert!(result.is_err());
        assert_eq!(
            "failed to fill whole buffer",
            result.unwrap_err().to_string()
        );
    }

    #[test]
    fn parse_file_header_only_should_fail_for_a_non_plugin_file() {
        let tmp_dir = tempdir().unwrap();

        let path = tmp_dir.path().join("Invalid.esm");
        write_invalid_plugin(&path);

        let mut plugin = Plugin::new(GameId::Skyrim, &path);

        let result = plugin.parse_file(true);
        assert!(result.is_err());
        assert_eq!("An error was encountered while parsing the plugin content [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00]: Expected record type [54, 45, 53, 34]", result.unwrap_err().to_string());
    }

    #[test]
    fn parse_file_should_fail_for_a_non_plugin_file() {
        let tmp_dir = tempdir().unwrap();

        let path = tmp_dir.path().join("Invalid.esm");
        write_invalid_plugin(&path);

        let mut plugin = Plugin::new(GameId::Skyrim, &path);

        let result = plugin.parse_file(false);
        assert!(result.is_err());
        assert_eq!("An error was encountered while parsing the plugin content [00, 00, 00, 00]: Expected record type [54, 45, 53, 34]", result.unwrap_err().to_string());
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

        let result = plugin.description();
        assert!(result.is_err());
        assert_eq!("An error was encountered while parsing the plugin content [9A, 99, 99, 3F, 01, 00, 00, 00]: Subrecord data field too short, expected at least 40 bytes", result.unwrap_err().to_string());
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

    #[test]
    fn hash_should_treat_plugin_names_case_insensitively_like_windows() {
        // \u03a1 is greek rho uppercase 'Ρ'
        // \u03c1 is greek rho lowercase 'ρ'
        // \u03f1 is greek rho 'ϱ'
        // \u0130 is turkish 'İ'
        // \u0131 is turkish 'ı'

        // I and i are the same, but İ and ı are different to them and each
        // other.
        assert_eq!(hash("i"), hash("I"));
        assert_ne!(hash("\u{0130}"), hash("i"));
        assert_ne!(hash("\u{0131}"), hash("i"));
        assert_ne!(hash("\u{0131}"), hash("\u{0130}"));

        // Windows filesystem treats Ρ and ρ as the same, but ϱ is different.
        assert_eq!(hash("\u{03a1}"), hash("\u{03c1}"));
        assert_ne!(hash("\u{03a1}"), hash("\u{03f1}"));

        // hash uses str::to_lowercase() internally, because unlike
        // str::to_uppercase() it has the desired behaviour. The asserts below
        // demonstrate that.

        // Check how greek rho Ρ case transforms.
        assert_eq!("\u{03c1}", "\u{03a1}".to_lowercase());
        assert_eq!("\u{03a1}", "\u{03a1}".to_uppercase());

        // Check how greek rho ρ case transforms.
        assert_eq!("\u{03c1}", "\u{03c1}".to_lowercase());
        assert_eq!("\u{03a1}", "\u{03c1}".to_uppercase());

        // Check how greek rho ϱ case transforms.
        assert_eq!("\u{03f1}", "\u{03f1}".to_lowercase());
        assert_eq!("\u{03a1}", "\u{03f1}".to_uppercase());

        // Check how turkish İ case transforms.
        assert_eq!("i\u{0307}", "\u{0130}".to_lowercase());
        assert_eq!("\u{0130}", "\u{0130}".to_uppercase());

        // Check how turkish ı case transforms.
        assert_eq!("\u{0131}", "\u{0131}".to_lowercase());
        assert_eq!("I", "\u{0131}".to_uppercase());
    }
}

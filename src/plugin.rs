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
use crate::form_id::{HashedFormId, ObjectIndexMask, SourcePlugin};
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
    FormIds(Vec<u32>),
    ResolvedFormIds(Vec<HashedFormId>),
    NamespacedIds(Vec<NamespacedId>),
}

impl From<Vec<NamespacedId>> for RecordIds {
    fn from(record_ids: Vec<NamespacedId>) -> RecordIds {
        RecordIds::NamespacedIds(record_ids)
    }
}

impl From<Vec<u32>> for RecordIds {
    fn from(form_ids: Vec<u32>) -> RecordIds {
        RecordIds::FormIds(form_ids)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
struct PluginData {
    header_record: Record,
    record_ids: RecordIds,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
enum PluginScale {
    Full,
    Medium,
    Small,
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
        self.data = parse_plugin(input, self.game_id, load_header_only)?.1;

        if self.game_id != GameId::Starfield {
            self.resolve_record_ids(&[])?;
        }

        Ok(())
    }

    fn read<R: BufRead + Seek>(
        &mut self,
        mut reader: R,
        load_header_only: bool,
        expected_header_type: &'static [u8],
    ) -> Result<(), Error> {
        self.data = read_plugin(
            &mut reader,
            self.game_id,
            load_header_only,
            expected_header_type,
        )?;

        if self.game_id != GameId::Starfield {
            self.resolve_record_ids(&[])?;
        }

        Ok(())
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

    /// plugins_metadata can be empty for all games other than Starfield, and for Starfield plugins with no masters.
    pub fn resolve_record_ids(&mut self, plugins_metadata: &[PluginMetadata]) -> Result<(), Error> {
        if let RecordIds::FormIds(ref form_ids) = self.data.record_ids {
            let filename = self
                .filename()
                .ok_or_else(|| Error::NoFilename(self.path.clone()))?;
            let parent_metadata = PluginMetadata {
                filename,
                scale: self.scale(),
            };
            let masters = self.masters()?;

            let form_ids = hashed_form_ids(
                self.game_id,
                form_ids,
                parent_metadata,
                &masters,
                plugins_metadata,
            )?;

            self.data.record_ids = RecordIds::ResolvedFormIds(form_ids);
        }

        Ok(())
    }

    pub fn game_id(&self) -> GameId {
        self.game_id
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

    fn scale(&self) -> PluginScale {
        if self.is_light_plugin() {
            PluginScale::Small
        } else if self.is_medium_plugin() {
            PluginScale::Medium
        } else {
            PluginScale::Full
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
        // if the plugin's light or medium flags are set.
        self.is_override_flag_set()
            && !self.is_light_flag_set()
            && !self.is_medium_flag_set()
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

    pub fn count_override_records(&self) -> Result<usize, Error> {
        match &self.data.record_ids {
            RecordIds::None => Ok(0),
            RecordIds::FormIds(_) => Err(Error::UnresolvedFormIds(self.path.clone())),
            RecordIds::ResolvedFormIds(form_ids) => {
                let count = form_ids.iter().filter(|f| f.is_overridden_record()).count();
                Ok(count)
            }
            RecordIds::NamespacedIds(_) => Ok(0), // FIXME: This gives misleading results for Morrowind plugins.
        }
    }

    pub fn overlaps_with(&self, other: &Self) -> Result<bool, Error> {
        use self::RecordIds::*;
        match (&self.data.record_ids, &other.data.record_ids) {
            (FormIds(_), _) => Err(Error::UnresolvedFormIds(self.path.clone())),
            (_, FormIds(_)) => Err(Error::UnresolvedFormIds(other.path.clone())),
            (ResolvedFormIds(left), ResolvedFormIds(right)) => {
                Ok(sorted_slices_intersect(left, right))
            }
            (NamespacedIds(left), NamespacedIds(right)) => Ok(sorted_slices_intersect(left, right)),
            _ => Ok(false),
        }
    }

    /// Count the number of records that appear in this plugin and one or more
    /// the others passed. If more than one other contains the same record, it
    /// is only counted once.
    pub fn overlap_size(&self, others: &[&Self]) -> Result<usize, Error> {
        use self::RecordIds::*;

        match &self.data.record_ids {
            FormIds(_) => Err(Error::UnresolvedFormIds(self.path.clone())),
            ResolvedFormIds(ids) => {
                let mut count = 0;
                for id in ids {
                    for other in others {
                        match &other.data.record_ids {
                            FormIds(_) => return Err(Error::UnresolvedFormIds(other.path.clone())),
                            ResolvedFormIds(master_ids) if master_ids.binary_search(id).is_ok() => {
                                count += 1;
                                break;
                            }
                            _ => {
                                // Do nothing.
                            }
                        }
                    }
                }

                Ok(count)
            }
            NamespacedIds(ids) => {
                let count = ids
                    .iter()
                    .filter(|id| {
                        others.iter().any(|other| match &other.data.record_ids {
                            NamespacedIds(master_ids) => master_ids.binary_search(id).is_ok(),
                            _ => false,
                        })
                    })
                    .count();
                Ok(count)
            }
            None => Ok(0),
        }
    }

    #[deprecated = "This has been renamed to Plugin::is_valid_as_light_plugin() for clarity."]
    pub fn is_valid_as_light_master(&self) -> Result<bool, Error> {
        self.is_valid_as_light_plugin()
    }

    pub fn is_valid_as_light_plugin(&self) -> Result<bool, Error> {
        if self.game_id.supports_light_plugins() {
            match &self.data.record_ids {
                RecordIds::None => Ok(true),
                RecordIds::FormIds(_) => Err(Error::UnresolvedFormIds(self.path.clone())),
                RecordIds::ResolvedFormIds(form_ids) => {
                    let valid_range = self.valid_light_form_id_range();

                    let is_valid = form_ids
                        .iter()
                        .filter(|f| !f.is_overridden_record())
                        .all(|f| valid_range.contains(&f.object_index()));

                    Ok(is_valid)
                }
                RecordIds::NamespacedIds(_) => Ok(false),
            }
        } else {
            Ok(false)
        }
    }

    pub fn is_valid_as_medium_plugin(&self) -> Result<bool, Error> {
        if self.game_id.supports_medium_plugins() {
            match &self.data.record_ids {
                RecordIds::None => Ok(true),
                RecordIds::FormIds(_) => Err(Error::UnresolvedFormIds(self.path.clone())),
                RecordIds::ResolvedFormIds(form_ids) => {
                    let valid_range = self.valid_medium_form_id_range();

                    let is_valid = form_ids
                        .iter()
                        .filter(|f| !f.is_overridden_record())
                        .all(|f| valid_range.contains(&f.object_index()));

                    Ok(is_valid)
                }
                RecordIds::NamespacedIds(_) => Ok(false), // this should never happen.
            }
        } else {
            Ok(false)
        }
    }

    pub fn is_valid_as_override_plugin(&self) -> Result<bool, Error> {
        if self.game_id == GameId::Starfield {
            // If an override plugin has a record that does not override an existing record, that
            // record is placed into the mod index of the plugin's first master, which risks
            // overwriting an unrelated record with the same object index, so treat that case as
            // invalid.
            match &self.data.record_ids {
                RecordIds::None => Ok(true),
                RecordIds::FormIds(_) => Err(Error::UnresolvedFormIds(self.path.clone())),
                RecordIds::ResolvedFormIds(form_ids) => {
                    Ok(form_ids.iter().all(HashedFormId::is_overridden_record))
                }
                RecordIds::NamespacedIds(_) => Ok(false), // this should never happen.
            }
        } else {
            Ok(false)
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

    fn valid_medium_form_id_range(&self) -> RangeInclusive<u32> {
        match self.game_id {
            GameId::Starfield => 0..=0xFFFF,
            _ => 0..=0,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PluginMetadata {
    filename: String,
    scale: PluginScale,
}

// Get PluginMetadata objects for a collection of loaded plugins.
pub fn plugins_metadata(plugins: &[&Plugin]) -> Result<Vec<PluginMetadata>, Error> {
    let mut vec = Vec::new();

    for plugin in plugins {
        let filename = plugin
            .filename()
            .ok_or_else(|| Error::NoFilename(plugin.path().to_path_buf()))?;

        let metadata = PluginMetadata {
            filename,
            scale: plugin.scale(),
        };

        vec.push(metadata);
    }

    Ok(vec)
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

fn hashed_form_ids(
    game_id: GameId,
    form_ids: &[u32],
    plugin_metadata: PluginMetadata,
    masters: &[String],
    other_plugins_metadata: &[PluginMetadata],
) -> Result<Vec<HashedFormId>, Error> {
    let hashed_parent = hashed_parent(game_id, plugin_metadata);
    let hashed_masters = match game_id {
        GameId::Starfield => hashed_masters_for_starfield(masters, other_plugins_metadata)?,
        _ => hashed_masters(masters),
    };

    let mut form_ids: Vec<_> = form_ids
        .iter()
        .map(|form_id| HashedFormId::new(hashed_parent, &hashed_masters, *form_id))
        .collect();

    form_ids.sort();

    Ok(form_ids)
}

fn hashed_parent(game_id: GameId, parent_metadata: PluginMetadata) -> SourcePlugin {
    match game_id {
        GameId::Starfield => {
            // The Creation Kit can create plugins that contain new records that use mod indexes that don't match the plugin's scale (full/medium/small), e.g. a medium plugin might have new records with FormIDs that don't start with 0xFD. However, at runtime the mod index part is replaced with the mod index of the plugin, according to the plugin's scale, so the plugin's scale is what matters when resolving FormIDs for comparison between plugins.
            let object_index_mask = match parent_metadata.scale {
                PluginScale::Full => ObjectIndexMask::Full,
                PluginScale::Medium => ObjectIndexMask::Medium,
                PluginScale::Small => ObjectIndexMask::Small,
            };
            SourcePlugin::parent(&parent_metadata.filename, object_index_mask)
        }
        // The full object index mask is used for all plugin scales in other games.
        _ => SourcePlugin::parent(&parent_metadata.filename, ObjectIndexMask::Full),
    }
}

fn hashed_masters(masters: &[String]) -> Vec<SourcePlugin> {
    masters
        .iter()
        .enumerate()
        .map(|(i, m)| {
            let mod_index_mask = u32::try_from(i << 24).expect("master index to fit in a u32");
            SourcePlugin::master(m, mod_index_mask, ObjectIndexMask::Full)
        })
        .collect()
}

// Get HashedMaster objects for the current plugin.
fn hashed_masters_for_starfield(
    masters: &[String],
    masters_metadata: &[PluginMetadata],
) -> Result<Vec<SourcePlugin>, Error> {
    let mut hashed_masters = Vec::new();
    let mut full_mask = 0;
    let mut medium_mask = 0xFD00_0000;
    let mut small_mask = 0xFE00_0000;

    for master in masters {
        let master_scale = masters_metadata
            .iter()
            .find(|m| unicase::eq(&m.filename, master))
            .map(|m| m.scale)
            .ok_or_else(|| Error::PluginMetadataNotFound(master.clone()))?;

        match master_scale {
            PluginScale::Full => {
                hashed_masters.push(SourcePlugin::master(
                    master,
                    full_mask,
                    ObjectIndexMask::Full,
                ));

                full_mask += 0x0100_0000;
            }
            PluginScale::Medium => {
                hashed_masters.push(SourcePlugin::master(
                    master,
                    medium_mask,
                    ObjectIndexMask::Medium,
                ));

                medium_mask += 0x0001_0000;
            }
            PluginScale::Small => {
                hashed_masters.push(SourcePlugin::master(
                    master,
                    small_mask,
                    ObjectIndexMask::Small,
                ));

                small_mask += 0x0000_1000;
            }
        }
    }

    Ok(hashed_masters)
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

fn parse_record_ids(input: &[u8], game_id: GameId) -> IResult<&[u8], RecordIds> {
    if game_id == GameId::Morrowind {
        parse_morrowind_record_ids(input)
    } else {
        let (remaining_input, form_ids) = parse_form_ids(input, game_id)?;

        Ok((remaining_input, form_ids.into()))
    }
}

fn read_record_ids<R: BufRead + Seek>(reader: &mut R, game_id: GameId) -> Result<RecordIds, Error> {
    if game_id == GameId::Morrowind {
        read_morrowind_record_ids(reader)
    } else {
        read_form_ids(reader, game_id).map(Into::into)
    }
}

fn parse_plugin(
    input: &[u8],
    game_id: GameId,
    load_header_only: bool,
) -> IResult<&[u8], PluginData> {
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

    let (input2, record_ids) = parse_record_ids(input1, game_id)?;

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

    let record_ids = read_record_ids(reader, game_id)?;

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

            assert_eq!(GameId::Morrowind, plugin.game_id());
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
            assert_eq!(0, plugin.count_override_records().unwrap());
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

            assert!(plugin1.overlaps_with(&plugin1).unwrap());
            assert!(!plugin1.overlaps_with(&plugin2).unwrap());
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

            assert_eq!(4, plugin1.overlap_size(&[&plugin2, &plugin2]).unwrap());
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

            assert_eq!(4, plugin1.overlap_size(&[&plugin2, &plugin3]).unwrap());
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

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]).unwrap());

            assert!(plugin1.parse_file(false).is_ok());

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]).unwrap());

            assert!(plugin2.parse_file(false).is_ok());

            assert_ne!(0, plugin1.overlap_size(&[&plugin2]).unwrap());
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

            assert!(!plugin1.overlaps_with(&plugin2).unwrap());
            assert_eq!(0, plugin1.overlap_size(&[&plugin2]).unwrap());
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
            assert!(!plugin.is_valid_as_light_plugin().unwrap());
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
            assert!(!plugin.is_valid_as_light_plugin().unwrap());
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
                RecordIds::ResolvedFormIds(ids) => assert_eq!(10, ids.len()),
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

            assert_eq!(GameId::Skyrim, plugin.game_id());
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
            assert_eq!(2, plugin.count_override_records().unwrap());
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

            assert!(plugin1.overlaps_with(&plugin1).unwrap());
            assert!(!plugin1.overlaps_with(&plugin2).unwrap());
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

            assert_eq!(4, plugin1.overlap_size(&[&plugin2, &plugin2]).unwrap());
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

            assert_eq!(2, plugin1.overlap_size(&[&plugin2, &plugin3]).unwrap());
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

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]).unwrap());

            assert!(plugin1.parse_file(false).is_ok());

            assert_eq!(0, plugin1.overlap_size(&[&plugin2]).unwrap());

            assert!(plugin2.parse_file(false).is_ok());

            assert_ne!(0, plugin1.overlap_size(&[&plugin2]).unwrap());
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

            assert!(!plugin1.overlaps_with(&plugin2).unwrap());
            assert_eq!(0, plugin1.overlap_size(&[&plugin2]).unwrap());
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
            assert!(!plugin.is_valid_as_light_plugin().unwrap());
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

            assert!(plugin.is_valid_as_light_plugin().unwrap());
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

            assert!(plugin.is_valid_as_light_plugin().unwrap());
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

            assert!(!plugin.is_valid_as_light_plugin().unwrap());
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
            assert!(!plugin.is_valid_as_light_plugin().unwrap());
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
            assert!(!plugin.is_valid_as_light_plugin().unwrap());
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

            assert!(plugin.is_valid_as_light_plugin().unwrap());
        }
    }

    mod starfield {
        use super::*;

        #[test]
        fn parse_file_should_succeed() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            match plugin.data.record_ids {
                RecordIds::FormIds(ids) => assert_eq!(10, ids.len()),
                _ => panic!("Expected raw FormIDs"),
            }
        }

        #[test]
        fn resolve_record_ids_should_resolve_unresolved_form_ids() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.resolve_record_ids(&[]).is_ok());

            match plugin.data.record_ids {
                RecordIds::ResolvedFormIds(ids) => assert_eq!(10, ids.len()),
                _ => panic!("Expected resolved FormIDs"),
            }
        }

        #[test]
        fn resolve_record_ids_should_do_nothing_if_form_ids_are_already_resolved() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.resolve_record_ids(&[]).is_ok());

            let vec_ptr = match plugin.data.record_ids {
                RecordIds::ResolvedFormIds(ref ids) => ids.as_ptr(),
                _ => panic!("Expected resolved FormIDs"),
            };

            assert!(plugin.resolve_record_ids(&[]).is_ok());

            let vec_ptr_2 = match plugin.data.record_ids {
                RecordIds::ResolvedFormIds(ref ids) => ids.as_ptr(),
                _ => panic!("Expected resolved FormIDs"),
            };

            assert_eq!(vec_ptr, vec_ptr_2);
        }

        #[test]
        fn scale_should_return_full_for_a_full_plugin() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(PluginScale::Full, plugin.scale());
        }

        #[test]
        fn scale_should_return_medium_for_a_medium_plugin() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.medium.esm"),
            );
            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(PluginScale::Medium, plugin.scale());
        }

        #[test]
        fn scale_should_return_small_for_a_small_plugin() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.small.esm"),
            );
            assert!(plugin.parse_file(true).is_ok());

            assert_eq!(PluginScale::Small, plugin.scale());
        }

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
        fn is_override_plugin_should_be_false_for_a_plugin_with_the_override_and_medium_flags_set()
        {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.medium.esm"),
            );
            plugin.parse_file(true).unwrap();

            assert!(!plugin.is_override_plugin());
        }

        #[test]
        fn count_override_records_should_error_if_form_ids_are_unresolved() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            match plugin.count_override_records().unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn count_override_records_should_succeed_if_form_ids_are_resolved() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );

            assert!(plugin.parse_file(false).is_ok());

            assert!(plugin.resolve_record_ids(&[]).is_ok());

            assert_eq!(0, plugin.count_override_records().unwrap());
        }

        #[test]
        fn overlaps_with_should_error_if_form_ids_in_self_are_unresolved() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            let plugin1_metadata = PluginMetadata {
                filename: plugin1.filename().unwrap(),
                scale: plugin1.scale(),
            };

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin2.resolve_record_ids(&[plugin1_metadata]).is_ok());

            match plugin1.overlaps_with(&plugin2).unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin1.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn overlaps_with_should_error_if_form_ids_in_other_are_unresolved() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin1.resolve_record_ids(&[]).is_ok());

            match plugin1.overlaps_with(&plugin2).unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin2.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn overlaps_with_should_succeed_if_form_ids_are_resolved() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            let plugin1_metadata = PluginMetadata {
                filename: plugin1.filename().unwrap(),
                scale: plugin1.scale(),
            };

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin1.resolve_record_ids(&[]).is_ok());
            assert!(plugin2.resolve_record_ids(&[plugin1_metadata]).is_ok());

            assert!(plugin1.overlaps_with(&plugin2).unwrap());
        }

        #[test]
        fn overlap_size_should_error_if_form_ids_in_self_are_unresolved() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            let plugin1_metadata = PluginMetadata {
                filename: plugin1.filename().unwrap(),
                scale: plugin1.scale(),
            };

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin2.resolve_record_ids(&[plugin1_metadata]).is_ok());

            match plugin1.overlap_size(&[&plugin2]).unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin1.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn overlap_size_should_error_if_form_ids_in_other_are_unresolved() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin1.resolve_record_ids(&[]).is_ok());

            match plugin1.overlap_size(&[&plugin2]).unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin2.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn overlap_size_should_succeed_if_form_ids_are_resolved() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            let plugin1_metadata = PluginMetadata {
                filename: plugin1.filename().unwrap(),
                scale: plugin1.scale(),
            };

            assert!(plugin1.parse_file(false).is_ok());
            assert!(plugin2.parse_file(false).is_ok());
            assert!(plugin1.resolve_record_ids(&[]).is_ok());
            assert!(plugin2.resolve_record_ids(&[plugin1_metadata]).is_ok());

            assert_eq!(1, plugin1.overlap_size(&[&plugin2]).unwrap());
        }

        #[test]
        fn valid_light_form_id_range_should_be_0_to_0xfff() {
            let plugin = Plugin::new(GameId::Starfield, Path::new("Blank.esp"));

            let range = plugin.valid_light_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0xFFF, range.end());
        }

        #[test]
        fn valid_medium_form_id_range_should_be_0_to_0xffff() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.small.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            let range = plugin.valid_medium_form_id_range();
            assert_eq!(&0, range.start());
            assert_eq!(&0xFFFF, range.end());
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_false_if_form_ids_are_unresolved() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.small.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            match plugin.is_valid_as_light_plugin().unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn is_valid_as_light_plugin_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(plugin.resolve_record_ids(&[]).is_ok());

            assert!(plugin.is_valid_as_light_plugin().unwrap());
        }

        #[test]
        fn is_valid_as_medium_plugin_should_be_false_if_form_ids_are_unresolved() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.medium.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());

            match plugin.is_valid_as_medium_plugin().unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn is_valid_as_medium_plugin_should_be_true_if_the_plugin_has_no_form_ids_outside_the_valid_range(
        ) {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.medium.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(plugin.resolve_record_ids(&[]).is_ok());

            assert!(plugin.is_valid_as_medium_plugin().unwrap());
        }

        #[test]
        fn is_valid_as_override_plugin_should_be_false_if_form_ids_are_unresolved() {
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            assert!(plugin.parse_file(false).is_ok());

            match plugin.is_valid_as_override_plugin().unwrap_err() {
                Error::UnresolvedFormIds(path) => assert_eq!(plugin.path, path),
                _ => panic!("Expected unresolved FormIDs error"),
            }
        }

        #[test]
        fn is_valid_as_override_plugin_should_be_true_if_the_plugin_has_no_new_records() {
            let master_metadata = PluginMetadata {
                filename: "Blank.full.esm".to_string(),
                scale: PluginScale::Full,
            };
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank - Override.esp"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(plugin.resolve_record_ids(&[master_metadata]).is_ok());

            assert!(plugin.is_valid_as_override_plugin().unwrap());
        }

        #[test]
        fn is_valid_as_override_plugin_should_be_false_if_the_plugin_has_new_records() {
            let master_metadata = PluginMetadata {
                filename: "Blank.full.esm".to_string(),
                scale: PluginScale::Full,
            };
            let mut plugin = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            assert!(plugin.parse_file(false).is_ok());
            assert!(plugin.resolve_record_ids(&[master_metadata]).is_ok());

            assert!(!plugin.is_valid_as_override_plugin().unwrap());
        }

        #[test]
        fn plugins_metadata_should_return_plugin_names_and_scales() {
            let mut plugin1 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.full.esm"),
            );
            let mut plugin2 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.medium.esm"),
            );
            let mut plugin3 = Plugin::new(
                GameId::Starfield,
                Path::new("testing-plugins/Starfield/Data/Blank.small.esm"),
            );
            assert!(plugin1.parse_file(true).is_ok());
            assert!(plugin2.parse_file(true).is_ok());
            assert!(plugin3.parse_file(true).is_ok());

            let metadata = plugins_metadata(&[&plugin1, &plugin2, &plugin3]).unwrap();

            assert_eq!(
                vec![
                    PluginMetadata {
                        filename: "Blank.full.esm".to_string(),
                        scale: PluginScale::Full,
                    },
                    PluginMetadata {
                        filename: "Blank.medium.esm".to_string(),
                        scale: PluginScale::Medium,
                    },
                    PluginMetadata {
                        filename: "Blank.small.esm".to_string(),
                        scale: PluginScale::Small,
                    },
                ],
                metadata
            );
        }

        #[test]
        fn hashed_parent_should_use_full_object_index_mask_for_games_other_than_starfield() {
            let metadata = PluginMetadata {
                filename: "a".to_string(),
                scale: PluginScale::Full,
            };

            let plugin = hashed_parent(GameId::SkyrimSE, metadata);

            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.mod_index_mask);
            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.object_index_mask);

            let metadata = PluginMetadata {
                filename: "a".to_string(),
                scale: PluginScale::Medium,
            };

            let plugin = hashed_parent(GameId::SkyrimSE, metadata);

            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.mod_index_mask);
            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.object_index_mask);

            let metadata = PluginMetadata {
                filename: "a".to_string(),
                scale: PluginScale::Small,
            };

            let plugin = hashed_parent(GameId::SkyrimSE, metadata);

            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.mod_index_mask);
            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.object_index_mask);
        }

        #[test]
        fn hashed_parent_should_use_object_index_mask_matching_the_plugin_scale_for_starfield() {
            let metadata = PluginMetadata {
                filename: "a".to_string(),
                scale: PluginScale::Full,
            };

            let plugin = hashed_parent(GameId::Starfield, metadata);

            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.mod_index_mask);
            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.object_index_mask);

            let metadata = PluginMetadata {
                filename: "a".to_string(),
                scale: PluginScale::Medium,
            };

            let plugin = hashed_parent(GameId::Starfield, metadata);

            assert_eq!(u32::from(ObjectIndexMask::Medium), plugin.mod_index_mask);
            assert_eq!(u32::from(ObjectIndexMask::Medium), plugin.object_index_mask);

            let metadata = PluginMetadata {
                filename: "a".to_string(),
                scale: PluginScale::Small,
            };

            let plugin = hashed_parent(GameId::Starfield, metadata);

            assert_eq!(u32::from(ObjectIndexMask::Small), plugin.mod_index_mask);
            assert_eq!(u32::from(ObjectIndexMask::Small), plugin.object_index_mask);
        }

        #[test]
        fn hashed_masters_should_use_vec_index_as_mod_index() {
            let masters = &["a".to_string(), "b".to_string(), "c".to_string()];
            let hashed_masters = hashed_masters(masters);

            assert_eq!(
                vec![
                    SourcePlugin::master("a", 0, ObjectIndexMask::Full),
                    SourcePlugin::master("b", 0x0100_0000, ObjectIndexMask::Full),
                    SourcePlugin::master("c", 0x0200_0000, ObjectIndexMask::Full),
                ],
                hashed_masters
            );
        }

        #[test]
        fn hashed_masters_for_starfield_should_error_if_it_cannot_find_a_masters_metadata() {
            let masters = &["a".to_string(), "b".to_string(), "c".to_string()];
            let metadata = &[
                PluginMetadata {
                    filename: masters[0].clone(),
                    scale: PluginScale::Full,
                },
                PluginMetadata {
                    filename: masters[1].clone(),
                    scale: PluginScale::Full,
                },
            ];

            match hashed_masters_for_starfield(masters, metadata).unwrap_err() {
                Error::PluginMetadataNotFound(master) => assert_eq!(masters[2], master),
                _ => panic!("Expected plugin metadata not found error"),
            }
        }

        #[test]
        fn hashed_masters_for_starfield_should_match_names_to_metadata_case_insensitively() {
            let masters = &["a".to_string()];
            let metadata = &[PluginMetadata {
                filename: "A".to_string(),
                scale: PluginScale::Full,
            }];

            let hashed_masters = hashed_masters_for_starfield(masters, metadata).unwrap();

            assert_eq!(
                vec![SourcePlugin::master(&masters[0], 0, ObjectIndexMask::Full),],
                hashed_masters
            );
        }

        #[test]
        fn hashed_masters_for_starfield_should_count_mod_indexes_separately_for_different_plugin_scales(
        ) {
            let masters: Vec<_> = (0..7).into_iter().map(|i| i.to_string()).collect();
            let metadata = &[
                PluginMetadata {
                    filename: masters[0].clone(),
                    scale: PluginScale::Full,
                },
                PluginMetadata {
                    filename: masters[1].clone(),
                    scale: PluginScale::Medium,
                },
                PluginMetadata {
                    filename: masters[2].clone(),
                    scale: PluginScale::Small,
                },
                PluginMetadata {
                    filename: masters[3].clone(),
                    scale: PluginScale::Medium,
                },
                PluginMetadata {
                    filename: masters[4].clone(),
                    scale: PluginScale::Full,
                },
                PluginMetadata {
                    filename: masters[5].clone(),
                    scale: PluginScale::Small,
                },
                PluginMetadata {
                    filename: masters[6].clone(),
                    scale: PluginScale::Small,
                },
            ];

            let hashed_masters = hashed_masters_for_starfield(&masters, metadata).unwrap();

            assert_eq!(
                vec![
                    SourcePlugin::master(&masters[0], 0, ObjectIndexMask::Full),
                    SourcePlugin::master(&masters[1], 0xFD00_0000, ObjectIndexMask::Medium),
                    SourcePlugin::master(&masters[2], 0xFE00_0000, ObjectIndexMask::Small),
                    SourcePlugin::master(&masters[3], 0xFD01_0000, ObjectIndexMask::Medium),
                    SourcePlugin::master(&masters[4], 0x0100_0000, ObjectIndexMask::Full),
                    SourcePlugin::master(&masters[5], 0xFE00_1000, ObjectIndexMask::Small),
                    SourcePlugin::master(&masters[6], 0xFE00_2000, ObjectIndexMask::Small),
                ],
                hashed_masters
            );
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
        let form_ids = hashed_form_ids(
            GameId::SkyrimSE,
            &raw_form_ids,
            PluginMetadata {
                filename: "Blàñk.esp".to_string(),
                scale: PluginScale::Full,
            },
            &masters,
            &[],
        )
        .unwrap();

        let other_masters = vec!["TÉST.ESM".to_string()];
        let other_form_ids = hashed_form_ids(
            GameId::SkyrimSE,
            &raw_form_ids,
            PluginMetadata {
                filename: "BLÀÑK.ESP".to_string(),
                scale: PluginScale::Full,
            },
            &other_masters,
            &[],
        )
        .unwrap();

        assert_eq!(form_ids, other_form_ids);
    }
}

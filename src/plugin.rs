/*
 * This file is part of libespm
 *
 * Copyright (C) 2017 Oliver Hamlet
 *
 * libespm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libespm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libespm. If not, see <http://www.gnu.org/licenses/>.
 */

use std::borrow::Cow;
use std::collections::HashSet;
use std::io::Cursor;
use std::fs::File;
use std::io::BufReader;
use std::io::Error;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::str;

use byteorder::{LittleEndian, ReadBytesExt};

use encoding::{Encoding, DecoderTrap};
use encoding::all::WINDOWS_1252;

use nom::ErrorKind;
use nom::IError;
use nom::IResult;

use memmap::Mmap;
use memmap::Protection;

use form_id::FormId;
use game_id::GameId;
use group::Group;
use record::Record;

#[derive(Debug)]
pub enum ParsingError {
    NonUTF8FilePath,
    NonUTF8StringData,
    IOError(Error),
    NoFilename,
    ContentError(IError),
    DecodeError(Cow<'static, str>),
}

#[derive(Debug)]
pub struct PluginData {
    pub header_record: Record,
    pub form_ids: HashSet<FormId>,
}

impl PluginData {
    pub fn new() -> PluginData {
        PluginData {
            header_record: Record::new(),
            form_ids: HashSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct Plugin {
    game_id: GameId,
    path: PathBuf,
    pub data: PluginData,
}

impl Plugin {
    pub fn new(game_id: GameId, filepath: &Path) -> Plugin {
        Plugin {
            game_id: game_id,
            path: filepath.to_path_buf(),
            data: PluginData::new(),
        }
    }

    pub fn parse(&mut self, input: &[u8], load_header_only: bool) -> Result<(), ParsingError> {
        match self.filename() {
            None => Err(ParsingError::NoFilename),
            Some(filename) => {
                self.data = parse_plugin(&input, self.game_id, &filename, load_header_only)
                    .to_full_result()
                    .map_err(ParsingError::ContentError)?;

                Ok(())
            }
        }
    }

    pub fn parse_file(&mut self, load_header_only: bool) -> Result<(), ParsingError> {
        let f = File::open(self.path.clone())
            .map_err(|e| ParsingError::IOError(e))?;

        let mut reader = BufReader::new(f);

        let mut content: Vec<u8> = Vec::new();
        reader
            .read_to_end(&mut content)
            .map_err(|e| ParsingError::IOError(e))?;

        self.parse(&content, load_header_only)
    }

    pub unsafe fn parse_mmapped_file(&mut self,
                                     load_header_only: bool)
                                     -> Result<(), ParsingError> {
        let mmap_view = Mmap::open_path(self.path.as_path(), Protection::Read)
            .map_err(ParsingError::IOError)?
            .into_view();

        let mmap_slice = mmap_view.as_slice();

        self.parse(mmap_slice, load_header_only)
    }

    pub fn filename(&self) -> Option<String> {
        self.path
            .file_name()
            .and_then(|filename| filename.to_str())
            .map(|filename| filename.trim_right_matches(".ghost").to_string())
    }

    pub fn masters(&self) -> Result<Vec<&str>, str::Utf8Error> {
        masters(&self.data.header_record)
    }

    pub fn is_master_file(&self) -> bool {
        if self.game_id != GameId::Morrowind {
            self.data.header_record.header.flags & 0x1 != 0
        } else {
            match self.path.extension() {
                Some(x) if x == "esm" => true,
                Some(x) if x == "ghost" => {
                    match self.path
                              .file_stem()
                              .and_then(|file_stem| file_stem.to_str()) {
                        Some(file_stem) => file_stem.ends_with(".esm"),
                        None => false,
                    }
                }
                _ => false,
            }
        }
    }

    pub fn is_valid(game_id: GameId, filepath: &Path, load_header_only: bool) -> bool {
        let mut plugin = Plugin::new(game_id, &filepath.to_path_buf());

        match plugin.parse_file(load_header_only) {
            Ok(_) => true,
            Err(_) => false,
        }
    }

    pub fn description(&self) -> Result<Option<String>, ParsingError> {
        let mut target_subrecord_type: &str = "SNAM";
        let mut description_offset: usize = 0;

        if self.game_id == GameId::Morrowind {
            target_subrecord_type = "HEDR";
            description_offset = 40;
        }

        for subrecord in &self.data.header_record.subrecords {
            if subrecord.subrecord_type == target_subrecord_type {
                let data = &subrecord.data[description_offset..(subrecord.data.len() - 1)];

                return WINDOWS_1252
                           .decode(data, DecoderTrap::Strict)
                           .map(|d| Option::Some(d))
                           .map_err(|e| ParsingError::DecodeError(e));
            }
        }

        Ok(Option::None)
    }

    pub fn record_and_group_count(&self) -> Option<u32> {
        let mut count_offset = 4;
        if self.game_id == GameId::Morrowind {
            count_offset = 296;
        }

        for subrecord in &self.data.header_record.subrecords {
            if subrecord.subrecord_type == "HEDR" {
                let data = &subrecord.data[count_offset..count_offset + 4];
                let mut cursor = Cursor::new(data);
                return cursor.read_u32::<LittleEndian>().ok();
            }
        }

        Option::None
    }
}

fn masters(header_record: &Record) -> Result<Vec<&str>, str::Utf8Error> {
    header_record
        .subrecords
        .iter()
        .filter(|s| s.subrecord_type == "MAST")
        .map(|s| str::from_utf8(&s.data[0..(s.data.len() - 1)]))
        .collect::<Result<Vec<&str>, str::Utf8Error>>()
}

fn parse_form_ids<'a>(input: &'a [u8],
                      game_id: GameId,
                      filename: &str,
                      header_record: &Record)
                      -> IResult<&'a [u8], HashSet<FormId>> {
    let masters = masters(&header_record);

    if masters.is_err() {
        return IResult::Error(ErrorKind::Custom(masters.unwrap_err().valid_up_to() as u32));
    }
    let masters = masters.unwrap();

    if game_id == GameId::Morrowind {
        let (input1, record_form_ids) =
            try_parse!(input,
                                                   many0!(apply!(Record::parse_form_id, game_id)));

        let form_ids: HashSet<FormId> = record_form_ids
            .into_iter()
            .map(|form_id| FormId::new(filename, &masters, form_id))
            .collect();

        IResult::Done(input1, form_ids)
    } else {
        let (input1, groups) = try_parse!(input, many0!(apply!(Group::new, game_id)));

        let mut form_ids: HashSet<FormId> = HashSet::new();
        for group in groups {
            form_ids.extend(group
                                .form_ids
                                .into_iter()
                                .map(|form_id| FormId::new(filename, &masters, form_id)));
        }

        IResult::Done(input1, form_ids)
    }
}

fn parse_plugin<'a>(input: &'a [u8],
                    game_id: GameId,
                    filename: &String,
                    load_header_only: bool)
                    -> IResult<&'a [u8], PluginData> {
    let (input1, header_record) = try_parse!(input, apply!(Record::parse, game_id, false));

    if load_header_only {
        return IResult::Done(input1,
                             PluginData {
                                 header_record: header_record,
                                 form_ids: HashSet::new(),
                             });
    }

    let (input2, form_ids) =
        try_parse!(input1,
                                        apply!(parse_form_ids, game_id, filename, &header_record));

    IResult::Done(input2,
                  PluginData {
                      header_record: header_record,
                      form_ids: form_ids,
                  })
}

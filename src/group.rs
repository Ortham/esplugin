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
use std::io::{BufRead, Seek};

use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::number::complete::le_u32;
use nom::sequence::delimited;
use nom::{IResult, Parser};

use crate::error::Error;
use crate::game_id::GameId;
use crate::record::Record;
use crate::record_id::RecordId;
use crate::ParsingErrorKind;

const GROUP_TYPE: &[u8] = b"GRUP";

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub(crate) struct Group;

impl Group {
    pub(crate) fn read_form_ids<R: BufRead + Seek>(
        reader: &mut R,
        game_id: GameId,
        form_ids: &mut Vec<u32>,
        header_buffer: &mut [u8],
    ) -> Result<(), Error> {
        let group_header_length = group_or_record_header_length(game_id);
        let skip_length = get_header_length_to_skip(game_id);

        // Get a slice of the right size from the header buffer.
        let Some(header_bytes) = header_buffer.get_mut(..usize::from(group_header_length)) else {
            return Err(Error::ParsingError(
                header_buffer.to_vec().into_boxed_slice(),
                ParsingErrorKind::GenericParserError("Group::read_form_ids".into()),
            ));
        };
        reader.read_exact(header_bytes)?;

        let (_, size_of_records) =
            all_consuming(parse_header(group_header_length, skip_length)).parse(header_bytes)?;

        read_records(reader, game_id, form_ids, header_buffer, size_of_records)
    }
}

// Groups and records have the same header length in any game that has both.
fn group_or_record_header_length(game_id: GameId) -> u8 {
    match game_id {
        GameId::Oblivion => 20,
        _ => 24,
    }
}

fn get_header_length_to_skip(game_id: GameId) -> u8 {
    match game_id {
        GameId::Oblivion => 12,
        _ => 16,
    }
}

fn parse_header(group_header_length: u8, skip_length: u8) -> impl Fn(&[u8]) -> IResult<&[u8], u32> {
    move |input| {
        map(
            delimited(tag(GROUP_TYPE), le_u32, take(skip_length)),
            move |group_size| group_size - u32::from(group_header_length),
        )
        .parse(input)
    }
}

fn read_records<R: BufRead + Seek>(
    reader: &mut R,
    game_id: GameId,
    form_ids: &mut Vec<u32>,
    header_buffer: &mut [u8],
    size_of_records: u32,
) -> Result<(), Error> {
    let header_length = group_or_record_header_length(game_id);
    let skip_length = get_header_length_to_skip(game_id);
    let parse_header = parse_header(header_length, skip_length);

    let mut bytes_read = 0;

    while bytes_read < size_of_records {
        // Read the next group/record header.

        // Get a slice of the right size from the header buffer.
        let Some(header_bytes) = header_buffer.get_mut(..usize::from(header_length)) else {
            return Err(Error::ParsingError(
                header_buffer.to_vec().into_boxed_slice(),
                ParsingErrorKind::GenericParserError("read_records".into()),
            ));
        };

        reader.read_exact(header_bytes)?;
        bytes_read += u32::from(header_length);

        if header_bytes.starts_with(GROUP_TYPE) {
            let (_, size_of_records) = all_consuming(&parse_header).parse(header_bytes)?;

            read_records(reader, game_id, form_ids, header_buffer, size_of_records)?;
            bytes_read += size_of_records;
        } else {
            let (record_bytes_read, record_id) =
                Record::read_record_id(reader, game_id, header_bytes, true)?;
            bytes_read += record_bytes_read;

            if let Some(RecordId::FormId(form_id)) = record_id {
                form_ids.push(form_id.get());
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::{read_test_data, record::MAX_RECORD_HEADER_LENGTH};

    use super::*;

    #[test]
    fn new_should_store_formids_for_all_records_in_a_group() {
        let data = read_test_data("Skyrim/Data/Blank - Master Dependent.esm", 0x56..);

        let mut form_ids: Vec<u32> = Vec::new();
        let mut header_buf = [0; MAX_RECORD_HEADER_LENGTH];
        Group::read_form_ids(
            &mut Cursor::new(&data),
            GameId::Skyrim,
            &mut form_ids,
            &mut header_buf,
        )
        .unwrap();

        assert_eq!(8, form_ids.len());
        // Also check three FormIDs from near the beginning, middle and end of the group.
        assert!(form_ids.contains(&0xCF0));
        assert!(form_ids.contains(&0x0100_0CEB));
        assert!(form_ids.contains(&0x0100_0CED));
    }

    #[test]
    fn new_should_store_formids_for_all_records_in_subgroups() {
        let data = read_test_data("Skyrim/Data/Blank.esm", 0x1004C..0x10114);

        let mut form_ids: Vec<u32> = Vec::new();
        let mut header_buf = [0; MAX_RECORD_HEADER_LENGTH];
        Group::read_form_ids(
            &mut Cursor::new(&data),
            GameId::Skyrim,
            &mut form_ids,
            &mut header_buf,
        )
        .unwrap();

        assert_eq!(1, form_ids.len());
        assert!(form_ids.contains(&0xCF9));
    }
}

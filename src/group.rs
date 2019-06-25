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

use std::mem;

use nom::bytes::complete::{tag, take};
use nom::combinator::{map, peek};
use nom::multi::length_data;
use nom::number::complete::le_u32;
use nom::sequence::delimited;
use nom::IResult;

use game_id::GameId;
use record::Record;
use record_id::RecordId;

const GROUP_TYPE: &[u8] = b"GRUP";

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct Group;

impl Group {
    pub fn parse_for_form_ids<'a>(
        input: &'a [u8],
        game_id: GameId,
        form_ids: &mut Vec<u32>,
    ) -> IResult<&'a [u8], ()> {
        let (remaining_input, records_data) = length_data(parse_header(game_id))(input)?;
        parse_records(records_data, game_id, form_ids)?;

        Ok((remaining_input, ()))
    }
}

fn get_header_length_to_skip(game_id: GameId) -> u8 {
    match game_id {
        GameId::Oblivion => 12,
        _ => 16,
    }
}

fn parse_header(game_id: GameId) -> impl Fn(&[u8]) -> IResult<&[u8], u32> {
    move |input| {
        let skip_length = get_header_length_to_skip(game_id);
        let group_header_length =
            GROUP_TYPE.len() as u8 + mem::size_of::<u32>() as u8 + skip_length;

        map(
            delimited(tag(GROUP_TYPE), le_u32, take(skip_length)),
            move |group_size| group_size - u32::from(group_header_length),
        )(input)
    }
}

fn parse_records<'a>(
    input: &'a [u8],
    game_id: GameId,
    form_ids: &mut Vec<u32>,
) -> IResult<&'a [u8], ()> {
    let mut input1 = input;

    while !input1.is_empty() {
        let (_, next_type) = peek(take(GROUP_TYPE.len()))(input1)?;

        if next_type == GROUP_TYPE {
            let (input2, _) = Group::parse_for_form_ids(input1, game_id, form_ids)?;
            input1 = input2;
        } else {
            let (input2, record_id) = Record::parse_record_id(input1, game_id)?;
            input1 = input2;
            if let Some(RecordId::FormId(form_id)) = record_id {
                form_ids.push(form_id.get());
            }
        }
    }

    Ok((&input1, ()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_should_store_formids_for_all_records_in_a_group() {
        let data =
            &include_bytes!("../testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")[0x56..];

        let mut form_ids: Vec<u32> = Vec::new();
        Group::parse_for_form_ids(data, GameId::Skyrim, &mut form_ids).unwrap();

        assert_eq!(8, form_ids.len());
        // Also check three FormIDs from near the beginning, middle and end of the group.
        assert!(form_ids.contains(&0xCF0));
        assert!(form_ids.contains(&0x1000CEB));
        assert!(form_ids.contains(&0x1000CED));
    }

    #[test]
    fn new_should_store_formids_for_all_records_in_subgroups() {
        let data = &include_bytes!("../testing-plugins/Skyrim/Data/Blank.esm")[0x1004C..0x10114];

        let mut form_ids: Vec<u32> = Vec::new();
        Group::parse_for_form_ids(data, GameId::Skyrim, &mut form_ids).unwrap();

        assert_eq!(1, form_ids.len());
        assert!(form_ids.contains(&0xCF9));
    }
}

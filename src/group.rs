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

use nom::IResult;
use nom::le_u32;

use game_id::GameId;
use record::Record;

const GROUP_TYPE: &'static str = "GRUP";
const GROUP_TYPE_LENGTH: u8 = 4;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct Group {
    form_ids: Vec<u32>,
}

impl Group {
    pub fn new(input: &[u8], game_id: GameId) -> IResult<&[u8], Group> {
        group(input, game_id)
    }

    pub fn form_ids(self) -> Vec<u32> {
        self.form_ids
    }
}

fn get_header_length_to_skip(game_id: GameId) -> u8 {
    match game_id {
        GameId::Oblivion => 12,
        _ => 16,
    }
}

named_args!(group_header(game_id: GameId) <u32>,
    do_parse!(
        take!(GROUP_TYPE_LENGTH) >>
        group_size: le_u32 >>
        take!(get_header_length_to_skip(game_id)) >>

        (group_size - (GROUP_TYPE_LENGTH
            + mem::size_of::<u32>() as u8
            + get_header_length_to_skip(game_id)) as u32)
    )
);

fn parse_records(input: &[u8], game_id: GameId) -> IResult<&[u8], Vec<u32>> {
    let mut input1: &[u8] = input;

    let mut form_ids: Vec<u32> = Vec::new();

    while !input1.is_empty() {
        let (input2, next_type) = try_parse!(input1, peek!(take_str!(GROUP_TYPE_LENGTH)));
        assert_eq!(input1, input2);

        if next_type == GROUP_TYPE {
            let (input2, subgroup) = try_parse!(input1, apply!(Group::new, game_id));
            input1 = input2;

            form_ids.extend(subgroup.form_ids.into_iter());
        } else {
            let (input2, form_id) = try_parse!(input1, apply!(Record::parse_form_id, game_id));
            input1 = input2;
            form_ids.push(form_id);
        }
    }

    IResult::Done(input1, form_ids)
}

named_args!(group(game_id: GameId) <Group>,
do_parse!(
    form_ids: length_value!(apply!(group_header, game_id), apply!(parse_records, game_id)) >>

    (Group {
        form_ids: form_ids
    })
)
);



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_should_store_formids_for_all_records_in_a_group() {
        let data = &include_bytes!(
            "../testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")
            [0x56..];

        let group = Group::new(data, GameId::Skyrim).to_result().unwrap();

        assert_eq!(8, group.form_ids.len());
        // Also check three FormIDs from near the beginning, middle and end of the group.
        assert!(group.form_ids.contains(&0xCF0));
        assert!(group.form_ids.contains(&0x1000CEB));
        assert!(group.form_ids.contains(&0x1000CED));
    }

    #[test]
    fn new_should_store_formids_for_all_records_in_subgroups() {
        let data = &include_bytes!("../testing-plugins/Skyrim/Data/Blank.esm")[0x1004C..0x10114];

        let group = Group::new(data, GameId::Skyrim).to_result().unwrap();

        assert_eq!(1, group.form_ids.len());
        assert!(group.form_ids.contains(&0xCF9));
    }
}

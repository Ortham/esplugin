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

use nom::IResult;
use nom::le_u32;

use game_id::GameId;
use subrecord::Subrecord;

const RECORD_TYPE_LENGTH: u8 = 4;

#[derive(Debug)]
pub struct RecordHeader {
    pub record_type: String,
    pub flags: u32,
    pub form_id: u32,
    size_of_subrecords: u32,
}

#[derive(Debug)]
pub struct Record {
    pub header: RecordHeader,
    pub subrecords: Vec<Subrecord>,
}

impl Record {
    pub fn new() -> Record {
        Record {
            header: RecordHeader {
                record_type: String::new(),
                flags: 0,
                form_id: 0,
                size_of_subrecords: 0,
            },
            subrecords: Vec::new(),
        }
    }

    pub fn parse(input: &[u8], game_id: GameId, skip_subrecords: bool) -> IResult<&[u8], Record> {
        record(input, game_id, skip_subrecords)
    }

    pub fn parse_form_id(input: &[u8], game_id: GameId) -> IResult<&[u8], u32> {
        let (input1, header) = try_parse!(input, apply!(record_header, game_id));
        let (input2, _) = try_parse!(input1, take!(header.size_of_subrecords));

        IResult::Done(input2, header.form_id)
    }
}

named_args!(record_header(game_id: GameId) <RecordHeader>,
    do_parse!(
        record_type: take_str!(RECORD_TYPE_LENGTH) >>
        size_of_subrecords: le_u32 >>
        cond!(game_id == GameId::Morrowind, take!(4)) >>
        flags: le_u32 >>
        form_id: cond!(game_id != GameId::Morrowind, le_u32) >>
        cond!(game_id != GameId::Morrowind, take!(4)) >>
        cond!(game_id != GameId::Morrowind && game_id != GameId::Oblivion, take!(4)) >>

        (RecordHeader {
            record_type: record_type.to_string(),
            flags: flags,
            form_id: form_id.unwrap_or(0),
            size_of_subrecords: size_of_subrecords,
        })
    )
);

fn record(input: &[u8], game_id: GameId, skip_subrecords: bool) -> IResult<&[u8], Record> {
    let (input1, header) = try_parse!(input, apply!(record_header, game_id));
    let (input2, subrecords_data) = try_parse!(input1, take!(header.size_of_subrecords));

    let subrecords: Vec<Subrecord>;
    if !skip_subrecords {
        subrecords = try_parse!(subrecords_data, apply!(parse_subrecords, game_id)).1;
    } else {
        subrecords = Vec::new();
    }

    IResult::Done(
        input2,
        Record {
            header: header,
            subrecords: subrecords,
        },
    )
}

fn parse_subrecords(input: &[u8], game_id: GameId) -> IResult<&[u8], Vec<Subrecord>> {
    let mut input1: &[u8] = input;
    let mut subrecords: Vec<Subrecord> = Vec::new();
    let mut large_subrecord_size: u32 = 0;

    while input1.len() > 0 {
        let (input2, subrecord) = try_parse!(input1, apply!(Subrecord::new, game_id, large_subrecord_size));
        input1 = input2;
        if subrecord.subrecord_type == "XXXX" {
            large_subrecord_size = try_parse!(&subrecord.data, le_u32).1;
        } else {
            large_subrecord_size = 0;
            subrecords.push(subrecord);
        }
    }

    IResult::Done(input1, subrecords)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_should_read_tes4_header_correctly() {
        let data = &include_bytes!("../tests/testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")
            [..0x56];

        let record = Record::parse(data, GameId::Skyrim, false)
            .to_result()
            .unwrap();

        assert_eq!(0x1, record.header.flags);
        assert_eq!(0, record.header.form_id);
        assert_eq!(5, record.subrecords.len());

        assert_eq!("HEDR", record.subrecords[0].subrecord_type);
        assert_eq!("CNAM", record.subrecords[1].subrecord_type);
        assert_eq!("SNAM", record.subrecords[2].subrecord_type);
        assert_eq!("MAST", record.subrecords[3].subrecord_type);
        assert_eq!("DATA", record.subrecords[4].subrecord_type);
    }

    #[test]
    fn parse_should_read_tes3_header_correctly() {
        let data = &include_bytes!("../tests/testing-plugins/Morrowind/Data Files/Blank.esm")
            [..0x144];

        let record = Record::parse(data, GameId::Morrowind, false)
            .to_result()
            .unwrap();

        assert_eq!(0, record.header.flags);
        assert_eq!(0, record.header.form_id);
        assert_eq!(1, record.subrecords.len());

        assert_eq!("HEDR", record.subrecords[0].subrecord_type);
    }

    #[test]
    fn parse_should_obey_skip_subrecords_parameter() {
        let data = &include_bytes!("../tests/testing-plugins/Morrowind/Data Files/Blank.esm")
            [..0x144];

        let record = Record::parse(data, GameId::Morrowind, true)
            .to_result()
            .unwrap();

        assert_eq!(0, record.header.flags);
        assert_eq!(0, record.header.form_id);
        assert_eq!(0, record.subrecords.len());
    }

    #[test]
    fn parse_should_read_large_subrecords_correctly() {
        let data = &include_bytes!("../tests/testing-plugins/Skyrim/Data/Blank.esm")[..0x1004C];

        let record = Record::parse(data, GameId::Skyrim, false)
            .to_result()
            .unwrap();

        assert_eq!(0x1, record.header.flags);
        assert_eq!(0, record.header.form_id);
        assert_eq!(4, record.subrecords.len());

        assert_eq!("HEDR", record.subrecords[0].subrecord_type);
        assert_eq!("CNAM", record.subrecords[1].subrecord_type);
        assert_eq!("SNAM", record.subrecords[2].subrecord_type);
        assert_eq!("ONAM", record.subrecords[3].subrecord_type);
    }

    #[test]
    fn parse_form_id_should_return_the_form_id() {
        let data = &include_bytes!("../tests/testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")
            [..0x56];

        let form_id = Record::parse_form_id(data, GameId::Skyrim)
            .to_result()
            .unwrap();

        assert_eq!(0, form_id);

        let data = &include_bytes!("../tests/testing-plugins/Morrowind/Data Files/Blank.esm")
            [..0x144];

        let form_id = Record::parse_form_id(data, GameId::Morrowind)
            .to_result()
            .unwrap();

        assert_eq!(0, form_id);

        let data = &include_bytes!("../tests/testing-plugins/Skyrim/Data/Blank.esp")[0x53..0xEF];

        let form_id = Record::parse_form_id(data, GameId::Skyrim)
            .to_result()
            .unwrap();

        assert_eq!(0xCEC, form_id);
    }
}

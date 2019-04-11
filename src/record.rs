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
use std::io;
use std::num::NonZeroU32;

use byteorder::{ByteOrder, LittleEndian};
use nom::IResult;
use nom::{le_u32, le_u8};

use error::Error;
use game_id::GameId;
use record_id::{NamespacedId, RecordId};
use subrecord::{parse_subrecord_data_as_u32, Subrecord, SubrecordRef};

const RECORD_TYPE_LENGTH: usize = 4;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct RecordHeader {
    record_type: [u8; 4],
    flags: u32,
    form_id: Option<NonZeroU32>,
    size_of_subrecords: u32,
}

impl RecordHeader {
    fn are_subrecords_compressed(&self) -> bool {
        (self.flags & 0x0004_0000) != 0
    }

    pub fn flags(&self) -> u32 {
        self.flags
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct Record {
    header: RecordHeader,
    subrecords: Vec<Subrecord>,
}

impl Record {
    pub fn read_and_validate<T: io::Read>(
        reader: &mut T,
        game_id: GameId,
        expected_type: &[u8],
    ) -> Result<Vec<u8>, Error> {
        let mut content: Vec<u8> = vec![0; header_length(game_id)];
        reader.read_exact(&mut content)?;

        if &content[0..4] != expected_type {
            return Err(Error::ParsingError);
        }

        let size_of_subrecords = LittleEndian::read_u32(&content[4..]) as usize;
        if size_of_subrecords > 0 {
            let mut subrecords = vec![0; size_of_subrecords];
            reader.read_exact(&mut subrecords)?;

            content.append(&mut subrecords);
        }

        Ok(content)
    }

    pub fn parse(input: &[u8], game_id: GameId, skip_subrecords: bool) -> IResult<&[u8], Record> {
        record(input, game_id, skip_subrecords)
    }

    pub fn parse_record_id(input: &[u8], game_id: GameId) -> IResult<&[u8], Option<RecordId>> {
        if game_id == GameId::Morrowind {
            let (remaining_input, header) = try_parse!(input, apply!(record_header, game_id));
            let (remaining_input, subrecords_data) =
                try_parse!(remaining_input, take!(header.size_of_subrecords));

            // Header is parsed, now parse subrecords until the id subrecord has been found.
            let mappers = get_record_id_subrecord_mappers(&header.record_type);
            if !mappers.is_empty() {
                let data = try_parse!(subrecords_data, apply!(parse_id_subrecords, mappers)).1;

                let namespaced_id = data.map(|data| {
                    RecordId::NamespacedId(NamespacedId::new(&header.record_type, data))
                });

                Ok((remaining_input, namespaced_id))
            } else {
                Ok((remaining_input, None))
            }
        } else {
            do_parse!(
                input,
                take!(RECORD_TYPE_LENGTH)
                    >> size_of_subrecords: le_u32
                    >> take!(4)
                    >> form_id: le_u32
                    >> take!(4)
                    >> cond!(game_id != GameId::Oblivion, take!(4))
                    >> take!(size_of_subrecords)
                    >> (NonZeroU32::new(form_id).map(RecordId::FormId))
            )
        }
    }

    pub fn header(&self) -> &RecordHeader {
        &self.header
    }

    pub fn subrecords(&self) -> &[Subrecord] {
        &self.subrecords
    }
}

fn map_scpt_id_data(data: &[u8]) -> Option<&[u8]> {
    if data.len() > 32 {
        Some(&data[..32])
    } else {
        None
    }
}

fn map_cell_name_id_data(data: &[u8]) -> Option<&[u8]> {
    if data.len() > 1 {
        Some(data)
    } else {
        None
    }
}

fn map_generic_id_data(data: &[u8]) -> Option<&[u8]> {
    Some(data)
}

fn get_record_id_subrecord_mappers<'a>(
    record_type: &[u8],
) -> Vec<(&[u8], &dyn Fn(&'a [u8]) -> Option<&'a [u8]>)> {
    match record_type {
        b"GMST" | b"GLOB" | b"CLAS" | b"FACT" | b"RACE" | b"SOUN" | b"REGN" | b"BSGN" | b"LTEX"
        | b"STAT" | b"DOOR" | b"MISC" | b"WEAP" | b"CONT" | b"SPEL" | b"CREA" | b"BODY"
        | b"LIGH" | b"ENCH" | b"NPC_" | b"ARMO" | b"CLOT" | b"REPA" | b"ACTI" | b"APPA"
        | b"LOCK" | b"PROB" | b"INGR" | b"BOOK" | b"ALCH" | b"LEVI" | b"LEVC" | b"PGRD"
        | b"DIAL" => vec![(b"NAME", &map_generic_id_data)],
        b"SKIL" | b"MGEF" => vec![(b"INDX", &map_generic_id_data)],
        b"SNDG" => vec![(b"SNAM", &map_generic_id_data)],
        b"INFO" => vec![(b"INAM", &map_generic_id_data)],
        b"LAND" => vec![(b"INTV", &map_generic_id_data)],
        b"SCPT" => vec![(b"SCHD", &map_scpt_id_data)],
        b"CELL" => vec![
            (b"NAME", &map_cell_name_id_data),
            (b"REGN", &map_generic_id_data),
        ],
        b"TES3" | _ => Vec::new(),
    }
}

fn header_length(game_id: GameId) -> usize {
    match game_id {
        GameId::Morrowind => 16,
        GameId::Oblivion => 20,
        _ => 24,
    }
}

named!(
    record_type<[u8; 4]>,
    count_fixed!(u8, le_u8, RECORD_TYPE_LENGTH)
);

named_args!(record_header(game_id: GameId) <RecordHeader>,
    do_parse!(
        record_type: record_type >>
        size_of_subrecords: le_u32 >>
        cond!(game_id == GameId::Morrowind, take!(4)) >>
        flags: le_u32 >>
        form_id: cond!(game_id != GameId::Morrowind, le_u32) >>
        cond!(game_id != GameId::Morrowind, take!(4)) >>
        cond!(game_id != GameId::Morrowind && game_id != GameId::Oblivion, take!(4)) >>

        (RecordHeader {
            record_type,
            flags,
            form_id: form_id.and_then(NonZeroU32::new),
            size_of_subrecords,
        })
    )
);

fn record(input: &[u8], game_id: GameId, skip_subrecords: bool) -> IResult<&[u8], Record> {
    let (input1, header) = try_parse!(input, apply!(record_header, game_id));
    let (input2, subrecords_data) = try_parse!(input1, take!(header.size_of_subrecords));

    let subrecords: Vec<Subrecord> = if !skip_subrecords {
        try_parse!(
            subrecords_data,
            apply!(
                parse_subrecords,
                game_id,
                header.are_subrecords_compressed()
            )
        )
        .1
    } else {
        Vec::new()
    };

    Ok((input2, Record { header, subrecords }))
}

fn parse_subrecords(
    input: &[u8],
    game_id: GameId,
    are_compressed: bool,
) -> IResult<&[u8], Vec<Subrecord>> {
    let mut input1: &[u8] = input;
    let mut subrecords: Vec<Subrecord> = Vec::new();
    let mut large_subrecord_size: u32 = 0;

    while !input1.is_empty() {
        let (input2, subrecord) = try_parse!(
            input1,
            apply!(
                Subrecord::new,
                game_id,
                large_subrecord_size,
                are_compressed
            )
        );
        if subrecord.subrecord_type() == b"XXXX" {
            large_subrecord_size = parse_subrecord_data_as_u32(input1)?.1;
        } else {
            large_subrecord_size = 0;
            subrecords.push(subrecord);
        }
        input1 = input2;
    }

    Ok((input1, subrecords))
}

/// Parses subrecords until one of each given subrecord type has been read, then
/// skips to the end of the record and returns.
fn parse_id_subrecords<'a>(
    input: &'a [u8],
    mut subrecord_mappers: Vec<(&[u8], &dyn Fn(&'a [u8]) -> Option<&'a [u8]>)>,
) -> IResult<&'a [u8], Option<&'a [u8]>> {
    let mut remaining_input: &[u8] = input;

    while !remaining_input.is_empty() {
        if subrecord_mappers.is_empty() {
            break;
        }
        let (input2, subrecord) = try_parse!(
            remaining_input,
            apply!(SubrecordRef::new, GameId::Morrowind, 0)
        );
        if let Ok(index) =
            subrecord_mappers.binary_search_by(|m| m.0.cmp(subrecord.subrecord_type()))
        {
            let data = subrecord_mappers[index].1(subrecord.data());
            if let Some(_) = data {
                return Ok((remaining_input, data));
            }
            subrecord_mappers.swap_remove(index);
        }
        remaining_input = input2;
    }

    Ok((remaining_input, None))
}

#[cfg(test)]
mod tests {
    use super::*;

    mod morrowind {
        use super::super::*;

        #[test]
        fn header_length_should_be_16() {
            assert_eq!(16, header_length(GameId::Morrowind));
        }

        #[test]
        fn parse_should_read_tes3_header_correctly() {
            let data =
                &include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[..0x144];

            let record = Record::parse(data, GameId::Morrowind, false).unwrap().1;

            assert_eq!(0, record.header.flags);
            assert!(record.header.form_id.is_none());
            assert_eq!(1, record.subrecords.len());

            assert_eq!(b"HEDR", record.subrecords[0].subrecord_type());
        }

        #[test]
        fn parse_should_set_form_id_to_none_for_all_records() {
            let data =
                &include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[0x144..0x16F];

            let record = Record::parse(data, GameId::Morrowind, false).unwrap().1;

            assert!(record.header.form_id.is_none());
        }

        #[test]
        fn parse_record_id_should_return_none_for_tes3_header() {
            let data =
                &include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[..0x144];

            let form_id = Record::parse_record_id(data, GameId::Morrowind).unwrap().1;

            assert!(form_id.is_none());
        }

        #[test]
        fn parse_record_id_should_return_some_namespaced_id_for_gmst() {
            let data =
                &include_bytes!("../testing-plugins/Morrowind/Data Files/Blank.esm")[0x144..0x16F];

            let form_id = Record::parse_record_id(data, GameId::Morrowind).unwrap().1;

            assert!(form_id.unwrap().namespaced_id().is_some());
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_name_data_for_most_record_types() {
            let mappers = get_record_id_subrecord_mappers(b"GMST");
            assert_eq!(1, mappers.len());
            assert_eq!(b"NAME", mappers[0].0);
            assert_eq!(Some(b"".as_ref()), mappers[0].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_indx_data_for_skil_and_mgef_records() {
            let mappers = get_record_id_subrecord_mappers(b"SKIL");
            assert_eq!(1, mappers.len());
            assert_eq!(b"INDX", mappers[0].0);
            assert_eq!(Some(b"".as_ref()), mappers[0].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));

            let mappers = get_record_id_subrecord_mappers(b"MGEF");
            assert_eq!(1, mappers.len());
            assert_eq!(b"INDX", mappers[0].0);
            assert_eq!(Some(b"".as_ref()), mappers[0].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_snam_data_for_sndg_records() {
            let mappers = get_record_id_subrecord_mappers(b"SNDG");
            assert_eq!(1, mappers.len());
            assert_eq!(b"SNAM", mappers[0].0);
            assert_eq!(Some(b"".as_ref()), mappers[0].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_inam_data_for_info_records() {
            let mappers = get_record_id_subrecord_mappers(b"INFO");
            assert_eq!(1, mappers.len());
            assert_eq!(b"INAM", mappers[0].0);
            assert_eq!(Some(b"".as_ref()), mappers[0].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_intv_data_for_land_records() {
            let mappers = get_record_id_subrecord_mappers(b"LAND");
            assert_eq!(1, mappers.len());
            assert_eq!(b"INTV", mappers[0].0);
            assert_eq!(Some(b"".as_ref()), mappers[0].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_schd_data_for_scpt_records() {
            let mappers = get_record_id_subrecord_mappers(b"SCPT");
            assert_eq!(1, mappers.len());
            assert_eq!(b"SCHD", mappers[0].0);
            assert_eq!(None, mappers[0].1(b"testdata"));
            assert_eq!(Some([0u8; 32].as_ref()), mappers[0].1([0u8; 35].as_ref()));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_use_name_and_regn_data_for_cell_records() {
            let mappers = get_record_id_subrecord_mappers(b"CELL");
            assert_eq!(2, mappers.len());

            assert_eq!(b"NAME", mappers[0].0);
            assert_eq!(None, mappers[0].1(b""));
            assert_eq!(None, mappers[0].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[0].1(b"testdata"));

            assert_eq!(b"REGN", mappers[1].0);
            assert_eq!(Some(b"".as_ref()), mappers[1].1(b""));
            assert_eq!(Some(b" ".as_ref()), mappers[1].1(b" "));
            assert_eq!(Some(b"testdata".as_ref()), mappers[1].1(b"testdata"));
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_return_an_empty_vec_for_the_tes3_record() {
            let mappers = get_record_id_subrecord_mappers(b"TES3");
            assert!(mappers.is_empty());
        }

        #[test]
        fn get_record_id_subrecord_mappers_should_return_an_empty_vec_for_unrecognised_types() {
            let mappers = get_record_id_subrecord_mappers(b"DUMY");
            assert!(mappers.is_empty());
        }
    }

    mod oblivion {
        use super::super::*;

        #[test]
        fn header_length_should_be_20() {
            assert_eq!(20, header_length(GameId::Oblivion));
        }

        #[test]
        fn parse_should_read_tes4_header_correctly() {
            let data = &include_bytes!("../testing-plugins/Oblivion/Data/Blank.esm")[..0x144];

            let record = Record::parse(data, GameId::Oblivion, false).unwrap().1;

            assert_eq!(1, record.header.flags);
            assert!(record.header.form_id.is_none());
            assert_eq!(3, record.subrecords.len());

            assert_eq!(b"HEDR", record.subrecords[0].subrecord_type());
            assert_eq!(b"CNAM", record.subrecords[1].subrecord_type());
            assert_eq!(b"SNAM", record.subrecords[2].subrecord_type());
        }

        #[test]
        fn parse_should_set_non_zero_form_id_for_non_header_records() {
            let data = &include_bytes!("../testing-plugins/Oblivion/Data/Blank.esm")[0x4C..0x70];

            let record = Record::parse(data, GameId::Oblivion, false).unwrap().1;

            assert_eq!(0xCF0, record.header.form_id.unwrap().get());
        }

        #[test]
        fn parse_record_id_should_return_the_form_id() {
            let data = &include_bytes!("../testing-plugins/Oblivion/Data/Blank.esm")[0x4C..0x70];

            let form_id = Record::parse_record_id(data, GameId::Oblivion).unwrap().1;

            assert_eq!(0xCF0, form_id.unwrap().form_id().unwrap().get());
        }
    }

    mod skyrim {
        use super::super::*;

        #[test]
        fn header_length_should_be_24() {
            assert_eq!(24, header_length(GameId::Skyrim));
        }

        #[test]
        fn parse_should_read_tes4_header_correctly() {
            let data =
                &include_bytes!("../testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")
                    [..0x56];

            let record = Record::parse(data, GameId::Skyrim, false).unwrap().1;

            assert_eq!(0x1, record.header.flags);
            assert!(record.header.form_id.is_none());
            assert_eq!(5, record.subrecords.len());

            assert_eq!(b"HEDR", record.subrecords[0].subrecord_type());
            assert_eq!(b"CNAM", record.subrecords[1].subrecord_type());
            assert_eq!(b"SNAM", record.subrecords[2].subrecord_type());
            assert_eq!(b"MAST", record.subrecords[3].subrecord_type());
            assert_eq!(b"DATA", record.subrecords[4].subrecord_type());
        }

        #[test]
        fn parse_should_set_non_zero_form_id_for_non_header_records() {
            let data = &include_bytes!("../testing-plugins/Skyrim/Data/Blank.esp")[0x53..0xEF];

            let record = Record::parse(data, GameId::Skyrim, false).unwrap().1;

            assert_eq!(0xCEC, record.header.form_id.unwrap().get());
        }

        #[test]
        fn parse_record_id_should_return_the_form_id() {
            let data = &include_bytes!("../testing-plugins/Skyrim/Data/Blank.esp")[0x53..0xEF];

            let form_id = Record::parse_record_id(data, GameId::Skyrim).unwrap().1;

            assert_eq!(0xCEC, form_id.unwrap().form_id().unwrap().get());
        }
    }

    #[test]
    fn read_and_validate_should_read_a_record_from_the_given_reader() {
        let data =
            &include_bytes!("../testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")[..0x56];
        let mut reader = io::Cursor::new(data);

        let bytes = Record::read_and_validate(&mut reader, GameId::Skyrim, b"TES4").unwrap();

        assert_eq!(data, bytes.as_slice());
    }

    #[test]
    fn read_and_validate_should_fail_if_the_type_is_unexpected() {
        let data =
            &include_bytes!("../testing-plugins/Skyrim/Data/Blank - Master Dependent.esm")[..0x56];
        let mut reader = io::Cursor::new(data);

        let result = Record::read_and_validate(&mut reader, GameId::Skyrim, b"TES3");
        assert!(result.is_err());
    }

    #[test]
    fn parse_should_obey_skip_subrecords_parameter() {
        let data = &include_bytes!("../testing-plugins/Skyrim/Data/Blank.esm")[..0x1004C];

        let record = Record::parse(data, GameId::Skyrim, true).unwrap().1;

        assert_eq!(1, record.header.flags);
        assert_eq!(0, record.subrecords.len());
    }

    #[test]
    fn parse_should_read_large_subrecords_correctly() {
        let data = &include_bytes!("../testing-plugins/Skyrim/Data/Blank.esm")[..0x1004C];

        let record = Record::parse(data, GameId::Skyrim, false).unwrap().1;

        assert_eq!(0x1, record.header.flags);
        assert_eq!(4, record.subrecords.len());

        assert_eq!(b"HEDR", record.subrecords[0].subrecord_type());
        assert_eq!(b"CNAM", record.subrecords[1].subrecord_type());
        assert_eq!(b"SNAM", record.subrecords[2].subrecord_type());
        assert_eq!(b"ONAM", record.subrecords[3].subrecord_type());
    }

    #[test]
    #[cfg(feature = "compressed-fields")]
    fn parse_should_read_compressed_subrecords_correctly() {
        const DATA: &'static [u8] = &[
            0x42, 0x50, 0x54, 0x44, //type
            0x23, 0x00, 0x00, 0x00, //size
            0x00, 0x00, 0x04, 0x00, //flags
            0xEC, 0x0C, 0x00, 0x00, //id
            0x00, 0x00, 0x00, 0x00, //revision
            0x2B, 0x00, //version
            0x00, 0x00, //unknown
            0x42, 0x50, 0x54, 0x4E, //field type
            0x1D, 0x00, //field size
            0x19, 0x00, 0x00, 0x00, //decompressed field size
            0x75, 0xc5, 0x21, 0x0d, 0x00, 0x00, 0x08, 0x05, 0xd1,
            0x6c, //field data (compressed)
            0x6c, 0xdc, 0x57, 0x48, 0x3c, 0xfd, 0x5b, 0x5c, 0x02,
            0xd4, //field data (compressed)
            0x6b, 0x32, 0xb5, 0xdc, 0xa3, //field data (compressed)
        ];

        let record = Record::parse(DATA, GameId::Skyrim, false).unwrap().1;

        assert_eq!(0xCEC, record.header.form_id.unwrap().get());
        assert_eq!(0x00040000, record.header.flags);
        assert_eq!(1, record.subrecords.len());

        let decompressed_data = record.subrecords[0].decompress_data().unwrap();
        assert_eq!(
            "DEFLATE_DEFLATE_DEFLATE_DEFLATE".as_bytes(),
            decompressed_data.as_slice()
        );
    }
}

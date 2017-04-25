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

extern crate nom;

use std::str;

use nom::le_u16;
use nom::le_u32;
use nom::IResult;

use game_id::GameId;

const SUBRECORD_TYPE_LENGTH: u8 = 4;

#[derive(Debug)]
pub struct Subrecord {
    pub subrecord_type: String,
    pub data: Vec<u8>,
}

impl Subrecord {
    pub fn new(input: &[u8],
               game_id: GameId,
               data_length_override: u32)
               -> IResult<&[u8], Subrecord> {
        if game_id == GameId::Morrowind {
            morrowind_subrecord(input)
        } else if data_length_override != 0 {
            presized_subrecord(input, data_length_override)
        } else {
            simple_subrecord(input)
        }
    }
}

named!(subrecord_type <&str>, take_str!(SUBRECORD_TYPE_LENGTH));

named!(morrowind_subrecord(&[u8]) -> Subrecord,
    do_parse!(
        subrecord_type: subrecord_type >>
        data: length_bytes!(le_u32) >>

        (Subrecord {
            subrecord_type: subrecord_type.to_string(),
            data: data.to_vec(),
        })
    )
);

named!(simple_subrecord(&[u8]) -> Subrecord,
    do_parse!(
        subrecord_type: subrecord_type >>
        data: length_bytes!(le_u16) >>

        (Subrecord {
            subrecord_type: subrecord_type.to_string(),
            data: data.to_vec(),
        })
    )
);

named_args!(presized_subrecord(data_length: u32) <Subrecord>,
    do_parse!(
        subrecord_type: subrecord_type >>
        le_u16 >>
        data: take!(data_length) >>

        (Subrecord {
            subrecord_type: subrecord_type.to_string(),
            data: data.to_vec(),
        })
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    const TES3_DATA_SUBRECORD: &'static [u8] = &[0x44, 0x41, 0x54, 0x41, 0x08, 0x00, 0x00, 0x00,
                                                 0x6D, 0x63, 0x61, 0x72, 0x6F, 0x66, 0x61, 0x6E];
    const TES4_CNAM_SUBRECORD: &'static [u8] = &[0x43, 0x4E, 0x41, 0x4D, 0x0A, 0x00, 0x6D, 0x63,
                                                 0x61, 0x72, 0x6F, 0x66, 0x61, 0x6E, 0x6F, 0x00];

    #[test]
    fn parse_should_parse_a_morrowind_subrecord_correctly() {
        let subrecord =
            Subrecord::new(TES3_DATA_SUBRECORD, GameId::Morrowind, 0).to_result().unwrap();

        assert_eq!("DATA", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72, 0x6F, 0x66, 0x61, 0x6E], subrecord.data);
    }

    #[test]
    fn parse_should_ignore_data_length_override_for_morrowind_subrecords() {
        let subrecord =
            Subrecord::new(TES3_DATA_SUBRECORD, GameId::Morrowind, 5).to_result().unwrap();

        assert_eq!("DATA", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72, 0x6F, 0x66, 0x61, 0x6E], subrecord.data);
    }

    #[test]
    fn parse_should_parse_a_non_morrowind_subrecord_with_no_data_length_override_correctly() {
        let subrecord = Subrecord::new(TES4_CNAM_SUBRECORD, GameId::Skyrim, 0).to_result().unwrap();

        assert_eq!("CNAM", subrecord.subrecord_type);

        let expected_data = vec![0x6D, 0x63, 0x61, 0x72, 0x6F, 0x66, 0x61, 0x6E, 0x6F, 0x00];
        assert_eq!(expected_data, subrecord.data);
    }

    #[test]
    fn parse_should_use_data_length_override_if_non_zero_and_game_id_is_not_morrowind() {
        let subrecord =
            Subrecord::new(TES4_CNAM_SUBRECORD, GameId::Oblivion, 4).to_result().unwrap();

        assert_eq!("CNAM", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72], subrecord.data);

        let subrecord = Subrecord::new(TES4_CNAM_SUBRECORD, GameId::Skyrim, 4).to_result().unwrap();

        assert_eq!("CNAM", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72], subrecord.data);

        let subrecord =
            Subrecord::new(TES4_CNAM_SUBRECORD, GameId::Fallout3, 4).to_result().unwrap();

        assert_eq!("CNAM", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72], subrecord.data);

        let subrecord =
            Subrecord::new(TES4_CNAM_SUBRECORD, GameId::FalloutNV, 4).to_result().unwrap();

        assert_eq!("CNAM", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72], subrecord.data);

        let subrecord =
            Subrecord::new(TES4_CNAM_SUBRECORD, GameId::Fallout4, 4).to_result().unwrap();

        assert_eq!("CNAM", subrecord.subrecord_type);
        assert_eq!(vec![0x6D, 0x63, 0x61, 0x72], subrecord.data);
    }
}

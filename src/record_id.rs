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
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub enum RecordId {
    FormId(std::num::NonZeroU32),
    NamespacedId(NamespacedId),
}

/// This is a FormID equivalent for Morrowind plugin records.
/// Record IDs with the same data in the same namespace refer to the same record
/// but if the data or namespace is different, the IDs refer to different records.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct NamespacedId {
    namespace: Namespace,
    hashed_id: u64,
}

impl NamespacedId {
    pub fn new(record_type: [u8; 4], id_data: &[u8]) -> Self {
        let mut hasher = DefaultHasher::new();
        id_data.hash(&mut hasher);

        Self {
            namespace: record_type.into(),
            hashed_id: hasher.finish(),
        }
    }
}

/// Each record's ID belongs to a namespace, depending on the record type.
/// Some record types share the same namespace, others have their own unique
/// namespace.
/// Information about namespaces from <https://github.com/loot/loot/issues/1101#issuecomment-480629856>
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Namespace {
    Race,
    Class,
    Birthsign,
    Script,
    Cell,
    Faction,
    Sound,
    Global,
    Region,
    Skill,
    MagicEffect,
    Land,
    PathGrid,
    Dialog,
    Other,
}

impl From<[u8; 4]> for Namespace {
    fn from(record_type: [u8; 4]) -> Namespace {
        use self::Namespace::*;
        match &record_type {
            b"RACE" => Race,
            b"CLAS" => Class,
            b"BSGN" => Birthsign,
            b"SCPT" => Script,
            b"CELL" => Cell,
            b"FACT" => Faction,
            b"SOUN" => Sound,
            b"GLOB" => Global,
            b"REGN" => Region,
            b"SKIL" => Skill,
            b"MGEF" => MagicEffect,
            b"LAND" => Land,
            b"PGRD" => PathGrid,
            b"DIAL" => Dialog,
            _ => Other,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const OTHER_RECORD_TYPES: &[&[u8; 4]] = &[
        b"ACTI", b"ALCH", b"APPA", b"ARMO", b"BODY", b"BOOK", b"CLOT", b"CONT", b"CREA", b"DOOR",
        b"ENCH", b"GMST", b"INFO", b"INGR", b"LEVC", b"LEVI", b"LIGH", b"LOCK", b"LTEX", b"MISC",
        b"NPC_", b"PROB", b"REPA", b"SNDG", b"SPEL", b"STAT", b"TES3", b"WEAP",
    ];

    #[test]
    fn namespace_from_array_should_namespace_race_class_bsgn_scpt_cell_fact_soun_glob_and_regn() {
        assert_eq!(Namespace::Race, (*b"RACE").into());
        assert_eq!(Namespace::Class, (*b"CLAS").into());
        assert_eq!(Namespace::Birthsign, (*b"BSGN").into());
        assert_eq!(Namespace::Script, (*b"SCPT").into());
        assert_eq!(Namespace::Cell, (*b"CELL").into());
        assert_eq!(Namespace::Faction, (*b"FACT").into());
        assert_eq!(Namespace::Sound, (*b"SOUN").into());
        assert_eq!(Namespace::Global, (*b"GLOB").into());
        assert_eq!(Namespace::Region, (*b"REGN").into());
        assert_eq!(Namespace::Skill, (*b"SKIL").into());
        assert_eq!(Namespace::MagicEffect, (*b"MGEF").into());
        assert_eq!(Namespace::Land, (*b"LAND").into());
        assert_eq!(Namespace::PathGrid, (*b"PGRD").into());
        assert_eq!(Namespace::Dialog, (*b"DIAL").into());
    }

    #[test]
    fn namespace_from_array_should_put_unrecognised_record_types_into_other_namespace() {
        assert_eq!(Namespace::Other, (*b"    ").into());
        assert_eq!(Namespace::Other, (*b"DUMY").into());

        for record_type in OTHER_RECORD_TYPES {
            assert_eq!(Namespace::Other, (**record_type).into());
        }
    }

    #[test]
    fn namespaced_id_new_should_hash_id_data_and_map_record_type_to_namespace() {
        let data = vec![1, 2, 3, 4];
        let record_id = NamespacedId::new(*b"BOOK", &data);

        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        let hashed_data = hasher.finish();

        assert_eq!(Namespace::Other, record_id.namespace);
        assert_eq!(hashed_data, record_id.hashed_id);
    }
}

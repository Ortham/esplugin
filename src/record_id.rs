use std::cmp::Ordering;
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
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ops::RangeInclusive;

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
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
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

impl From<Namespace> for u32 {
    fn from(value: Namespace) -> u32 {
        value as u32
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ObjectIndexMask {
    Full = 0x00FF_FFFF,
    Medium = 0x0000_FFFF,
    Small = 0x0000_0FFF,
}

impl From<ObjectIndexMask> for u32 {
    fn from(value: ObjectIndexMask) -> u32 {
        value as u32
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourcePlugin {
    pub hashed_name: u64,
    /// mod_index_mask is not used when the SourcePlugin is used to represent the plugin that a FormID is found in.
    pub mod_index_mask: u32,
    pub object_index_mask: u32,
}

impl SourcePlugin {
    pub fn master(name: &str, mod_index_mask: u32, object_index_mask: ObjectIndexMask) -> Self {
        let object_index_mask = u32::from(object_index_mask);

        SourcePlugin {
            hashed_name: calculate_filename_hash(name),
            mod_index_mask,
            object_index_mask,
        }
    }

    pub fn parent(name: &str, object_index_mask: ObjectIndexMask) -> Self {
        let object_index_mask = u32::from(object_index_mask);

        // Set mod_index_mask to object_index_mask because it should be unused but needs a value, and object_index_mask is obviously wrong (if a parent SourcePlugin is used as a master SourcePlugin, it'll never match any of the plugin's raw FormIDs).
        SourcePlugin {
            hashed_name: calculate_filename_hash(name),
            mod_index_mask: object_index_mask,
            object_index_mask,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum RecordIdType {
    FormId,
    NamespacedId,
}

#[derive(Clone, Debug)]
pub struct ResolvedRecordId {
    record_id_type: RecordIdType,
    overridden_record: bool,
    hashed_data: u64,
    other_data: u32,
}

impl ResolvedRecordId {
    pub fn from_form_id(
        parent_plugin: SourcePlugin,
        masters: &[SourcePlugin],
        raw_form_id: u32,
    ) -> Self {
        let source_master = masters
            .iter()
            .find(|m| (raw_form_id & !m.object_index_mask) == m.mod_index_mask);

        match source_master {
            Some(hashed_master) => {
                let object_index = raw_form_id & hashed_master.object_index_mask;
                ResolvedRecordId {
                    record_id_type: RecordIdType::FormId,
                    overridden_record: true,
                    hashed_data: hashed_master.hashed_name,
                    other_data: object_index,
                }
            }
            None => {
                let object_index = raw_form_id & parent_plugin.object_index_mask;
                ResolvedRecordId {
                    record_id_type: RecordIdType::FormId,
                    overridden_record: false,
                    hashed_data: parent_plugin.hashed_name,
                    other_data: object_index,
                }
            }
        }
    }

    pub fn from_namespaced_id(
        namespaced_id: &NamespacedId,
        masters_record_ids: &HashSet<NamespacedId>,
    ) -> Self {
        let overridden_record = masters_record_ids.contains(namespaced_id);

        ResolvedRecordId {
            record_id_type: RecordIdType::NamespacedId,
            overridden_record,
            hashed_data: namespaced_id.hashed_id,
            other_data: namespaced_id.namespace.into(),
        }
    }

    pub fn is_overridden_record(&self) -> bool {
        self.overridden_record
    }

    pub fn is_object_index_in(&self, range: &RangeInclusive<u32>) -> bool {
        match self.record_id_type {
            RecordIdType::FormId => range.contains(&self.other_data),
            RecordIdType::NamespacedId => false,
        }
    }
}

impl Ord for ResolvedRecordId {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.record_id_type.cmp(&other.record_id_type) {
            Ordering::Equal => match self.other_data.cmp(&other.other_data) {
                Ordering::Equal => self.hashed_data.cmp(&other.hashed_data),
                o => o,
            },
            o => o,
        }
    }
}

impl PartialOrd for ResolvedRecordId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ResolvedRecordId {
    fn eq(&self, other: &Self) -> bool {
        self.record_id_type == other.record_id_type
            && self.other_data == other.other_data
            && self.hashed_data == other.hashed_data
    }
}

impl Eq for ResolvedRecordId {}

impl Hash for ResolvedRecordId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.record_id_type.hash(state);
        self.other_data.hash(state);
        self.hashed_data.hash(state);
    }
}

pub fn calculate_filename_hash(string: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    string.to_lowercase().hash(&mut hasher);
    hasher.finish()
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

    mod source_plugin {
        use super::*;

        #[test]
        fn parent_should_use_object_index_mask_as_mod_index_mask() {
            let plugin = SourcePlugin::parent("a", ObjectIndexMask::Full);

            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.object_index_mask);
            assert_eq!(plugin.mod_index_mask, plugin.object_index_mask);
        }
    }

    mod resolved_record_id {
        use super::*;

        const PARENT_PLUGIN_NAME: u64 = 1;
        const PARENT_PLUGIN: SourcePlugin = SourcePlugin {
            hashed_name: PARENT_PLUGIN_NAME,
            mod_index_mask: ObjectIndexMask::Full as u32,
            object_index_mask: ObjectIndexMask::Full as u32,
        };
        const OTHER_PARENT_PLUGIN: SourcePlugin = SourcePlugin {
            hashed_name: 6,
            mod_index_mask: ObjectIndexMask::Full as u32,
            object_index_mask: ObjectIndexMask::Full as u32,
        };
        const MASTERS: &[SourcePlugin] = &[
            SourcePlugin {
                hashed_name: 2,
                mod_index_mask: 0,
                object_index_mask: ObjectIndexMask::Full as u32,
            },
            SourcePlugin {
                hashed_name: 3,
                mod_index_mask: 0x1200_0000,
                object_index_mask: ObjectIndexMask::Full as u32,
            },
            SourcePlugin {
                hashed_name: 4,
                mod_index_mask: 0xFD12_0000,
                object_index_mask: ObjectIndexMask::Medium as u32,
            },
            SourcePlugin {
                hashed_name: 5,
                mod_index_mask: 0xFE12_3000,
                object_index_mask: ObjectIndexMask::Small as u32,
            },
        ];
        const NO_MASTERS: &[SourcePlugin] = &[];

        fn hash(form_id: &ResolvedRecordId) -> u64 {
            let mut hasher = DefaultHasher::new();
            form_id.hash(&mut hasher);
            hasher.finish()
        }

        #[test]
        fn new_should_match_override_record_to_master_based_on_mod_index() {
            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x00456789);

            assert!(form_id.is_overridden_record());
            assert_eq!(0x456789, form_id.other_data);
            assert_eq!(MASTERS[0].hashed_name, form_id.hashed_data);

            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x12456789);

            assert!(form_id.is_overridden_record());
            assert_eq!(0x456789, form_id.other_data);
            assert_eq!(MASTERS[1].hashed_name, form_id.hashed_data);

            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0xFD126789);

            assert!(form_id.is_overridden_record());
            assert_eq!(0x6789, form_id.other_data);
            assert_eq!(MASTERS[2].hashed_name, form_id.hashed_data);

            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0xFE123789);

            assert!(form_id.is_overridden_record());
            assert_eq!(0x789, form_id.other_data);
            assert_eq!(MASTERS[3].hashed_name, form_id.hashed_data);
        }

        #[test]
        fn new_should_create_non_override_formid_if_no_master_mod_indexes_match() {
            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01456789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x456789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);

            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x20456789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x456789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);

            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0xFD216789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x216789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);

            let form_id = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0xFE321789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x321789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);
        }

        #[test]
        fn new_should_use_parent_source_plugin_other_data_mask_if_no_master_mod_indexes_match() {
            let parent_plugin = SourcePlugin {
                hashed_name: PARENT_PLUGIN_NAME,
                mod_index_mask: u32::from(ObjectIndexMask::Full),
                object_index_mask: u32::from(ObjectIndexMask::Full),
            };
            let form_id = ResolvedRecordId::from_form_id(parent_plugin, MASTERS, 0x01456789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x456789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);

            let parent_plugin = SourcePlugin {
                hashed_name: PARENT_PLUGIN_NAME,
                mod_index_mask: u32::from(ObjectIndexMask::Medium),
                object_index_mask: u32::from(ObjectIndexMask::Medium),
            };
            let form_id = ResolvedRecordId::from_form_id(parent_plugin, MASTERS, 0xFD216789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x6789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);

            let parent_plugin = SourcePlugin {
                hashed_name: PARENT_PLUGIN_NAME,
                mod_index_mask: u32::from(ObjectIndexMask::Small),
                object_index_mask: u32::from(ObjectIndexMask::Small),
            };
            let form_id = ResolvedRecordId::from_form_id(parent_plugin, MASTERS, 0xFE321789);

            assert!(!form_id.is_overridden_record());
            assert_eq!(0x789, form_id.other_data);
            assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_data);
        }

        #[test]
        fn form_ids_should_not_be_equal_if_plugin_names_are_unequal() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(OTHER_PARENT_PLUGIN, MASTERS, 0x05000001);

            assert_ne!(form_id1, form_id2);
        }

        #[test]
        fn form_ids_should_not_be_equal_if_object_indices_are_unequal() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x02);

            assert_ne!(form_id1, form_id2);
        }

        #[test]
        fn form_ids_with_equal_plugin_names_and_object_ids_should_be_equal() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, NO_MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x05000001);

            assert_eq!(form_id1, form_id2);
        }

        #[test]
        fn form_ids_can_be_equal_if_one_is_an_override_record_and_the_other_is_not() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(MASTERS[0], NO_MASTERS, 0x05000001);

            assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
            assert_eq!(form_id1, form_id2);
        }

        #[test]
        fn form_ids_should_be_ordered_according_to_object_index_then_hashed_datas() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x02);

            assert_eq!(Ordering::Less, form_id1.cmp(&form_id2));
            assert_eq!(Ordering::Greater, form_id2.cmp(&form_id1));

            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x05000001);
            let form_id2 = ResolvedRecordId::from_form_id(OTHER_PARENT_PLUGIN, MASTERS, 0x05000001);

            assert_eq!(Ordering::Less, form_id1.cmp(&form_id2));
            assert_eq!(Ordering::Greater, form_id2.cmp(&form_id1));
        }

        #[test]
        fn form_ids_should_not_be_ordered_according_to_override_record_flag_value() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(MASTERS[0], NO_MASTERS, 0x05000001);

            assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
            assert_eq!(Ordering::Equal, form_id2.cmp(&form_id1));
        }

        #[test]
        fn form_id_hashes_should_not_be_equal_if_plugin_names_are_unequal() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(OTHER_PARENT_PLUGIN, MASTERS, 0x05000001);

            assert_ne!(hash(&form_id1), hash(&form_id2));
        }

        #[test]
        fn form_id_hashes_should_not_be_equal_if_object_indices_are_unequal() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x02);

            assert_ne!(hash(&form_id1), hash(&form_id2));
        }

        #[test]
        fn form_id_hashes_with_equal_plugin_names_and_object_ids_should_be_equal() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, NO_MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x05000001);

            assert_eq!(hash(&form_id1), hash(&form_id2));
        }

        #[test]
        fn form_id_hashes_can_be_equal_with_unequal_override_record_flag_values() {
            let form_id1 = ResolvedRecordId::from_form_id(PARENT_PLUGIN, MASTERS, 0x01);
            let form_id2 = ResolvedRecordId::from_form_id(MASTERS[0], NO_MASTERS, 0x05000001);

            assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
            assert_eq!(hash(&form_id1), hash(&form_id2));
        }
    }

    #[test]
    fn calculate_filename_hash_should_treat_plugin_names_case_insensitively_like_windows() {
        // \u03a1 is greek rho uppercase 'Ρ'
        // \u03c1 is greek rho lowercase 'ρ'
        // \u03f1 is greek rho 'ϱ'
        // \u0130 is turkish 'İ'
        // \u0131 is turkish 'ı'

        // I and i are the same, but İ and ı are different to them and each
        // other.
        assert_eq!(calculate_filename_hash("i"), calculate_filename_hash("I"));
        assert_ne!(
            calculate_filename_hash("\u{0130}"),
            calculate_filename_hash("i")
        );
        assert_ne!(
            calculate_filename_hash("\u{0131}"),
            calculate_filename_hash("i")
        );
        assert_ne!(
            calculate_filename_hash("\u{0131}"),
            calculate_filename_hash("\u{0130}")
        );

        // Windows filesystem treats Ρ and ρ as the same, but ϱ is different.
        assert_eq!(
            calculate_filename_hash("\u{03a1}"),
            calculate_filename_hash("\u{03c1}")
        );
        assert_ne!(
            calculate_filename_hash("\u{03a1}"),
            calculate_filename_hash("\u{03f1}")
        );

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

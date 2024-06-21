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
use std::cmp::Ordering;
use std::hash::{DefaultHasher, Hash, Hasher};

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

#[derive(Clone, Debug)]
pub struct HashedFormId {
    overridden_record: bool,
    object_index: u32,
    hashed_plugin_name: u64,
}

impl HashedFormId {
    pub fn new(parent_plugin: SourcePlugin, masters: &[SourcePlugin], raw_form_id: u32) -> Self {
        let source_master = masters
            .iter()
            .find(|m| (raw_form_id & !m.object_index_mask) == m.mod_index_mask);

        match source_master {
            Some(hashed_master) => HashedFormId {
                overridden_record: true,
                object_index: raw_form_id & hashed_master.object_index_mask,
                hashed_plugin_name: hashed_master.hashed_name,
            },
            None => HashedFormId {
                overridden_record: false,
                object_index: raw_form_id & parent_plugin.object_index_mask,
                hashed_plugin_name: parent_plugin.hashed_name,
            },
        }
    }

    pub fn object_index(&self) -> u32 {
        self.object_index
    }

    pub fn is_overridden_record(&self) -> bool {
        self.overridden_record
    }
}

impl Ord for HashedFormId {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.object_index.cmp(&other.object_index) {
            Ordering::Equal => self.hashed_plugin_name.cmp(&other.hashed_plugin_name),
            o => o,
        }
    }
}

impl PartialOrd for HashedFormId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for HashedFormId {
    fn eq(&self, other: &Self) -> bool {
        self.object_index == other.object_index
            && self.hashed_plugin_name == other.hashed_plugin_name
    }
}

impl Eq for HashedFormId {}

impl Hash for HashedFormId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.object_index.hash(state);
        self.hashed_plugin_name.hash(state);
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

    mod source_plugin {
        use super::*;

        #[test]
        fn parent_should_use_object_index_mask_as_mod_index_mask() {
            let plugin = SourcePlugin::parent("a", ObjectIndexMask::Full);

            assert_eq!(u32::from(ObjectIndexMask::Full), plugin.object_index_mask);
            assert_eq!(plugin.mod_index_mask, plugin.object_index_mask);
        }
    }

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

    fn hash(form_id: &HashedFormId) -> u64 {
        let mut hasher = DefaultHasher::new();
        form_id.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn new_should_match_override_record_to_master_based_on_mod_index() {
        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x00456789);

        assert!(form_id.is_overridden_record());
        assert_eq!(0x456789, form_id.object_index);
        assert_eq!(MASTERS[0].hashed_name, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x12456789);

        assert!(form_id.is_overridden_record());
        assert_eq!(0x456789, form_id.object_index);
        assert_eq!(MASTERS[1].hashed_name, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0xFD126789);

        assert!(form_id.is_overridden_record());
        assert_eq!(0x6789, form_id.object_index);
        assert_eq!(MASTERS[2].hashed_name, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0xFE123789);

        assert!(form_id.is_overridden_record());
        assert_eq!(0x789, form_id.object_index);
        assert_eq!(MASTERS[3].hashed_name, form_id.hashed_plugin_name);
    }

    #[test]
    fn new_should_create_non_override_formid_if_no_master_mod_indexes_match() {
        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01456789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x456789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x20456789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x456789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0xFD216789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x216789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0xFE321789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x321789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);
    }

    #[test]
    fn new_should_use_parent_source_plugin_object_index_mask_if_no_master_mod_indexes_match() {
        let parent_plugin = SourcePlugin {
            hashed_name: PARENT_PLUGIN_NAME,
            mod_index_mask: u32::from(ObjectIndexMask::Full),
            object_index_mask: u32::from(ObjectIndexMask::Full),
        };
        let form_id = HashedFormId::new(parent_plugin, MASTERS, 0x01456789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x456789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);

        let parent_plugin = SourcePlugin {
            hashed_name: PARENT_PLUGIN_NAME,
            mod_index_mask: u32::from(ObjectIndexMask::Medium),
            object_index_mask: u32::from(ObjectIndexMask::Medium),
        };
        let form_id = HashedFormId::new(parent_plugin, MASTERS, 0xFD216789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x6789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);

        let parent_plugin = SourcePlugin {
            hashed_name: PARENT_PLUGIN_NAME,
            mod_index_mask: u32::from(ObjectIndexMask::Small),
            object_index_mask: u32::from(ObjectIndexMask::Small),
        };
        let form_id = HashedFormId::new(parent_plugin, MASTERS, 0xFE321789);

        assert!(!form_id.is_overridden_record());
        assert_eq!(0x789, form_id.object_index);
        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);
    }

    #[test]
    fn form_ids_should_not_be_equal_if_plugin_names_are_unequal() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(OTHER_PARENT_PLUGIN, MASTERS, 0x05000001);

        assert_ne!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_should_not_be_equal_if_object_indices_are_unequal() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x02);

        assert_ne!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_with_equal_plugin_names_and_object_ids_should_be_equal() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, NO_MASTERS, 0x01);
        let form_id2 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x05000001);

        assert_eq!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_can_be_equal_if_one_is_an_override_record_and_the_other_is_not() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(MASTERS[0], NO_MASTERS, 0x05000001);

        assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
        assert_eq!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_should_be_ordered_according_to_object_index_then_hashed_plugin_names() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x02);

        assert_eq!(Ordering::Less, form_id1.cmp(&form_id2));
        assert_eq!(Ordering::Greater, form_id2.cmp(&form_id1));

        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x05000001);
        let form_id2 = HashedFormId::new(OTHER_PARENT_PLUGIN, MASTERS, 0x05000001);

        assert_eq!(Ordering::Less, form_id1.cmp(&form_id2));
        assert_eq!(Ordering::Greater, form_id2.cmp(&form_id1));
    }

    #[test]
    fn form_ids_should_not_be_ordered_according_to_override_record_flag_value() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(MASTERS[0], NO_MASTERS, 0x05000001);

        assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
        assert_eq!(Ordering::Equal, form_id2.cmp(&form_id1));
    }

    #[test]
    fn form_id_hashes_should_not_be_equal_if_plugin_names_are_unequal() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(OTHER_PARENT_PLUGIN, MASTERS, 0x05000001);

        assert_ne!(hash(&form_id1), hash(&form_id2));
    }

    #[test]
    fn form_id_hashes_should_not_be_equal_if_object_indices_are_unequal() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x02);

        assert_ne!(hash(&form_id1), hash(&form_id2));
    }

    #[test]
    fn form_id_hashes_with_equal_plugin_names_and_object_ids_should_be_equal() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, NO_MASTERS, 0x01);
        let form_id2 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x05000001);

        assert_eq!(hash(&form_id1), hash(&form_id2));
    }

    #[test]
    fn form_id_hashes_can_be_equal_with_unequal_override_record_flag_values() {
        let form_id1 = HashedFormId::new(PARENT_PLUGIN, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(MASTERS[0], NO_MASTERS, 0x05000001);

        assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
        assert_eq!(hash(&form_id1), hash(&form_id2));
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

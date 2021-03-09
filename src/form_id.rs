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
use std::hash::{Hash, Hasher};

use crate::game_id::GameId;

#[derive(Clone, Debug)]
pub struct HashedFormId {
    // Precalculate and store whether this FormID is for an overridden record
    // for efficiency, as alignment padding means it wastes no space.
    overridden_record: bool,
    game_id: GameId,
    object_index: u32,
    hashed_plugin_name: u64,
}

impl HashedFormId {
    pub fn new(
        game_id: GameId,
        hashed_parent_plugin_name: u64,
        hashed_masters: &[u64],
        raw_form_id: u32,
    ) -> Self {
        let mod_index = raw_form_id >> 24;

        Self {
            overridden_record: (mod_index as usize) < hashed_masters.len(),
            game_id,
            object_index: raw_form_id & 0xFF_FFFF,
            hashed_plugin_name: hashed_masters
                .get(mod_index as usize)
                .cloned()
                .unwrap_or(hashed_parent_plugin_name),
        }
    }

    pub fn is_valid_in_light_plugin(&self) -> bool {
        match self.game_id {
            GameId::SkyrimSE => self.object_index >= 0x800 && self.object_index <= 0xFFF,
            GameId::Fallout4 => self.object_index >= 0x001 && self.object_index <= 0xFFF,
            _ => false,
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::hash_map::DefaultHasher;

    const PARENT_PLUGIN_NAME: u64 = 2;
    const OTHER_PLUGIN_NAME: u64 = 3;
    const MASTERS: &[u64] = &[0, 1];
    const NO_MASTERS: &[u64] = &[];

    fn hash(form_id: &HashedFormId) -> u64 {
        let mut hasher = DefaultHasher::new();
        form_id.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn is_overridden_record_should_be_true_if_mod_index_is_less_than_masters_length() {
        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);

        assert!(form_id.is_overridden_record());

        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01000001);

        assert!(form_id.is_overridden_record());
    }

    #[test]
    fn is_overridden_record_should_be_false_if_mod_index_is_not_less_than_masters_length() {
        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x02000001);

        assert!(!form_id.is_overridden_record());

        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x03000001);

        assert!(!form_id.is_overridden_record());
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_morrowind() {
        // Check every possible object ID, it's only takes a second or so.
        for index in 0..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::Morrowind, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_oblivion() {
        // Check every possible object ID, it's only takes a second or so.
        for index in 0..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_skyrim() {
        // Check every possible object ID, it's only takes a second or so.
        for index in 0..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::Skyrim, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_fallout_3() {
        // Check every possible object ID, it's only takes a second or so.
        for index in 0..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::Fallout3, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_fallout_nv() {
        // Check every possible object ID, it's only takes a second or so.
        for index in 0..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::FalloutNV, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_true_if_game_is_skyrim_se_and_object_index_is_between_0x800_and_0xFFF_inclusive(
    ) {
        for index in 0x800..=0xFFF {
            let form_id = HashedFormId::new(GameId::SkyrimSE, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_true_if_game_is_fallout_4_and_object_index_is_between_0x001_and_0xFFF_inclusive(
    ) {
        for index in 0x001..=0xFFF {
            let form_id = HashedFormId::new(GameId::Fallout4, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_skyrim_se_and_object_index_is_outwith_0x800_and_0xFFF_inclusive(
    ) {
        for index in 0..0x800 {
            let form_id = HashedFormId::new(GameId::SkyrimSE, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }

        for index in 0x1000..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::SkyrimSE, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[allow(non_snake_case)]
    #[test]
    fn is_valid_in_light_plugin_should_be_false_if_game_is_fallout_4_and_object_index_is_outwith_0x001_and_0xFFF_inclusive(
    ) {
        let form_id = HashedFormId::new(GameId::Fallout4, PARENT_PLUGIN_NAME, MASTERS, 0);

        assert!(!form_id.is_valid_in_light_plugin());

        for index in 0x1000..=0xFF_FFFF {
            let form_id = HashedFormId::new(GameId::Fallout4, PARENT_PLUGIN_NAME, MASTERS, index);

            assert!(!form_id.is_valid_in_light_plugin());
        }
    }

    #[test]
    fn object_index_should_equal_last_three_bytes_of_raw_form_id_value() {
        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);

        assert_eq!(0x01, form_id.object_index);

        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01000001);

        assert_eq!(0x01, form_id.object_index);
    }

    #[test]
    fn should_store_master_at_mod_index_as_plugin_name() {
        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);

        assert_eq!(MASTERS[0], form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01000001);

        assert_eq!(MASTERS[1], form_id.hashed_plugin_name);
    }

    #[test]
    fn should_store_parent_plugin_name_for_mod_index_greater_than_last_index_of_masters() {
        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, NO_MASTERS, 0x01);

        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);

        let form_id = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_eq!(PARENT_PLUGIN_NAME, form_id.hashed_plugin_name);
    }

    #[test]
    fn form_ids_should_not_be_equal_if_plugin_names_are_unequal() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, OTHER_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_ne!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_should_not_be_equal_if_object_indices_are_unequal() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x02);

        assert_ne!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_with_equal_plugin_names_and_object_ids_should_be_equal() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, NO_MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_eq!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_can_be_equal_if_one_is_an_override_record_and_the_other_is_not() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, MASTERS[0], NO_MASTERS, 0x05000001);

        assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
        assert_eq!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_should_be_ordered_according_to_object_index_then_hashed_plugin_names() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x02);

        assert_eq!(Ordering::Less, form_id1.cmp(&form_id2));
        assert_eq!(Ordering::Greater, form_id2.cmp(&form_id1));

        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x05000001);
        let form_id2 = HashedFormId::new(GameId::Oblivion, OTHER_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_eq!(Ordering::Less, form_id1.cmp(&form_id2));
        assert_eq!(Ordering::Greater, form_id2.cmp(&form_id1));
    }

    #[test]
    fn form_ids_should_not_be_ordered_according_to_override_record_flag_value() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, MASTERS[0], NO_MASTERS, 0x05000001);

        assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
        assert_eq!(Ordering::Equal, form_id2.cmp(&form_id1));
    }

    #[test]
    fn form_id_hashes_should_not_be_equal_if_plugin_names_are_unequal() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, OTHER_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_ne!(hash(&form_id1), hash(&form_id2));
    }

    #[test]
    fn form_id_hashes_should_not_be_equal_if_object_indices_are_unequal() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x02);

        assert_ne!(hash(&form_id1), hash(&form_id2));
    }

    #[test]
    fn form_id_hashes_with_equal_plugin_names_and_object_ids_should_be_equal() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, NO_MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_eq!(hash(&form_id1), hash(&form_id2));
    }

    #[test]
    fn form_id_hashes_can_be_equal_with_unequal_override_record_flag_values() {
        let form_id1 = HashedFormId::new(GameId::Oblivion, PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = HashedFormId::new(GameId::Oblivion, MASTERS[0], NO_MASTERS, 0x05000001);

        assert_ne!(form_id1.overridden_record, form_id2.overridden_record);
        assert_eq!(hash(&form_id1), hash(&form_id2));
    }
}

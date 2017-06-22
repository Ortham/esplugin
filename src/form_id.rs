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

use libc::{c_char, uint32_t};

use ffi::helpers::*;
use ffi::constants::*;

#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct FormId {
    pub object_index: u32,
    pub plugin_name: String,
}

impl FormId {
    pub fn new(parent_plugin_name: &str, masters: &[&str], raw_form_id: u32) -> FormId {
        let mod_index = (raw_form_id >> 24) as usize;

        FormId {
            object_index: raw_form_id & 0xFFFFFF,
            plugin_name: masters
                .get(mod_index)
                .unwrap_or(&parent_plugin_name)
                .to_string(),
        }
    }
}

#[no_mangle]
pub extern "C" fn espm_formid_new(
    formid_ptr_ptr: *mut *const FormId,
    parent_plugin_name: *const c_char,
    masters: *const *const c_char,
    masters_count: uint32_t,
    raw_form_id: uint32_t,
) -> u32 {
    let rust_name = match to_str(parent_plugin_name) {
        Ok(x) => x,
        Err(x) => return x,
    };
    let rust_masters = match to_str_vec(masters, masters_count as isize) {
        Ok(x) => x,
        Err(x) => return x,
    };

    let formid = FormId::new(rust_name, &rust_masters, raw_form_id);
    unsafe {
        *formid_ptr_ptr = Box::into_raw(Box::new(formid));
    };

    ESPM_OK
}

#[no_mangle]
pub extern "C" fn espm_formid_free(formid_ptr: *mut FormId) {
    if !formid_ptr.is_null() {
        unsafe {
            Box::from_raw(formid_ptr);
        }
    }
}

#[no_mangle]
pub extern "C" fn espm_formid_plugin_name(
    name: *mut *mut c_char,
    formid_ptr: *const FormId,
) -> u32 {
    if name.is_null() || formid_ptr.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let formid = unsafe { &*formid_ptr };
        let c_string = match to_c_string(&formid.plugin_name) {
            Ok(x) => x,
            Err(x) => return x,
        };

        unsafe {
            *name = c_string;
        }

        ESPM_OK
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const PARENT_PLUGIN_NAME: &'static str = "plugin0";
    const OTHER_PLUGIN_NAME: &'static str = "plugin1";
    const MASTERS: &'static [&'static str] = &["plugin2", "plugin3"];
    const NO_MASTERS: &'static [&'static str] = &[];

    #[test]
    fn object_index_should_equal_last_three_bytes_of_raw_form_id_value() {
        let form_id = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x01);

        assert_eq!(0x01, form_id.object_index);

        let form_id = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x01000001);

        assert_eq!(0x01, form_id.object_index);
    }

    #[test]
    fn should_store_master_at_mod_index_as_plugin_name() {
        let form_id = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x01);

        assert_eq!(MASTERS[0], form_id.plugin_name);

        let form_id = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x01000001);

        assert_eq!(MASTERS[1], form_id.plugin_name);
    }

    #[test]
    fn should_store_parent_plugin_name_for_mod_index_greater_than_last_index_of_masters() {
        let form_id = FormId::new(PARENT_PLUGIN_NAME, NO_MASTERS, 0x01);

        assert_eq!(PARENT_PLUGIN_NAME, form_id.plugin_name);

        let form_id = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_eq!(PARENT_PLUGIN_NAME, form_id.plugin_name);
    }

    #[test]
    fn form_ids_should_not_be_equal_if_plugin_names_are_unequal() {
        let form_id1 = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = FormId::new(OTHER_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_ne!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_should_not_be_equal_if_object_indices_are_unequal() {
        let form_id1 = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x01);
        let form_id2 = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x02);

        assert_ne!(form_id1, form_id2);
    }

    #[test]
    fn form_ids_with_equal_plugin_names_and_object_ids_should_be_equal() {
        let form_id1 = FormId::new(PARENT_PLUGIN_NAME, NO_MASTERS, 0x01);
        let form_id2 = FormId::new(PARENT_PLUGIN_NAME, MASTERS, 0x05000001);

        assert_eq!(form_id1, form_id2);
    }
}

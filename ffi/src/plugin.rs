use std::mem;
use std::path::Path;
use std::ptr;
use libc::{c_char, size_t, uint8_t, uint32_t};

use espm::Plugin;
use constants::*;
use helpers::*;

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_new(
    plugin_ptr_ptr: *mut *mut Plugin,
    game_id: uint32_t,
    path: *const c_char,
) -> uint32_t {
    let rust_path = match to_str(path) {
        Ok(x) => Path::new(x),
        Err(x) => return x,
    };

    let mapped_game_id = match map_game_id(game_id) {
        Ok(x) => x,
        Err(x) => return x,
    };

    let plugin = Plugin::new(mapped_game_id, rust_path);
    *plugin_ptr_ptr = Box::into_raw(Box::new(plugin));

    ESPM_OK
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_free(plugin_ptr: *mut Plugin) {
    if !plugin_ptr.is_null() {
        Box::from_raw(plugin_ptr);
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_parse(
    plugin_ptr: *mut Plugin,
    load_header_only: bool,
) -> uint32_t {
    if plugin_ptr.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &mut *plugin_ptr;
        match plugin.parse_mmapped_file(load_header_only) {
            Ok(_) => ESPM_OK,
            Err(_) => ESPM_ERROR_PARSE_ERROR,
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_filename(
    plugin_ptr: *const Plugin,
    filename: *mut *mut c_char,
) -> uint32_t {
    if filename.is_null() || plugin_ptr.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        let c_string: *mut c_char = match plugin.filename().map(|s| to_c_string(&s)) {
            None => ptr::null_mut(),
            Some(Ok(x)) => x,
            Some(Err(x)) => return x,
        };

        *filename = c_string;

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_masters(
    plugin_ptr: *const Plugin,
    plugin_masters: *mut *mut *mut c_char,
    plugin_masters_size: *mut uint8_t,
) -> uint32_t {
    if plugin_masters.is_null() || plugin_ptr.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        let masters_vec = match plugin.masters() {
            Ok(x) => x.iter().map(|m| to_c_string(m)).collect(),
            Err(_) => return ESPM_ERROR_NOT_UTF8,
        };

        let mut c_string_vec: Vec<*mut c_char> = match masters_vec {
            Ok(x) => x,
            Err(x) => return x,
        };

        c_string_vec.shrink_to_fit();

        *plugin_masters = c_string_vec.as_mut_ptr();
        *plugin_masters_size = c_string_vec.len() as u8;

        mem::forget(c_string_vec);

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_is_master(
    plugin_ptr: *const Plugin,
    is_master: *mut bool,
) -> uint32_t {
    if plugin_ptr.is_null() || is_master.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        *is_master = plugin.is_master_file();

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_is_light_master(
    plugin_ptr: *const Plugin,
    is_light_master: *mut bool,
) -> uint32_t {
    if plugin_ptr.is_null() || is_light_master.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        *is_light_master = plugin.is_light_master_file();

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_is_valid(
    game_id: uint32_t,
    path: *const c_char,
    load_header_only: bool,
    is_valid: *mut bool,
) -> uint32_t {
    if path.is_null() || is_valid.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let rust_path = match to_str(path) {
            Ok(x) => Path::new(x),
            Err(x) => return x,
        };

        let mapped_game_id = match map_game_id(game_id) {
            Ok(x) => x,
            Err(x) => return x,
        };

        *is_valid = Plugin::is_valid(mapped_game_id, rust_path, load_header_only);

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_description(
    plugin_ptr: *const Plugin,
    description: *mut *const c_char,
) -> uint32_t {
    if description.is_null() || plugin_ptr.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        let description_option = match plugin.description() {
            Ok(x) => x.map(|d| to_c_string(&d)),
            Err(_) => return ESPM_ERROR_NOT_UTF8,
        };

        let c_string = match description_option {
            None => ptr::null(),
            Some(Ok(x)) => x,
            Some(Err(x)) => return x,
        };

        *description = c_string;

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_is_empty(
    plugin_ptr: *const Plugin,
    is_empty: *mut bool,
) -> uint32_t {
    if plugin_ptr.is_null() || is_empty.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        *is_empty = plugin.record_and_group_count().unwrap_or(0) == 0;

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_count_override_records(
    plugin_ptr: *const Plugin,
    count: *mut size_t,
) -> uint32_t {
    if plugin_ptr.is_null() || count.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        *count = plugin.count_override_records();

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_do_records_overlap(
    plugin_ptr: *const Plugin,
    other_plugin_ptr: *const Plugin,
    overlap: *mut bool,
) -> uint32_t {
    if plugin_ptr.is_null() || other_plugin_ptr.is_null() || overlap.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;
        let other_plugin = &*other_plugin_ptr;

        *overlap = plugin
            .form_ids()
            .intersection(other_plugin.form_ids())
            .count() > 0;

        ESPM_OK
    }
}

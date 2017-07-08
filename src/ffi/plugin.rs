use std::mem;
use std::path::Path;
use std::ptr;
use libc::{c_char, size_t, uint8_t, uint32_t};

use form_id::FormId;
use plugin::Plugin;
use ffi::constants::*;
use ffi::helpers::*;

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_new(
    plugin_ptr_ptr: *mut *const Plugin,
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
    filename: *mut *const c_char,
) -> uint32_t {
    if filename.is_null() || plugin_ptr.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        let c_string = match plugin.filename().map(|s| to_c_string(&s)) {
            None => ptr::null(),
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
pub unsafe extern "C" fn espm_plugin_record_and_group_count(
    plugin_ptr: *const Plugin,
    count: *mut uint32_t,
) -> uint32_t {
    if plugin_ptr.is_null() || count.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        *count = plugin.record_and_group_count().unwrap_or(0);

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_form_ids(
    plugin_ptr: *const Plugin,
    form_ids: *mut *mut *const FormId,
    form_ids_size: *mut size_t,
) -> uint32_t {
    if plugin_ptr.is_null() || form_ids.is_null() {
        ESPM_ERROR_NULL_POINTER
    } else {
        let plugin = &*plugin_ptr;

        let mut plugin_form_ids: Vec<*const FormId> = plugin
            .form_ids()
            .iter()
            .map(|f| f as *const FormId)
            .collect();

        plugin_form_ids.shrink_to_fit();

        *form_ids = plugin_form_ids.as_mut_ptr();
        *form_ids_size = plugin_form_ids.len();

        mem::forget(plugin_form_ids);

        ESPM_OK
    }
}

#[no_mangle]
pub unsafe extern "C" fn espm_plugin_form_ids_free(
    form_ids: *mut *const FormId,
    form_ids_size: size_t,
) {
    if form_ids.is_null() || form_ids_size == 0 {
        return;
    }

    Vec::from_raw_parts(form_ids, form_ids_size, form_ids_size);
}

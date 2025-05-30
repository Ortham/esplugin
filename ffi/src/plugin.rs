use std::ffi::{c_char, c_float};
use std::path::Path;
use std::{f32, panic, ptr};

use libc::size_t;

use esplugin::{ParseOptions, Plugin, PluginMetadata};

use crate::constants::{ESP_ERROR_NULL_POINTER, ESP_ERROR_PANICKED, ESP_OK};
use crate::error::{error, handle_error};
use crate::helpers::{map_game_id, to_c_string, to_c_string_array, to_plugin_refs_slice, to_str};

/// Create a new Plugin handle for the plugin at the given path.
///
/// The details of the plugin format varies between the different games that
/// esplugin supports, so the given `game_id` must the appropriate `ESP_GAME_*`
/// constant.
///
/// The path must be encoded as UTF-8. This function does not read the file at
/// the given `path`: use [`esp_plugin_parse`] to do so.
///
/// Returns [`ESP_OK`] if successful, otherwise an `ESP_ERROR_*` code is returned.
///
/// # Safety
///
/// `plugin_ptr_ptr` must be a valid pointer.
///
/// The lifetime of the output plugin handle is independent of the lifetimes of
/// the inputs, and ownership is passed of it is passed to the caller. It must
/// only be deallocated using [`esp_plugin_free`].
///
/// It is only safe to call this function more than once in parallel with the
/// if each call is passed a different value for `plugin_ptr_ptr`.
#[no_mangle]
pub unsafe extern "C" fn esp_plugin_new(
    plugin_ptr_ptr: *mut *mut Plugin,
    game_id: u32,
    path: *const c_char,
) -> u32 {
    panic::catch_unwind(|| {
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

        ESP_OK
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

/// Free the memory allocated for the given Plugin handle.
///
/// # Safety
///
/// The given `plugin_ptr` must be null or the output of a call to
/// [`esp_plugin_new`], though a null pointer will cause nothing to happen.
///
/// This function must not be called twice for the same `plugin_ptr` value. This
/// function invalidates `plugin_ptr`, which must not be dereferenced after this
/// function is called.
///
/// It is not safe to call this function in parallel with any function that is
/// passed the same plugin handle.
#[no_mangle]
pub unsafe extern "C" fn esp_plugin_free(plugin_ptr: *mut Plugin) {
    if !plugin_ptr.is_null() {
        drop(Box::from_raw(plugin_ptr));
    }
}

/// Reads a plugin file's data into memory.
///
/// If `load_header_only` is true then only the plugin's header will be read.
/// Loading only the header will affect the output of the following functions:
///
/// * [`esp_plugin_count_override_records`]
/// * [`esp_plugin_do_records_overlap`]
/// * [`esp_plugin_records_overlap_size`]
/// * [`esp_plugin_is_valid_as_light_plugin`]
/// * [`esp_plugin_is_valid_as_update_plugin`]
///
/// Returns [`ESP_OK`] if successful, otherwise an `ESP_ERROR_*` code is returned.
///
/// # Safety
///
/// `plugin_ptr` must be null or the output of a call to [`esp_plugin_new`],
/// though a null pointer will cause an error to be returned.
///
/// It is not safe to call this function in parallel with any function that is
/// passed the same plugin handle.
#[no_mangle]
pub unsafe extern "C" fn esp_plugin_parse(plugin_ptr: *mut Plugin, load_header_only: bool) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &mut *plugin_ptr;

            let options = if load_header_only {
                ParseOptions::header_only()
            } else {
                ParseOptions::whole_plugin()
            };

            match plugin.parse_file(options) {
                Ok(()) => ESP_OK,
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_resolve_record_ids(
    plugin_ptr: *mut Plugin,
    plugins_metadata: *const Vec<PluginMetadata>,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &mut *plugin_ptr;
            let plugins_metadata = if plugins_metadata.is_null() {
                &[]
            } else {
                (*plugins_metadata).as_slice()
            };

            match plugin.resolve_record_ids(plugins_metadata) {
                Ok(()) => ESP_OK,
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

/// Outputs the filename associated with the given plugin handle.
///
/// The filename string will be encoded in UTF-8.
///
/// Returns [`ESP_OK`] if successful, otherwise an `ESP_ERROR_*` code is returned.
///
/// # Safety
///
/// `plugin_ptr` must be null or the output of a call to [`esp_plugin_new`],
/// though a null pointer will cause an error to be returned.
///
/// `filename` must be a valid pointer.
///
/// The lifetime of the output `filename` is independent of the lifetime of
/// the plugin handle, and ownership is passed of it is passed to the caller. It
/// must only be deallocated using [`super::esp_string_free`].
///
/// It is safe to call this function in parallel with any function so long as
/// that function does not mutate the same data. I.e. it is not safe to call
/// this function in parallel with [`esp_plugin_free`] or [`esp_plugin_parse`], or
/// to call this function multiple times in parallel while sharing the same
/// `filename` value between calls.
#[no_mangle]
pub unsafe extern "C" fn esp_plugin_filename(
    plugin_ptr: *const Plugin,
    filename: *mut *mut c_char,
) -> u32 {
    panic::catch_unwind(|| {
        if filename.is_null() || plugin_ptr.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            let c_string = match plugin.filename().map(|s| to_c_string(&s)) {
                None => ptr::null_mut(),
                Some(Ok(x)) => x,
                Some(Err(e)) => return error(e, "The filename contained a null byte"),
            };

            *filename = c_string;

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_masters(
    plugin_ptr: *const Plugin,
    plugin_masters: *mut *mut *mut c_char,
    num_plugin_masters: *mut size_t,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_masters.is_null() || plugin_ptr.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            let masters = match plugin.masters() {
                Ok(x) => x,
                Err(e) => return handle_error(&e),
            };

            match to_c_string_array(&masters) {
                Ok((pointer, size)) => {
                    *plugin_masters = pointer;
                    *num_plugin_masters = size;
                    ESP_OK
                }
                Err(x) => error(x, "A filename contained a null byte"),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_master(
    plugin_ptr: *const Plugin,
    is_master: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_master.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *is_master = plugin.is_master_file();

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_light_plugin(
    plugin_ptr: *const Plugin,
    is_light_plugin: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_light_plugin.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *is_light_plugin = plugin.is_light_plugin();

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_medium_plugin(
    plugin_ptr: *const Plugin,
    is_medium_plugin: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_medium_plugin.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *is_medium_plugin = plugin.is_medium_plugin();

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_update_plugin(
    plugin_ptr: *const Plugin,
    is_update_plugin: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_update_plugin.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *is_update_plugin = plugin.is_update_plugin();

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_blueprint_plugin(
    plugin_ptr: *const Plugin,
    is_blueprint_plugin: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_blueprint_plugin.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *is_blueprint_plugin = plugin.is_blueprint_plugin();

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_valid(
    game_id: u32,
    path: *const c_char,
    load_header_only: bool,
    is_valid: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if path.is_null() || is_valid.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let rust_path = match to_str(path) {
                Ok(x) => Path::new(x),
                Err(x) => return x,
            };

            let mapped_game_id = match map_game_id(game_id) {
                Ok(x) => x,
                Err(x) => return x,
            };

            let options = if load_header_only {
                ParseOptions::header_only()
            } else {
                ParseOptions::whole_plugin()
            };

            *is_valid = Plugin::is_valid(mapped_game_id, rust_path, options);

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_description(
    plugin_ptr: *const Plugin,
    description: *mut *mut c_char,
) -> u32 {
    panic::catch_unwind(|| {
        if description.is_null() || plugin_ptr.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            let description_option = match plugin.description() {
                Ok(x) => x,
                Err(e) => return handle_error(&e),
            };

            let c_string = match description_option {
                None => ptr::null_mut(),
                Some(d) => match to_c_string(&d) {
                    Ok(s) => s,
                    Err(e) => return error(e, "The description contained a null byte"),
                },
            };

            *description = c_string;

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_header_version(
    plugin_ptr: *const Plugin,
    header_version: *mut c_float,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || header_version.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            match plugin.header_version() {
                Some(i) => *header_version = i,
                None => *header_version = f32::NAN,
            }

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_empty(
    plugin_ptr: *const Plugin,
    is_empty: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_empty.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *is_empty = plugin.record_and_group_count().unwrap_or(0) == 0;

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_record_and_group_count(
    plugin_ptr: *const Plugin,
    count: *mut u32,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || count.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            *count = plugin.record_and_group_count().unwrap_or(0);

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_count_override_records(
    plugin_ptr: *const Plugin,
    count: *mut size_t,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || count.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            match plugin.count_override_records() {
                Ok(c) => {
                    *count = c;
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_do_records_overlap(
    plugin_ptr: *const Plugin,
    other_plugin_ptr: *const Plugin,
    overlap: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || other_plugin_ptr.is_null() || overlap.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;
            let other_plugin = &*other_plugin_ptr;

            match plugin.overlaps_with(other_plugin) {
                Ok(x) => {
                    *overlap = x;
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_records_overlap_size(
    plugin_ptr: *const Plugin,
    other_plugins_ptr: *const *const Plugin,
    other_plugins_ptr_count: size_t,
    overlap_size: *mut size_t,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || other_plugins_ptr.is_null() || overlap_size.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            let Some(other_plugins) =
                to_plugin_refs_slice(other_plugins_ptr, other_plugins_ptr_count)
            else {
                return error(
                    ESP_ERROR_NULL_POINTER,
                    "Null pointer passed in plugins array",
                );
            };

            match plugin.overlap_size(&other_plugins) {
                Ok(x) => {
                    *overlap_size = x;
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_valid_as_light_plugin(
    plugin_ptr: *const Plugin,
    is_valid: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_valid.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            match plugin.is_valid_as_light_plugin() {
                Ok(x) => {
                    *is_valid = x;
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_valid_as_medium_plugin(
    plugin_ptr: *const Plugin,
    is_valid: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_valid.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            match plugin.is_valid_as_medium_plugin() {
                Ok(x) => {
                    *is_valid = x;
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugin_is_valid_as_update_plugin(
    plugin_ptr: *const Plugin,
    is_valid: *mut bool,
) -> u32 {
    panic::catch_unwind(|| {
        if plugin_ptr.is_null() || is_valid.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let plugin = &*plugin_ptr;

            match plugin.is_valid_as_update_plugin() {
                Ok(x) => {
                    *is_valid = x;
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

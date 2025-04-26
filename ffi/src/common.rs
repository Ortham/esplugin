use std::ffi::{c_char, CString};

use libc::size_t;

/// Free the memory allocated for the given string.
///
/// # Safety
///
/// The given `string` must have been allocated by esplugin, e.g. it could be
/// the output of [`super::esp_plugin_filename`].
///
/// This function must not be called twice for the same `string` value.
///
/// This function is thread-safe.
#[no_mangle]
pub unsafe extern "C" fn esp_string_free(string: *mut c_char) {
    if !string.is_null() {
        drop(CString::from_raw(string));
    }
}

/// Free the memory allocated for the given string array.
///
/// # Safety
///
/// The given `array` must have been allocated by esplugin, e.g. it could be
/// the output of [`super::esp_plugin_masters`].
///
/// The given `size` must be the size of the array that was allocated by
/// esplugin, i.e. it's not valid to free only part of `array`.
///
/// This function must not be called twice for the same `array` value.
///
/// This function is thread-safe.
#[no_mangle]
pub unsafe extern "C" fn esp_string_array_free(array: *mut *mut c_char, size: size_t) {
    if array.is_null() || size == 0 {
        return;
    }

    let strings = Box::from_raw(std::slice::from_raw_parts_mut(array, size));
    for string in &strings {
        esp_string_free(*string);
    }

    drop(strings);
}

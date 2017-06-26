use std::ffi::CString;

use libc::c_char;

use game_id::GameId;

#[no_mangle]
pub extern "C" fn espm_string_free(string: *mut c_char) {
    if !string.is_null() {
        unsafe { CString::from_raw(string) };
    }
}

#[no_mangle]
pub extern "C" fn espm_string_array_free(array: *mut *mut c_char, size: usize) {
    if array.is_null() || size == 0 {
        return;
    }

    unsafe {
        let vec = Vec::from_raw_parts(array, size, size);
        for string in vec {
            espm_string_free(string);
        }
    }
}

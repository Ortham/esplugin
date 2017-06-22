
use libc::c_char;
use std::ffi::{CStr, CString};
use std::str;

pub mod constants {
    #[no_mangle]
    pub static ESPM_OK: u32 = 0;

    #[no_mangle]
    pub static ESPM_ERROR_NULL_POINTER: u32 = 1;

    #[no_mangle]
    pub static ESPM_ERROR_NOT_UTF8: u32 = 2;

    #[no_mangle]
    pub static ESPM_ERROR_STRING_CONTAINS_NUL: u32 = 3;
}

#[no_mangle]
pub extern "C" fn espm_string_free(string: *mut c_char) {
    if !string.is_null() {
        unsafe { CString::from_raw(string) };
    }
}

pub mod helpers {
    use super::*;

    pub fn to_str<'a>(c_string: *const c_char) -> Result<&'a str, u32> {
        if c_string.is_null() {
            return Err(constants::ESPM_ERROR_NULL_POINTER);
        }

        let rust_c_string = unsafe { CStr::from_ptr(c_string) };

        Ok(rust_c_string.to_str().map_err(
            |e| constants::ESPM_ERROR_NOT_UTF8,
        )?)
    }

    pub fn to_str_vec<'a>(
        array: *const *const c_char,
        array_size: isize,
    ) -> Result<Vec<&'a str>, u32> {
        let mut vec: Vec<&str> = Vec::new();

        for i in 0..array_size {
            let string = unsafe { to_str(*array.offset(i))? };
            vec.push(string);
        }

        Ok(vec)
    }

    pub fn to_c_string(string: &String) -> Result<*mut c_char, u32> {
        let c_string_name = CString::new(string.clone()).map_err(|e| {
            constants::ESPM_ERROR_STRING_CONTAINS_NUL
        })?;

        Ok(c_string_name.into_raw())
    }
}

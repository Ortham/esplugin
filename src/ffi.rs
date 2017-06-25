
use std::ffi::{CStr, CString};

use libc::c_char;

pub mod constants {
    #[no_mangle]
    pub static ESPM_OK: u32 = 0;

    #[no_mangle]
    pub static ESPM_ERROR_NULL_POINTER: u32 = 1;

    #[no_mangle]
    pub static ESPM_ERROR_NOT_UTF8: u32 = 2;

    #[no_mangle]
    pub static ESPM_ERROR_STRING_CONTAINS_NUL: u32 = 3;

    #[no_mangle]
    pub static ESPM_ERROR_INVALID_GAME_ID: u32 = 4;

    #[no_mangle]
    pub static ESPM_ERROR_PARSE_ERROR: u32 = 4;
}

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

pub mod helpers {
    use std::str;
    use std::path::PathBuf;

    use game_id::GameId;

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

    pub fn to_c_string(string: &str) -> Result<*mut c_char, u32> {
        let c_string_name = CString::new(string.clone()).map_err(|e| {
            constants::ESPM_ERROR_STRING_CONTAINS_NUL
        })?;

        Ok(c_string_name.into_raw())
    }

    pub fn path_to_c_string(path: &PathBuf) -> Result<*mut c_char, u32> {
        match path.to_str() {
            Some(x) => Ok(to_c_string(x)?),
            None => Err(constants::ESPM_ERROR_NOT_UTF8),
        }
    }

    pub fn map_game_id(game_id: u32) -> Result<GameId, u32> {
        use game_id::*;
        match game_id {
            x if x == ESPM_GAME_OBLIVION => Ok(GameId::Oblivion),
            x if x == ESPM_GAME_SKYRIM => Ok(GameId::Skyrim),
            x if x == ESPM_GAME_FALLOUT3 => Ok(GameId::Fallout3),
            x if x == ESPM_GAME_FALLOUTNV => Ok(GameId::FalloutNV),
            x if x == ESPM_GAME_MORROWIND => Ok(GameId::Morrowind),
            x if x == ESPM_GAME_FALLOUT4 => Ok(GameId::Fallout4),
            _ => Err(constants::ESPM_ERROR_INVALID_GAME_ID),
        }
    }
}

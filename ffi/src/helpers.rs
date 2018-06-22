use std::ffi::{CStr, CString, NulError};

use libc::c_char;

use constants::*;
use esplugin::GameId;

pub unsafe fn to_str<'a>(c_string: *const c_char) -> Result<&'a str, u32> {
    if c_string.is_null() {
        return Err(ESP_ERROR_NULL_POINTER);
    }

    let rust_c_string = CStr::from_ptr(c_string);

    Ok(rust_c_string.to_str().map_err(|_| ESP_ERROR_NOT_UTF8)?)
}

pub fn to_c_string(string: &str) -> *mut c_char {
    CString::new(string)
        .unwrap_or_else(to_truncated_c_string)
        .into_raw()
}

fn to_truncated_c_string(error: NulError) -> CString {
    // This is safe because the bytes vector is truncated to before the first
    // nul byte.
    unsafe {
        let nul_position = error.nul_position();
        let mut bytes = error.into_vec();
        bytes.truncate(nul_position);

        CString::from_vec_unchecked(bytes)
    }
}

pub fn map_game_id(game_id: u32) -> Result<GameId, u32> {
    match game_id {
        x if x == ESP_GAME_OBLIVION => Ok(GameId::Oblivion),
        x if x == ESP_GAME_SKYRIM => Ok(GameId::Skyrim),
        x if x == ESP_GAME_FALLOUT3 => Ok(GameId::Fallout3),
        x if x == ESP_GAME_FALLOUTNV => Ok(GameId::FalloutNV),
        x if x == ESP_GAME_MORROWIND => Ok(GameId::Morrowind),
        x if x == ESP_GAME_FALLOUT4 => Ok(GameId::Fallout4),
        x if x == ESP_GAME_SKYRIMSE => Ok(GameId::SkyrimSE),
        _ => Err(ESP_ERROR_INVALID_GAME_ID),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_c_string_should_not_truncate_a_string_containing_no_nul_chars() {
        let c_string = to_c_string("test without null");
        let rust_c_string = unsafe { CStr::from_ptr(c_string) };

        assert_eq!(
            CString::new("test without null").unwrap().to_bytes(),
            rust_c_string.to_bytes()
        );
    }

    #[test]
    fn to_c_string_should_truncate_string_to_first_nul() {
        let c_string = to_c_string("test\0with null");
        let rust_c_string = unsafe { CStr::from_ptr(c_string) };

        assert_eq!(
            CString::new("test").unwrap().to_bytes(),
            rust_c_string.to_bytes()
        );
    }
}

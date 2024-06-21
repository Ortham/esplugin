use std::ffi::{CStr, CString};

use libc::{c_char, size_t};

use esplugin::GameId;

use crate::{constants::*, error::error};

pub unsafe fn to_str<'a>(c_string: *const c_char) -> Result<&'a str, u32> {
    if c_string.is_null() {
        return Err(error(ESP_ERROR_NULL_POINTER, "Null pointer passed"));
    }

    let rust_c_string = CStr::from_ptr(c_string);

    rust_c_string
        .to_str()
        .map_err(|_| error(ESP_ERROR_NOT_UTF8, "Non-UTF-8 string passed"))
}

pub fn to_c_string(string: &str) -> Result<*mut c_char, u32> {
    CString::new(string).map(CString::into_raw).map_err(|_| {
        error(
            ESP_ERROR_TEXT_ENCODE_ERROR,
            "String could not be converted to a C string as it contained a null byte",
        )
    })
}

pub fn to_truncated_c_string(string: &str) -> *mut c_char {
    CString::new(string)
        .unwrap_or_else(|error| {
            let nul_position = error.nul_position();
            let mut bytes = error.into_vec();
            bytes.truncate(nul_position + 1);

            CString::from_vec_with_nul(bytes).expect("vec should end in a nul byte")
        })
        .into_raw()
}

pub fn to_c_string_array(strings: &[String]) -> Result<(*mut *mut c_char, size_t), u32> {
    let mut c_strings = strings
        .iter()
        .map(|s| to_c_string(s))
        .collect::<Result<Vec<*mut c_char>, u32>>()?;

    c_strings.shrink_to_fit();

    let pointer = c_strings.as_mut_ptr();
    let size = c_strings.len();
    std::mem::forget(c_strings);

    Ok((pointer, size))
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
        x if x == ESP_GAME_STARFIELD => Ok(GameId::Starfield),
        _ => Err(error(
            ESP_ERROR_INVALID_GAME_ID,
            &format!("Invalid game ID: {}", game_id),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_c_string_should_convert_a_string_containing_no_nul_chars() {
        let c_string = to_c_string("test without null").unwrap();
        let rust_c_string = unsafe { CStr::from_ptr(c_string) };

        assert_eq!(
            CString::new("test without null").unwrap().to_bytes(),
            rust_c_string.to_bytes()
        );
    }

    #[test]
    fn to_c_string_should_error_if_a_string_contains_nul_chars() {
        let err = to_c_string("test\0with null").unwrap_err();

        assert_eq!(ESP_ERROR_TEXT_ENCODE_ERROR, err);
    }

    #[test]
    fn to_truncated_c_string_should_return_full_string_if_it_does_not_contain_nul() {
        let c_string = to_truncated_c_string("test without null");
        let rust_c_string = unsafe { CStr::from_ptr(c_string) };

        assert_eq!(
            CString::new("test without null").unwrap().to_bytes(),
            rust_c_string.to_bytes()
        );
    }

    #[test]
    fn to_truncated_c_string_should_truncate_string_to_first_nul() {
        let c_string = to_truncated_c_string("test\0with null");
        let rust_c_string = unsafe { CStr::from_ptr(c_string) };

        assert_eq!(
            CString::new("test").unwrap().to_bytes(),
            rust_c_string.to_bytes()
        );
    }
}

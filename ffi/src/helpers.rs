use std::ffi::{c_char, CStr, CString};

use esplugin::{GameId, Plugin};

use crate::{
    constants::{
        ESP_ERROR_INVALID_GAME_ID, ESP_ERROR_NOT_UTF8, ESP_ERROR_NULL_POINTER,
        ESP_ERROR_TEXT_ENCODE_ERROR, ESP_GAME_FALLOUT3, ESP_GAME_FALLOUT4, ESP_GAME_FALLOUTNV,
        ESP_GAME_MORROWIND, ESP_GAME_OBLIVION, ESP_GAME_SKYRIM, ESP_GAME_SKYRIMSE,
        ESP_GAME_STARFIELD,
    },
    error::error,
};

pub(crate) unsafe fn to_str<'a>(c_string: *const c_char) -> Result<&'a str, u32> {
    if c_string.is_null() {
        return Err(error(ESP_ERROR_NULL_POINTER, "Null pointer passed"));
    }

    let rust_c_string = CStr::from_ptr(c_string);

    rust_c_string
        .to_str()
        .map_err(|_e| error(ESP_ERROR_NOT_UTF8, "Non-UTF-8 string passed"))
}

pub(crate) fn to_c_string(string: &str) -> Result<*mut c_char, u32> {
    CString::new(string).map(CString::into_raw).map_err(|_e| {
        error(
            ESP_ERROR_TEXT_ENCODE_ERROR,
            "String could not be converted to a C string as it contained a null byte",
        )
    })
}

pub(crate) fn to_c_string_array(strings: &[String]) -> Result<(*mut *mut c_char, usize), u32> {
    let c_strings = strings
        .iter()
        .map(|s| to_c_string(s))
        .collect::<Result<Box<[*mut c_char]>, u32>>()?;

    let size = c_strings.len();

    // Although this is a pointer for the box and we want a pointer to the
    // start of the slice in the box, they're actually the same value, and we
    // can recover the box as well as the slice from the pointer and size.
    let pointer = Box::into_raw(c_strings);

    Ok((pointer.cast(), size))
}

pub(crate) unsafe fn to_plugin_refs_slice<'a>(
    array: *const *const Plugin,
    array_size: usize,
) -> Option<Box<[&'a Plugin]>> {
    std::slice::from_raw_parts(array, array_size)
        .iter()
        .map(|pointer| pointer.as_ref())
        .collect()
}

pub(crate) fn map_game_id(game_id: u32) -> Result<GameId, u32> {
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
            &format!("Invalid game ID: {game_id}"),
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
}

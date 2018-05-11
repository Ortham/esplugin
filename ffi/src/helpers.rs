use std::ffi::{CStr, CString};

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

pub fn to_c_string(string: &str) -> Result<*mut c_char, u32> {
    let c_string_name =
        CString::new(string.to_string()).map_err(|_| ESP_ERROR_STRING_CONTAINS_NUL)?;

    Ok(c_string_name.into_raw())
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

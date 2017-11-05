use esplugin::GameId;

use libc::uint32_t;

#[no_mangle]
pub static ESP_OK: uint32_t = 0;

#[no_mangle]
pub static ESP_ERROR_NULL_POINTER: uint32_t = 1;

#[no_mangle]
pub static ESP_ERROR_NOT_UTF8: uint32_t = 2;

#[no_mangle]
pub static ESP_ERROR_STRING_CONTAINS_NUL: uint32_t = 3;

#[no_mangle]
pub static ESP_ERROR_INVALID_GAME_ID: uint32_t = 4;

#[no_mangle]
pub static ESP_ERROR_PARSE_ERROR: uint32_t = 5;

#[no_mangle]
pub static ESP_ERROR_PANICKED: uint32_t = 6;

#[no_mangle]
pub static ESP_GAME_OBLIVION: uint32_t = GameId::Oblivion as uint32_t;

#[no_mangle]
pub static ESP_GAME_SKYRIM: uint32_t = GameId::Skyrim as uint32_t;

#[no_mangle]
pub static ESP_GAME_FALLOUT3: uint32_t = GameId::Fallout3 as uint32_t;

#[no_mangle]
pub static ESP_GAME_FALLOUTNV: uint32_t = GameId::FalloutNV as uint32_t;

#[no_mangle]
pub static ESP_GAME_MORROWIND: uint32_t = GameId::Morrowind as uint32_t;

#[no_mangle]
pub static ESP_GAME_FALLOUT4: uint32_t = GameId::Fallout4 as uint32_t;

#[no_mangle]
pub static ESP_GAME_SKYRIMSE: uint32_t = GameId::SkyrimSE as uint32_t;

use game_id::GameId;

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

#[no_mangle]
pub static ESPM_GAME_OBLIVION: u32 = GameId::Oblivion as u32;

#[no_mangle]
pub static ESPM_GAME_SKYRIM: u32 = GameId::Skyrim as u32;

#[no_mangle]
pub static ESPM_GAME_FALLOUT3: u32 = GameId::Fallout3 as u32;

#[no_mangle]
pub static ESPM_GAME_FALLOUTNV: u32 = GameId::FalloutNV as u32;

#[no_mangle]
pub static ESPM_GAME_MORROWIND: u32 = GameId::Morrowind as u32;

#[no_mangle]
pub static ESPM_GAME_FALLOUT4: u32 = GameId::Fallout4 as u32;

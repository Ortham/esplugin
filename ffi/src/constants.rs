#[unsafe(no_mangle)]
pub static ESP_OK: u32 = 0;

#[unsafe(no_mangle)]
pub static ESP_ERROR_NULL_POINTER: u32 = 1;

#[unsafe(no_mangle)]
pub static ESP_ERROR_NOT_UTF8: u32 = 2;

#[unsafe(no_mangle)]
pub static ESP_ERROR_STRING_CONTAINS_NUL: u32 = 3;

#[unsafe(no_mangle)]
pub static ESP_ERROR_INVALID_GAME_ID: u32 = 4;

#[unsafe(no_mangle)]
pub static ESP_ERROR_PARSE_ERROR: u32 = 5;

#[unsafe(no_mangle)]
pub static ESP_ERROR_PANICKED: u32 = 6;

#[unsafe(no_mangle)]
pub static ESP_ERROR_NO_FILENAME: u32 = 7;

#[unsafe(no_mangle)]
pub static ESP_ERROR_TEXT_DECODE_ERROR: u32 = 8;

#[unsafe(no_mangle)]
pub static ESP_ERROR_TEXT_ENCODE_ERROR: u32 = 9;

#[unsafe(no_mangle)]
pub static ESP_ERROR_IO_ERROR: u32 = 10;

#[unsafe(no_mangle)]
pub static ESP_ERROR_FILE_NOT_FOUND: u32 = 11;

#[unsafe(no_mangle)]
pub static ESP_ERROR_IO_PERMISSION_DENIED: u32 = 12;

#[unsafe(no_mangle)]
pub static ESP_ERROR_UNRESOLVED_RECORD_IDS: u32 = 13;

#[unsafe(no_mangle)]
pub static ESP_ERROR_PLUGIN_METADATA_NOT_FOUND: u32 = 14;

#[unsafe(no_mangle)]
pub static ESP_GAME_OBLIVION: u32 = 0;

#[unsafe(no_mangle)]
pub static ESP_GAME_SKYRIM: u32 = 1;

#[unsafe(no_mangle)]
pub static ESP_GAME_FALLOUT3: u32 = 2;

#[unsafe(no_mangle)]
pub static ESP_GAME_FALLOUTNV: u32 = 3;

#[unsafe(no_mangle)]
pub static ESP_GAME_MORROWIND: u32 = 4;

#[unsafe(no_mangle)]
pub static ESP_GAME_FALLOUT4: u32 = 5;

#[unsafe(no_mangle)]
pub static ESP_GAME_SKYRIMSE: u32 = 6;

#[unsafe(no_mangle)]
pub static ESP_GAME_STARFIELD: u32 = 7;

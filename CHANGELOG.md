# Changelog

As of v1.0.4, version numbers are shared between esplugin and esplugin-ffi.

## [1.0.10] - 2018-06-02

### Changed

- Updated to nom v4.0.0.

## [1.0.8] - 2018-02-03

### Changed

- Updated byteorder, memmap and flate2 dependencies.

## [1.0.7] - 2017-11-21

### Added

- `ESP_ERROR_PANICKED` return code for indicating that a panic was caught.

### Changed

- Unwinding panics are now caught at the FFI boundary.

### Fixed

- `ESP_ERROR_PARSE_ERROR` and `ESP_ERROR_INVALID_GAME_ID` had the same value.

## [1.0.5] - 2017-10-09

### Added

- Support for Skyrim SE to correctly recognise ESL (light master) plugins when calling `Plugin::is_light_master_file()`.
- `GameId::SkyrimSE` to distinguish between Skyrim and Skyrim SE plugins (the only difference in handling is ESL support).

### Changed

- Updated to nom v3.2.0.

## [1.0.4] - 2017-10-02

### Added

- `Plugin::parse_open_file()` for better performance when a file handle is already open.

## [1.0.1] - 2017-09-30

### Fixed

- Fix some invalid files being parsed successfully or failing inefficiently by validating the file header's type field as soon as possible.

## [1.0.0] - 2017-09-26

Initial release

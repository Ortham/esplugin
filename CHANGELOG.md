# Changelog

As of v1.0.4, version numbers are shared between esplugin and esplugin-ffi.

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

# Changelog

As of v1.0.4, version numbers are shared between esplugin and esplugin-ffi.

## [2.1.1] - 2018-10-22

### Fixed

- `Plugin::description()`, `Plugin::header_version()` and
  `Plugin::record_and_group_count()` could panic if the TES4 subrecords they
  read were smaller than expected. Instead, the former will now error, and the
  latter two will now return `None`.

## [2.1.0] - 2018-09-16

### Added

- `Plugin::header_version()` for getting the value of version field in the
  TES3/TES4 header record's `HEDR` subrecord.
- `Plugin::is_valid_as_light_master()` for checking if it is safe to convert the
  plugin to a light master. This is true for Skyrim SE and Fallout 4 plugins
  that only add records with FormIDs with object indices between 0x800 and 0xFFF
  inclusive, and false otherwise.

## [2.0.1] - 2018-09-10

## Changed

- Error on top-level records by failing group parsing if the group type is not
  `GRUP`. Top-level records would previously cause errors later in parsing, this
  change makes it more obvious where the issue lies.

## [2.0.0] - 2018-06-24

### Added

- `Plugin::overlaps_with()` for checking if two plugins edit the same records.

### Changed

- `Plugin::parse_file()` and `Plugin::parse_open_file()` now use memory mapping
  when fully parsing plugin files that are larger than 1 MB.
- Improved performance when parsing plugin headers and full plugin files by
  reducing allocations.

### Removed

- `FormId` and `Plugin::form_ids()`, there is now no public interface for
  representing or getting a plugin's FormIDs.
- `Plugin::parse_mmapped_file()`, use `Plugin::parse_file()` instead.

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

# Changelog

As of v1.0.4, version numbers are shared between esplugin and esplugin-ffi.

## [6.1.3] - 2025-04-28

### Changed

- Several cases where mismatches between buffer sizes and their expected sizes
  could cause panics now cause errors instead.
- If a malformed plugin has more than 256 masters, those past that limit will be
  ignored during parsing instead of causing a panic.
- Binary content in error messages (including paths) will now be output as ASCII
  strings with character escapes instead of hexadecimal byte arrays.
- Many code quality improvements.

## [6.1.2] - 2025-04-19

### Changed

- Updated encoding_rs to 0.8.35.
- Updated flate2 to 1.1.1.
- Updated nom to 8.0.0.
- Updated unicase to 2.8.1.

## [6.1.1] - 2024-10-07

### Changed

- Improved the performance of `Plugin::is_master_file()`,
  `Plugin::is_light_plugin()`, and `Plugin::is_medium_plugin()` when the file
  extension is checked.
- Updated flate2 to 1.0.34.

## [6.1.0] - 2024-07-23

### Added

- `Plugin::is_blueprint_plugin()` returns true if the plugin is a Starfield
  plugin that has the `0x800` flag set in its TES4 header record.

## [6.0.1] - 2024-07-22

### Fixed

- `Plugin::masters()` and `Plugin::description()` now truncate the strings that
  they return up to (and not including) their first null byte, to match the
  behaviour of supported games and their official modding tools.

## [6.0.0] - 2024-06-27

### Added

- Support for the new medium plugin type introduced by Starfield 1.12.30.0.
- Support for comparing FormIDs between Starfield plugins.
- Support for counting the number of override records in Morrowind and Starfield
  plugins.
- `Plugin::is_medium_plugin()`, which checks if the given plugin is a medium
  plugin.
- `Plugin::is_valid_as_medium_plugin()`, which checks if the given plugin's
  FormIDs fall within the valid range for a medium plugin.
- `Error::UnresolvedFormIds` as a new error variant.
- `Error::PluginMetadataNotFound` as a new error variant.
- `plugins_metadata()`, which outputs an opaque struct that holds
  metadata for all the given plugins that can be passed to
  `Plugin::resolve_record_ids()`.
- `PluginMetadata` as an opaque struct that holds metadata about a plugin.
- `Plugin::resolve_record_ids()`, which is used to resolve Morrowind and
  Starfield records using the metadata of their masters, which is provided by
  the output of `plugins_metadata()`.
- `Plugin::parse_reader()` as a more generic replacement for `Plugin::parse()`
  and `Plugin::parse_open_file()`.
- `ParseOptions` as an opaque struct holding configuration options for parsing a
  plugin.

### Changed

- `Plugin::count_override_records()` now returns a `Result<usize, Error>`. It
  will error if called on a Morrowind or Starfield plugin that has not had
  `Plugin::resolve_record_ids()` run on it.
- `Plugin::overlaps_with()` now returns a `Result<bool, Error>`. It will error
  if called on a Starfield plugin that has not had
  `Plugin::resolve_record_ids()` run on it.
- `Plugin::overlap_size()` now returns a `Result<usize, Error>`. It will error
  if called on a Starfield plugin that has not had
  `Plugin::resolve_record_ids()` run on it.
- `Plugin::is_valid_as_light_plugin()` now returns a `Result<bool, Error>`. It
  will error if called on a Starfield plugin that has not had
  `Plugin::resolve_record_ids()` run on it.
- `Plugin::is_valid_as_override_plugin()` now returns a `Result<bool, Error>`.
  It will error if called on a Starfield plugin that has not had
  `Plugin::resolve_record_ids()` run on it.
- `Plugin::game_id()` now returns a `GameId` instead of a `&GameId`.
- `Plugin::is_override_plugin()` has been renamed to
  `Plugin::is_update_plugin()` to reflect the terminology used by Starfield's
  Creation Kit.
- `Plugin::is_valid_as_override_plugin()` has been renamed to
  `Plugin::is_valid_as_update_plugin()` to reflect the terminology used by
  Starfield's Creation Kit.
- `Plugin::parse_file()` and `Plugin::is_valid()` now take a `ParseOptions`
  parameter object instead of a boolean indicating whether or not to only load
  the plugin's header.
- `Error::ParsingError`'s first field is now a `Box<[u8]>` instead of a
  `Vec<u8>`.
- `Error::DecodeError`'s first field is now a `Box<[u8]>` instead of a
  `Vec<u8>`.

### Fixed

- `Plugin::is_master_file()` incorrectly used the file extension instead of the
header flag for Morrowind plugins.

### Removed

- `Plugin::is_light_master()`: use `Plugin::is_light_plugin()` instead.
- `Plugin::is_valid_as_light_master()`: use `Plugin::is_valid_as_light_plugin()`
  instead.
- `Plugin::parse()`: use `Plugin::parse_reader()` with a `std::io::Cursor`
  instead.
- `Plugin::parse_open_file()`: use `Plugin::parse_reader()` instead.

## [5.0.1] - 2024-05-02

### Changed

- Updated to Rust's 2021 edition.

## [5.0.0] - 2024-05-02

### Added

- `Cargo.lock` is no longer ignored by Git.

### Changed

- `Error::NoFilename`, `Error::ParsingIncomplete` and `Error::DecodeError` now hold contextual data.

## [4.1.1] - 2022-09-15

### Added

- Support for the expanded FormID range introduced in Skyrim Special Edition
  v1.6.1130.0 for light plugins with a `HEDR` version of 1.71.

### Fixed

- `Plugin::is_valid_as_light_plugin()` now takes into account the plugin's
  `HEDR` version when determining the valid FormID range. This means that
  Fallout 4 light plugins with a `HEDR` version below `1.0` now use the
  correct range of object indexes, starting at `0x800`.

## [4.1.0] - 2023-09-05

### Added

- Support for parsing Starfield plugins.
- `GameId::supports_light_plugins()`, which returns true for Skyrim SE,
  Fallout 4 and Starfield.
- `Plugin::is_override_plugin()`, which checks if a Starfield plugin is
  loaded as an override plugin.
- `Plugin::is_valid_as_override_plugin()`, which checks if a Starfield plugin
  contains no new records.

## [4.0.0] - 2022-09-15

### Changed

- Internal code quality improvements.

## [3.5.1] - 2022-03-27

### Changed

- Avoid unnecessary path copy when checking if a plugin is valid.
- Updated to nom v7.0.0.

## [3.4.0] - 2021-04-17

### Added

- `Plugin::is_light_plugin()` is the preferred alias of
  `Plugin::is_light_master_file()` as it reflects that not all plugins with the
  light flag set are masters.
- `Plugin::is_valid_as_light_plugin()` is the preferred alias of
  `Plugin::is_valid_as_light_master()`.

### Changed

- Updated to nom v6.0.0.

### Deprecated

- `Plugin::is_light_master_file()`: use the alias `Plugin::is_light_plugin()`
  instead.
- `Plugin::is_valid_as_light_master()`: use the alias
  `Plugin::is_valid_as_light_plugin()` instead.

## [3.3.1] - 2020-04-03

### Changed

- Removed the `std::error::Error::description()` implementation on `Error`, to
  avoid CMake build errors due to the deprecation warning for `description()`.

## [3.3.0] - 2019-12-01

### Changed

- The range of FormIDs that are recognised as valid in light masters has been
  extended for Fallout 4 plugins, from between `0x800` and `0xFFF` inclusive to
  between `0x001` and `0xFFF` inclusive, to reflect the extended range supported
  by Fallout 4 v1.10.162.0.0. The valid range for Skyrim Special Edition plugins
  is unchanged.

## [3.2.0] - 2019-09-06

### Added

- `Plugin::overlap_size()` for counting how many of the plugin's records are
  also present in the given plugins.

### Fixed

- `Plugin::overlaps_with()` could fail to detect an overlap between Morrowind
  plugins.

## [3.0.0] - 2019-07-21

### Changed

- The `Error::DecodeError` variant no longer has any fields.
- The `Error::ParsingError` variant is now
  `Error::ParsingError(Vec<u8>, ParsingErrorKind)` to provide more detail about
  why parsing failed.
- Replaced the encoding dependency with encoding_rs, as the former is
  unmaintained.
- Updated to nom v5.0.0.

### Removed

- The byteorder dependency, as Rust standard library additions have
  made it unnecessary.
- The unicase dependency as Unicode-aware case-insensitive string
  comparison was not strictly required.
- The memmap dependency. Its use was unsafe, but this was not exposed
  correctly. The performance gained from reading memory-mapped files was
  outweighed by the negative impact on usability that exposing the unsafety
  correctly would have. There is now no use of `unsafe` in esplugin itself.

## [2.1.2] - 2019-04-24

### Fixed

- `Plugin::overlaps_with()` now detects overlaps between Morrowind plugins. It
  would previously always return `false` for all pairs of Morrowind plugins
  because it was checking for record FormIDs present in both plugins, but
  Morrowind records don't have FormIDs. Instead, record IDs are calculated from
  record data and used for the same purpose.
- `Plugin::count_override_records()` now always returns `0` for Morrowind
  plugins, as it's impossible to tell which records a plugin contains are new
  and which are overrides without comparing against its masters. The function
  would previously return the total number of records in the plugin.

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

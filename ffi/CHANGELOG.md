# Changelog

After v1.0.3, version numbers are shared between esplugin and esplugin-ffi.

## [6.0.0] - 2024-06-27

### Added

- `esp_get_error_message()`, which outputs a thread-local error message string
  for the last error that occurred.
- `esp_plugin_is_medium_plugin()`, which checks if the given plugin is a medium
  plugin (as introduced by Starfield 1.12.30.0).
- `esp_plugin_is_valid_as_medium_plugin()`, which checks if the given plugin's
  FormIDs fall within the valid range for a medium plugin.
- `esp_get_plugins_metadata()`, which outputs an opaque struct that holds
  metadata for all the given plugins that can be passed to
  `esp_plugin_resolve_record_ids()`.
- `esp_plugins_metadata_free()`, which frees objects allocated by
  `esp_get_plugins_metadata()`.
- `esp_plugin_resolve_record_ids()`, which is used to resolve Morrowind and
  Starfield records using the metadata of their masters, which is provided by
  the output of `esp_get_plugins_metadata()`.
- `ESP_ERROR_UNRESOLVED_RECORD_IDS` as a new error code.
- `ESP_ERROR_PLUGIN_METADATA_NOT_FOUND` as a new error code.

### Changed

- Strings that contain null bytes now cause errors instead of being silently
  truncated when read from:
  - Plugin filenames
  - Plugin masters
  - Plugin descriptions (excluding Morrowind plugins, which still have their
    descriptions truncated to the first null byte)
- `esp_plugin_is_override_plugin()` has been renamed to
  `esp_plugin_is_update_plugin()` to reflect the terminology used by Starfield's
  Creation Kit.
- `esp_plugin_is_valid_as_override_plugin()` has been renamed to
  `esp_plugin_is_valid_as_update_plugin()` to reflect the terminology used by
  Starfield's Creation Kit.
- Updated to esplugin v6.0.0.

### Removed

- `esp_plugin_is_light_master()`: use `esp_plugin_is_light_plugin()` instead.
- `esp_plugin_is_valid_as_light_master()`: use
  `esp_plugin_is_valid_as_light_plugin()` instead.

## [5.0.1] - 2024-05-02

### Changed

- Updated to esplugin v5.0.1.
- Updated to Rust's 2021 edition.

## [5.0.0] - 2024-05-02

### Changed

- Updated to esplugin v5.0.0.

### Removed

- The `ffi-headers` build feature: if you want to generate C or C++ headers,
  install and run cbindgen separately.

## [4.1.1] - 2023-12-06

### Changed

- Updated to esplugin v4.1.1.
- Updated to cbindgen v0.26.

## [4.1.0] - 2023-09-05

### Added

- `ESP_GAME_STARFIELD` as the game code to use with Starfield plugins.
- `esp_plugin_is_override_plugin()`, which checks if a Starfield plugin is
  loaded as an override plugin.
- `esp_plugin_is_valid_as_override_plugin()`, which checks if a Starfield plugin
  contains no new records.

### Changed

- Updated to esplugin v4.1.0.

## [4.0.0] - 2022-09-15

### Added

- Some public functions now have documentation comments.

### Changed

- `esp_plugin_masters()` now takes a pointer to a `size_t` instead of a pointer
  to a `u8` as its array size output parameter. This means that the function
  will correctly handle invalid plugins with more than 255 masters.

## [3.5.1] - 2022-03-27

### Changed

- esplugin-ffi now uses Rust's 2018 edition for consistency with esplugin.
- Updated to esplugin v3.5.1.
- Updated to cbindgen v0.20.

## [3.5.0] - 2021-05-04

### Added

- `esp_plugin_is_light_plugin()` is the preferred alias of
  `esp_plugin_is_light_master()` as it reflects that not all plugins with the
  light flag set are masters.

### Deprecated

- `esp_plugin_is_light_master()`: use the alias `esp_plugin_is_light_plugin()`
  instead.

## [3.4.0] - 2021-04-17

### Added

- `esp_plugin_is_valid_as_light_plugin()` is the preferred alias of
  `esp_plugin_is_valid_as_light_master()` as it reflects that not all plugins with the
  light flag set are masters.

### Changed

- Updated to esplugin v3.4.0.
- Updated to cbindgen v0.19.

### Deprecated

- `esp_plugin_is_valid_as_light_master()`: use the alias
  `esp_plugin_is_valid_as_light_plugin()` instead.

## [3.3.1] - 2020-04-03

### Changed

- Updated to cbindgen v0.13.
- Updated to esplugin v3.3.1.

## [3.3.0] - 2019-12-01

### Changed

- Updated to esplugin v3.3.0.

## [3.2.0] - 2019-09-06

### Added

- `esp_plugin_records_overlap_size()`, which wraps `Plugin::overlap_size()`,
  for counting how many of the plugin's records are also present in the given
  plugins.

### Changed

- Updated to esplugin v3.2.0.

## [3.1.0] - 2019-08-30

### Added

- `esp_plugin_record_and_group_count()`, which wraps
  `esplugin::Plugin::record_and_group_count()`.

## [3.0.0] - 2019-07-21

### Changed

- `u8` and `u32` are now used in place of the deprecated `libc::uint8_t` and
  `libc::uint32_t` types. The latter were aliases of the former, so this should
  have no impact on usage.
- Updated to cbindgen v0.9.
- Updated to esplugin v2.2.0.

### Removed

- The included cbindgen config and `ffi-headers` feature no longer generate an
  `esplugin.hpp`. Instead, the `esplugin.h` header can now be used by C and
  C++ projects.

## [2.1.2] - 2019-04-24

### Changed

- Updated to esplugin v2.1.2.

## [2.1.1] - 2018-10-22

### Changed

- It is now possible to generate a C++ header file with the cbindgen CLI using
  the included `cbindgen.toml`.
- Updated to esplugin v2.1.1.

## [2.1.0] - 2018-09-16

### Added

- `esp_plugin_header_version()`, which wraps
  `esplugin::Plugin::header_version()`. If no header version is present, the
  output is `NaN`.
- `esp_plugin_is_valid_as_light_master()`, which wraps
  `esplugin::Plugin::is_valid_as_light_master()`.

### Changed

- Updated to esplugin v2.1.0.

## [2.0.1] - 2018-09-10

### Changed

- Updated to esplugin v2.0.1.

## [2.0.0] - 2018-06-24

### Changed

- `esp_plugin_filename`, `esp_plugin_masters` and `esp_plugin_description` will
  now truncate string output when the string contains a null byte, instead of
  erroring.
- Updated to esplugin v2.0.0.

## [1.0.10] - 2018-06-02

### Changed

- Updated cbindgen dependency to v0.6.0.

## [1.0.9] - 2018-03-13

### Changed

- Updated cbindgen dependency to fix build error when generating C/C++ headers.

## [1.0.8] - 2018-02-03

### Changed

- Updated esplugin and cbindgen dependencies.

## [1.0.7] - 2017-11-21

### Changed

- `Plugin::is_master_file()` now also returns true for Fallout 4 and Skyrim SE plugins that have `.esm` or `.esl` file extensions, regardless of their master flag value, to match game behaviour.
- `Plugin::is_light_master_file()` now also returns true for Fallout 4 and Skyrim SE plugins that have the light master flag set, regardless of their file extension, to match game behaviour.

## [1.0.6] - 2017-10-19

### Fixed

- Redefinition of constants in the C/C++ header files generated by cbindgen v0.1.27.
- Unused variable warnings.

## [1.0.5] - 2017-10-09

### Added

- `ESP_GAME_SKYRIMSE` to distinguish between Skyrim and Skyrim SE plugins, as Skyrim SE now supports light master plugins.

### Changed

- Updated to esplugin v1.0.5.

## [1.0.3] - 2017-09-30

### Changed

- Updated to esplugin v1.0.1.

## [1.0.2] - 2017-09-26

### Fixed

- When publishing to crates.io, the build, include and target directories were included and uploaded unnecessarily.

## [1.0.1] - 2017-09-26 [YANKED]

### Fixed

- The FFI constants were not renamed when the library was renamed from libespm to esplugin. They now start with `ESP_`, not `ESPM_`, to match function names.

## [1.0.0] - 2017-09-26 [YANKED]

Initial release

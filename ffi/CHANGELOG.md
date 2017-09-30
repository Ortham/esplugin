# Changelog

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

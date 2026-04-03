esplugin
=======

![CI](https://github.com/Ortham/esplugin/actions/workflows/ci.yml/badge.svg?branch=master&event=push)
[![Coverage Status](https://coveralls.io/repos/github/Ortham/esplugin/badge.svg?branch=master)](https://coveralls.io/github/Ortham/esplugin?branch=master)
[![docs](https://docs.rs/esplugin/badge.svg)](https://docs.rs/crate/esplugin)

A free software library for reading Elder Scrolls Plugin (`.esp`), Elder Scrolls Master (`.esm`) and Elder Scrolls Light (`.esl`) files. It can currently parse plugins for the following games:

* TES III: Morrowind
* TES IV: Oblivion (including Remastered)
* TES V: Skyrim (including Special Edition, Anniversary Edition and VR)
* Fallout 3
* Fallout: New Vegas
* Fallout 4 (including VR)
* Starfield

esplugin is a rewrite of [libespm](https://github.com/Ortham/libespm) and focuses on providing a useful API to [libloadorder](https://github.com/Ortham/libloadorder) and [LOOT](https://github.com/loot/loot), rather than a general-purpose plugin parser.

## Minimum supported Rust version

The minimum supported Rust version is `1.81`. This may change at any time, but there is a CI job to check that it does not change unexpectedly.

esplugin
=======

![CI](https://github.com/Ortham/esplugin/workflows/CI/badge.svg?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/Ortham/esplugin/badge.svg?branch=master)](https://coveralls.io/github/Ortham/esplugin?branch=master)
[![docs](https://docs.rs/esplugin/badge.svg)](https://docs.rs/crate/esplugin)

A free software library for reading Elder Scrolls Plugin (`.esp`), Elder Scrolls Master (`.esm`) and Elder Scrolls Light Master (`.esl`) files. It can currently parse plugins for the following games:

* TES III: Morrowind
* TES IV: Oblivion
* TES V: Skyrim (including Special Edition)
* Fallout 3
* Fallout: New Vegas
* Fallout 4

esplugin is a rewrite of [libespm](https://github.com/Ortham/libespm) and focuses on providing a useful API to [libloadorder](https://github.com/Ortham/libloadorder) and [LOOT](https://github.com/loot/loot), rather than a general-purpose plugin parser.

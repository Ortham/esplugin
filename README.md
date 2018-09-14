esplugin
=======

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/WrinklyNinja/esplugin?branch=master&svg=true)](https://ci.appveyor.com/project/WrinklyNinja/esplugin)
[![Travis Build Status](https://travis-ci.org/WrinklyNinja/esplugin.svg?branch=master)](https://travis-ci.org/WrinklyNinja/esplugin)
[![dependency status](https://deps.rs/repo/github/WrinklyNinja/esplugin/status.svg)](https://deps.rs/repo/github/WrinklyNinja/esplugin)
[![docs](https://docs.rs/esplugin/badge.svg)](https://docs.rs/crate/esplugin)

A free software library for reading Elder Scrolls Plugin (`.esp`), Elder Scrolls Master (`.esm`) and Elder Scrolls Light Master (`.esl`) files. It can currently parse plugins for the following games:

* TES III: Morrowind
* TES IV: Oblivion
* TES V: Skyrim (including Special Edition)
* Fallout 3
* Fallout: New Vegas
* Fallout 4

esplugin is a rewrite of [libespm](https://github.com/WrinklyNinja/libespm) and focuses on providing a useful API to [libloadorder](https://github.com/WrinklyNinja/libloadorder) and [LOOT](https://github.com/loot/loot), rather than a general-purpose plugin parser.

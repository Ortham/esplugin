esplugin
=======

[![Build Status](https://travis-ci.org/WrinklyNinja/esplugin.svg?branch=rust-rewrite)](https://travis-ci.org/WrinklyNinja/esplugin)
[![Coveralls branch](https://img.shields.io/coveralls/WrinklyNinja/esplugin/rust-rewrite.svg)](https://coveralls.io/github/WrinklyNinja/esplugin)

A free software library for reading Elder Scrolls Plugin (`.esp`), Elder Scrolls Master (`.esm`) and Elder Scrolls Light Master (`.esl`) files. It can currently parse plugins for the following games:

* TES III: Morrowind
* TES IV: Oblivion
* TES V: Skyrim (including Special Edition)
* Fallout 3
* Fallout: New Vegas
* Fallout 4

esplugin is a rewrite of [libespm](https://github.com/WrinklyNinja/libespm) and focuses on providing a useful API to [libloadorder](https://github.com/WrinklyNinja/libloadorder) and [LOOT](https://github.com/loot/loot), rather than a general-purpose plugin parser.

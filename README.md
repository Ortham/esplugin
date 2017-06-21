Libespm
=======

[![Build Status](https://travis-ci.org/WrinklyNinja/libespm.svg?branch=rust-rewrite)](https://travis-ci.org/WrinklyNinja/libespm)
[![Coveralls branch](https://img.shields.io/coveralls/WrinklyNinja/libespm/rust-rewrite.svg)](https://coveralls.io/github/WrinklyNinja/libespm)

A free software library for reading Elder Scrolls Plugin (`.esp`) and Elder Scrolls Master (`.esm`) files. It can currently parse plugins for the following games:

* TES III: Morrowind
* TES IV: Oblivion
* TES V: Skyrim
* Fallout 3
* Fallout: New Vegas
* Fallout 4

Libespm focuses on providing a useful API to [libloadorder](https://github.com/WrinklyNinja/libloadorder) and [LOOT](https://github.com/loot/loot), rather than a general-purpose plugin parser.

**This branch holds a rewrite of libespm in Rust, done as a learning exercise, so it's a bit rough.**

Libespm
=======

[![Build Status](https://travis-ci.org/WrinklyNinja/libespm.svg?branch=rust-rewrite)](https://travis-ci.org/WrinklyNinja/libespm)
[![Coveralls branch](https://img.shields.io/coveralls/WrinklyNinja/libespm/rust-rewrite.svg)](https://coveralls.io/github/WrinklyNinja/libespm)

A free software library for reading Elder Scrolls Plugin (`.esp`), Elder Scrolls Master (`.esm`) and Elder Scrolls Light Master (`.esl`) files. It can currently parse plugins for the following games:

* TES III: Morrowind
* TES IV: Oblivion
* TES V: Skyrim (including Special Edition)
* Fallout 3
* Fallout: New Vegas
* Fallout 4

Libespm focuses on providing a useful API to [libloadorder](https://github.com/WrinklyNinja/libloadorder) and [LOOT](https://github.com/loot/loot), rather than a general-purpose plugin parser.

Libespm v1 and v2 were header-only C++ libraries with Boost dependencies.
Libespm v3 is a Rust rewrite that produces a Rust library and a shared library
with a C FFI.

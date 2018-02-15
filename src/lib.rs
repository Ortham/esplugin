/*
 * This file is part of esplugin
 *
 * Copyright (C) 2017 Oliver Hamlet
 *
 * esplugin is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * esplugin is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with esplugin. If not, see <http://www.gnu.org/licenses/>.
 */
#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

extern crate byteorder;
extern crate encoding;
extern crate memmap;
#[macro_use]
extern crate nom;
extern crate unicase;

#[cfg(feature = "compressed-fields")]
extern crate flate2;

pub use error::Error;
pub use form_id::*;
pub use game_id::*;
pub use plugin::*;

mod error;
mod form_id;
mod game_id;
mod group;
mod plugin;
mod record;
mod subrecord;

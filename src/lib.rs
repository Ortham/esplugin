/*
 * This file is part of libespm
 *
 * Copyright (C) 2017 Oliver Hamlet
 *
 * libespm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libespm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libespm. If not, see <http://www.gnu.org/licenses/>.
 */

extern crate byteorder;
extern crate encoding;
extern crate flate2;
extern crate libc;
extern crate memmap;
#[macro_use]
extern crate nom;

pub use form_id::*;
pub use game_id::*;
pub use plugin::*;
pub use ffi::*;

mod ffi;
mod form_id;
mod game_id;
mod group;
mod plugin;
mod record;
mod subrecord;

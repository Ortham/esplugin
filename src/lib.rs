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
use std::convert::{TryFrom, TryInto};

pub use crate::error::{Error, MoreDataNeeded, ParsingErrorKind};
pub use crate::game_id::GameId;
pub use crate::plugin::{plugins_metadata, ParseOptions, Plugin, PluginMetadata};

mod error;
mod game_id;
mod group;
mod plugin;
mod record;
mod record_id;
mod subrecord;

fn le_slice_to_u32(input: &[u8]) -> u32 {
    let int_bytes = &input[..std::mem::size_of::<u32>()];
    u32::from_le_bytes(
        int_bytes
            .try_into()
            .expect("slice to contain enough bytes to read a u32"),
    )
}

fn le_slice_to_f32(input: &[u8]) -> f32 {
    f32::from_bits(le_slice_to_u32(input))
}

fn u32_to_usize(input: u32) -> usize {
    usize::try_from(input).expect("u32 to fit in usize")
}

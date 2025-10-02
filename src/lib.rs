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
// Allow a few clippy pedantic lints.
#![allow(
    clippy::doc_markdown,
    clippy::exhaustive_enums,
    clippy::must_use_candidate,
    clippy::missing_errors_doc
)]
#![cfg_attr(
    test,
    allow(
        clippy::assertions_on_result_states,
        clippy::indexing_slicing,
        clippy::missing_asserts_for_indexing,
        clippy::panic,
        clippy::unwrap_used,
    )
)]

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

// No point recording any details of the error because it's not reported.
struct SliceTooSmallError;

fn le_slice_to_u32(input: &[u8]) -> Result<u32, SliceTooSmallError> {
    const ARRAY_SIZE: usize = std::mem::size_of::<u32>();

    subarray::<ARRAY_SIZE>(input, 0).map(u32::from_le_bytes)
}

fn le_slice_to_f32(input: &[u8]) -> Result<f32, SliceTooSmallError> {
    const ARRAY_SIZE: usize = std::mem::size_of::<f32>();

    subarray::<ARRAY_SIZE>(input, 0).map(f32::from_le_bytes)
}

fn subarray<const SIZE: usize>(
    bytes: &[u8],
    start_index: usize,
) -> Result<[u8; SIZE], SliceTooSmallError> {
    let stop_index = start_index + SIZE;

    let bytes = bytes
        .get(start_index..stop_index)
        .ok_or(SliceTooSmallError)?;

    <[u8; SIZE]>::try_from(bytes).map_err(|_e| SliceTooSmallError)
}

#[expect(
    clippy::as_conversions,
    reason = "A compile-time assertion ensures that this conversion will be lossless on all relevant target platforms"
)]
const fn u32_to_usize(input: u32) -> usize {
    // Error at compile time if this conversion isn't lossless.
    const _: () = assert!(u32::BITS <= usize::BITS, "cannot fit a u32 into a usize!");
    input as usize
}

#[cfg(test)]
fn read_test_data(
    path: impl AsRef<std::path::Path>,
    byte_range: impl std::ops::RangeBounds<usize>,
) -> Vec<u8> {
    let path = std::path::Path::new("testing-plugins").join(path);

    std::fs::read(path).unwrap().drain(byte_range).collect()
}

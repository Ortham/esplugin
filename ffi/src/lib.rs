// Deny some rustc lints that are allow-by-default.
#![deny(
    ambiguous_negative_literals,
    impl_trait_overcaptures,
    let_underscore_drop,
    missing_copy_implementations,
    missing_debug_implementations,
    non_ascii_idents,
    redundant_imports,
    redundant_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unit_bindings,
    unreachable_pub
)]
#![deny(clippy::pedantic)]
// Allow a few clippy pedantic lints.
#![allow(clippy::missing_safety_doc)]
// Selectively deny clippy restriction lints.
#![deny(
    clippy::allow_attributes,
    clippy::as_conversions,
    clippy::as_underscore,
    clippy::assertions_on_result_states,
    clippy::big_endian_bytes,
    clippy::cfg_not_test,
    clippy::clone_on_ref_ptr,
    clippy::create_dir,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::default_numeric_fallback,
    clippy::doc_include_without_cfg,
    clippy::empty_drop,
    clippy::error_impl_error,
    clippy::exit,
    clippy::exhaustive_enums,
    clippy::expect_used,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::fn_to_numeric_cast_any,
    clippy::get_unwrap,
    clippy::host_endian_bytes,
    clippy::if_then_some_else_none,
    clippy::indexing_slicing,
    clippy::infinite_loop,
    clippy::integer_division,
    clippy::integer_division_remainder_used,
    clippy::iter_over_hash_type,
    clippy::let_underscore_must_use,
    clippy::lossy_float_literal,
    clippy::map_err_ignore,
    clippy::map_with_unused_argument_over_ranges,
    clippy::mem_forget,
    clippy::missing_assert_message,
    clippy::missing_asserts_for_indexing,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::mutex_atomic,
    clippy::mutex_integer,
    clippy::needless_raw_strings,
    clippy::non_ascii_literal,
    clippy::non_zero_suggestions,
    clippy::panic,
    clippy::panic_in_result_fn,
    clippy::partial_pub_fields,
    clippy::pathbuf_init_then_push,
    clippy::precedence_bits,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::redundant_type_annotations,
    clippy::ref_patterns,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::str_to_string,
    clippy::string_lit_chars_any,
    clippy::string_slice,
    clippy::string_to_string,
    clippy::suspicious_xor_used_as_pow,
    clippy::tests_outside_test_module,
    clippy::todo,
    clippy::try_err,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unnecessary_safety_comment,
    clippy::unneeded_field_pattern,
    clippy::unreachable,
    clippy::unused_result_ok,
    clippy::unwrap_in_result,
    clippy::unwrap_used,
    clippy::use_debug,
    clippy::verbose_file_reads
)]
#![cfg_attr(test, allow(clippy::undocumented_unsafe_blocks, clippy::unwrap_used,))]

//! ## Thread Safety
//!
//! esplugin's functions are not thread-safe. However, there is no hidden shared
//! state, so it is safe to concurrently call functions so long as none of their
//! mutable arguments are shared between concurrent calls. For example:
//!
//! * It's safe to call [`esp_plugin_filename`] and [`esp_plugin_description`]
//!   concurrently so long as their output string arguments use different
//!   buffers.
//! * It's safe to call [`esp_plugin_new`] concurrently so long as each call
//!   passes a different plugin handle pointer to write to.
//! * It's __not__ safe to call [`esp_plugin_parse`] and [`esp_plugin_filename`]
//!   concurrently with the same plugin handle.
//!
use std::panic;

use self::error::error;
use error::handle_error;
use esplugin::Plugin;
use esplugin::PluginMetadata;
use helpers::to_plugin_refs_slice;
use libc::size_t;

pub use self::common::*;
pub use self::constants::*;
pub use self::plugin::*;

mod common;
mod constants;
mod error;
mod helpers;
mod plugin;

#[no_mangle]
pub unsafe extern "C" fn esp_get_plugins_metadata(
    plugins: *const *const Plugin,
    plugins_len: size_t,
    plugins_metadata: *mut *mut Vec<PluginMetadata>,
) -> u32 {
    panic::catch_unwind(|| {
        if plugins.is_null() || plugins_metadata.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            let Some(plugins) = to_plugin_refs_slice(plugins, plugins_len) else {
                return error(
                    ESP_ERROR_NULL_POINTER,
                    "Null pointer passed in plugins array",
                );
            };

            match esplugin::plugins_metadata(&plugins) {
                Ok(m) => {
                    *plugins_metadata = Box::into_raw(Box::new(m));
                    ESP_OK
                }
                Err(e) => handle_error(&e),
            }
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

#[no_mangle]
pub unsafe extern "C" fn esp_plugins_metadata_free(plugins_metadata: *mut Vec<PluginMetadata>) {
    if !plugins_metadata.is_null() {
        drop(Box::from_raw(plugins_metadata));
    }
}

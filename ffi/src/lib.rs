//! ## Thread Safety
//!
//! esplugin's functions are not thread-safe. However, there is no hidden shared
//! state, so it is safe to concurrently call functions so long as none of their
//! mutable arguments are shared between concurrent calls. For example:
//!
//! * It's safe to call [esp_plugin_filename] and [esp_plugin_description]
//!   concurrently so long as their output string arguments use different
//!   buffers.
//! * It's safe to call [esp_plugin_new] concurrently so long as each call
//!   passes a different plugin handle pointer to write to.
//! * It's __not__ safe to call [esp_plugin_parse] and [esp_plugin_filename]
//!   concurrently with the same plugin handle.
//!
use std::panic;

use self::error::error;
use error::handle_error;
use esplugin::Plugin;
use esplugin::PluginMetadata;
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
            let plugins: Option<Vec<&Plugin>> = std::slice::from_raw_parts(plugins, plugins_len)
                .iter()
                .map(|pointer| pointer.as_ref())
                .collect();

            let plugins = match plugins {
                Some(p) => p,
                None => {
                    return error(
                        ESP_ERROR_NULL_POINTER,
                        "Null pointer passed in plugins array",
                    )
                }
            };

            match esplugin::plugins_metadata(&plugins) {
                Ok(m) => {
                    *plugins_metadata = Box::into_raw(Box::new(m));
                    ESP_OK
                }
                Err(e) => handle_error(e),
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

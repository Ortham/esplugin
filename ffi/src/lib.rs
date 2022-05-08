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
pub use self::common::*;
pub use self::constants::*;
pub use self::plugin::*;

mod common;
mod constants;
mod helpers;
mod plugin;

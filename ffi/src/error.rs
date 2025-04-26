use std::cell::RefCell;
use std::ffi::{c_char, c_uint, CString};
use std::panic::catch_unwind;

use esplugin::Error;

use crate::constants::{
    ESP_ERROR_FILE_NOT_FOUND, ESP_ERROR_IO_ERROR, ESP_ERROR_IO_PERMISSION_DENIED,
    ESP_ERROR_NO_FILENAME, ESP_ERROR_NULL_POINTER, ESP_ERROR_PANICKED, ESP_ERROR_PARSE_ERROR,
    ESP_ERROR_PLUGIN_METADATA_NOT_FOUND, ESP_ERROR_TEXT_DECODE_ERROR,
    ESP_ERROR_UNRESOLVED_RECORD_IDS, ESP_OK,
};

thread_local!(static ERROR_MESSAGE: RefCell<CString> = RefCell::new(CString::default()));

/// Get the message for the last error encountered.
///
/// Outputs a string giving a message containing the details of the last error or warning
/// encountered by a function. The message uses thread-local storage, and only one message is
/// stored at any one time.
///
/// Returns `ESP_OK` if successful, otherwise a `ESP_ERROR_*` code is returned.
#[no_mangle]
pub(crate) unsafe extern "C" fn esp_get_error_message(message: *mut *const c_char) -> c_uint {
    catch_unwind(|| {
        if message.is_null() {
            error(ESP_ERROR_NULL_POINTER, "Null pointer passed")
        } else {
            ERROR_MESSAGE.with(|f| {
                if f.borrow().as_bytes().is_empty() {
                    *message = std::ptr::null();
                } else {
                    *message = f.borrow().as_ptr();
                }
            });

            ESP_OK
        }
    })
    .unwrap_or(ESP_ERROR_PANICKED)
}

pub(crate) fn error(code: c_uint, message: &str) -> c_uint {
    ERROR_MESSAGE.with(|f| {
        *f.borrow_mut() =
            CString::new(message.as_bytes()).unwrap_or(c"Failed to retrieve error message".into());
    });
    code
}

pub(crate) fn handle_error(err: &Error) -> c_uint {
    let code = map_error(err);
    error(code, &format!("{err}"))
}

fn map_io_error(err: &std::io::Error) -> c_uint {
    match err.kind() {
        std::io::ErrorKind::NotFound => ESP_ERROR_FILE_NOT_FOUND,
        std::io::ErrorKind::PermissionDenied => ESP_ERROR_IO_PERMISSION_DENIED,
        _ => ESP_ERROR_IO_ERROR,
    }
}

fn map_error(err: &Error) -> c_uint {
    match err {
        Error::IoError(x) => map_io_error(x),
        Error::NoFilename(_) => ESP_ERROR_NO_FILENAME,
        Error::ParsingIncomplete(_) | Error::ParsingError(_, _) => ESP_ERROR_PARSE_ERROR,
        Error::DecodeError(_) => ESP_ERROR_TEXT_DECODE_ERROR,
        Error::UnresolvedRecordIds(_) => ESP_ERROR_UNRESOLVED_RECORD_IDS,
        Error::PluginMetadataNotFound(_) => ESP_ERROR_PLUGIN_METADATA_NOT_FOUND,
    }
}

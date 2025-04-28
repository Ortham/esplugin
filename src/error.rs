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

use std::error;
use std::fmt;
use std::io;
use std::num::NonZeroUsize;
use std::path::Path;
use std::path::PathBuf;
use std::slice::EscapeAscii;

use nom::Err;

#[expect(clippy::error_impl_error)]
#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    NoFilename(PathBuf),
    ParsingIncomplete(MoreDataNeeded),
    ParsingError(Box<[u8]>, ParsingErrorKind),
    DecodeError(Box<[u8]>),
    UnresolvedRecordIds(PathBuf),
    PluginMetadataNotFound(String),
}

impl From<Err<nom::error::Error<&[u8]>>> for Error {
    fn from(error: Err<nom::error::Error<&[u8]>>) -> Self {
        match error {
            Err::Incomplete(nom::Needed::Unknown) => {
                Error::ParsingIncomplete(MoreDataNeeded::UnknownSize)
            }
            Err::Incomplete(nom::Needed::Size(size)) => {
                Error::ParsingIncomplete(MoreDataNeeded::Size(size))
            }
            Err::Error(err) | Err::Failure(err) => Error::ParsingError(
                err.input.into(),
                ParsingErrorKind::GenericParserError(err.code.description().to_owned()),
            ),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::IoError(error)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::IoError(x) => x.fmt(f),
            Error::NoFilename(path) => {
                write!(
                    f,
                    "The plugin path \"{}\" has no filename part",
                    escape_ascii(path)
                )
            }
            Error::ParsingIncomplete(MoreDataNeeded::UnknownSize) => write!(
                f,
                "An unknown number of bytes of additional input was expected by the plugin parser"
            ),
            Error::ParsingIncomplete(MoreDataNeeded::Size(size)) => write!(
                f,
                "{size} bytes of additional input was expected by the plugin parser"
            ),
            Error::ParsingError(input, kind) => write!(
                f,
                "An error was encountered while parsing the plugin content \"{}\": {kind}",
                input.escape_ascii()
            ),
            Error::DecodeError(bytes) => write!(
                f,
                "Plugin string content could not be decoded from Windows-1252, content is \"{}\"",
                bytes.escape_ascii()
            ),
            Error::UnresolvedRecordIds(path) => {
                write!(
                    f,
                    "Record IDs are unresolved for plugin at \"{}\"",
                    escape_ascii(path)
                )
            }
            Error::PluginMetadataNotFound(plugin) => {
                write!(f, "Plugin metadata for \"{plugin}\" not found")
            }
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Error::IoError(x) => Some(x),
            _ => None,
        }
    }
}

fn escape_ascii(path: &Path) -> EscapeAscii {
    path.as_os_str().as_encoded_bytes().escape_ascii()
}

#[derive(Debug)]
pub enum ParsingErrorKind {
    /// The `Vec<u8>` field is the expected record type.
    UnexpectedRecordType(Vec<u8>),
    /// The usize field is the expected minimum data length.
    SubrecordDataTooShort(usize),
    /// The String field is the name of the parser that errored.
    GenericParserError(String),
}

impl fmt::Display for ParsingErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParsingErrorKind::UnexpectedRecordType(v) => {
                write!(f, "Expected record type \"{}\"", v.escape_ascii())
            }
            ParsingErrorKind::SubrecordDataTooShort(s) => write!(
                f,
                "Subrecord data field too short, expected at least {s} bytes",
            ),
            ParsingErrorKind::GenericParserError(e) => write!(f, "Error in parser: {e}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MoreDataNeeded {
    /// It's not known how much more data are needed
    UnknownSize,
    /// Contains the number of bytes of data that are needed
    Size(NonZeroUsize),
}

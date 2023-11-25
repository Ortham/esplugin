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

use nom::Err;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    NoFilename,
    ParsingIncomplete,
    ParsingError(Vec<u8>, ParsingErrorKind),
    DecodeError(Vec<u8>),
}

impl From<Err<nom::error::Error<&[u8]>>> for Error {
    fn from(error: Err<nom::error::Error<&[u8]>>) -> Self {
        match error {
            Err::Incomplete(_) => Error::ParsingIncomplete,
            Err::Error(err) | Err::Failure(err) => Error::ParsingError(
                err.input.to_vec(),
                ParsingErrorKind::GenericParserError(err.code.description().to_string()),
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
            Error::IoError(ref x) => x.fmt(f),
            Error::NoFilename => write!(f, "The plugin path has no filename part"),
            Error::ParsingIncomplete => write!(f, "More input was expected by the plugin parser"),
            Error::ParsingError(input, kind) => write!(
                f,
                "An error was encountered while parsing the plugin content {:02X?}: {}",
                input, kind
            ),
            Error::DecodeError(bytes) => write!(
                f,
                "Plugin string content could not be decoded from Windows-1252, bytes are {bytes:02X?}"
            ),
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

#[derive(Debug)]
pub enum ParsingErrorKind {
    /// The Vec<u8> field is the expected record type.
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
                write!(f, "Expected record type {:02X?}", v)
            }
            ParsingErrorKind::SubrecordDataTooShort(s) => write!(
                f,
                "Subrecord data field too short, expected at least {} bytes",
                s
            ),
            ParsingErrorKind::GenericParserError(e) => write!(f, "Error in parser: {}", e),
        }
    }
}

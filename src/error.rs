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
    ParsingError,
    DecodeError,
}

impl<E> From<Err<E>> for Error {
    fn from(error: Err<E>) -> Self {
        match error {
            Err::Incomplete(_) => Error::ParsingIncomplete,
            _ => Error::ParsingError,
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
        match *self {
            Error::IoError(ref x) => x.fmt(f),
            Error::NoFilename => write!(f, "The plugin path has no filename part"),
            Error::ParsingIncomplete => write!(f, "More input was expected by the plugin parser"),
            Error::ParsingError => write!(f, "An error was encountered while parsing a plugin"),
            Error::DecodeError => write!(
                f,
                "Plugin string content could not be decoded from Windows-1252"
            ),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::IoError(ref x) => x.description(),
            Error::NoFilename => "The plugin path has no filename part",
            Error::ParsingIncomplete => "More input was expected by the plugin parser",
            Error::ParsingError => "An error was encountered while parsing a plugin",
            Error::DecodeError => "Plugin string content could not be decoded from Windows-1252",
        }
    }

    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Error::IoError(x) => Some(x),
            _ => None,
        }
    }
}

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

use std::borrow::Cow;
use std::error;
use std::fmt;
use std::io;

use nom::IError;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    NoFilename,
    ParsingIncomplete,
    ParsingError,
    DecodeError(Cow<'static, str>),
}

impl From<IError> for Error {
    fn from(error: IError) -> Self {
        match error {
            IError::Error(_) => Error::ParsingError,
            _ => Error::ParsingIncomplete,
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::IoError(error)
    }
}

impl From<Cow<'static, str>> for Error {
    fn from(error: Cow<'static, str>) -> Self {
        Error::DecodeError(error)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::IoError(ref x) => x.fmt(f),
            Error::NoFilename => write!(f, "The plugin path has no filename part"),
            Error::ParsingIncomplete => write!(f, "More input was expected by the plugin parser"),
            Error::ParsingError => {
                write!(f, "{}", "An error was encountered while parsing a plugin")
            }
            Error::DecodeError(_) => write!(
                f,
                "{}",
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
            Error::DecodeError(_) => "Plugin string content could not be decoded from Windows-1252",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::IoError(ref x) => Some(x),
            Error::NoFilename => None,
            Error::ParsingIncomplete => None,
            Error::ParsingError => None,
            Error::DecodeError(_) => None,
        }
    }
}

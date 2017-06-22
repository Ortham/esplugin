/*
 * This file is part of libespm
 *
 * Copyright (C) 2017 Oliver Hamlet
 *
 * libespm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libespm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libespm. If not, see <http://www.gnu.org/licenses/>.
 */

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum GameId {
    Oblivion,
    Skyrim,
    Fallout3,
    FalloutNV,
    Morrowind,
    Fallout4,
}

#[no_mangle]
pub static ESPM_GAME_OBLIVION: u32 = GameId::Oblivion as u32;

#[no_mangle]
pub static ESPM_GAME_SKYRIM: u32 = GameId::Skyrim as u32;

#[no_mangle]
pub static ESPM_GAME_FALLOUT3: u32 = GameId::Fallout3 as u32;

#[no_mangle]
pub static ESPM_GAME_FALLOUTNV: u32 = GameId::FalloutNV as u32;

#[no_mangle]
pub static ESPM_GAME_MORROWIND: u32 = GameId::Morrowind as u32;

#[no_mangle]
pub static ESPM_GAME_FALLOUT4: u32 = GameId::Fallout4 as u32;

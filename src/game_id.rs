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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum GameId {
    Oblivion,
    Skyrim,
    Fallout3,
    FalloutNV,
    Morrowind,
    Fallout4,
    SkyrimSE,
    Starfield,
}

impl GameId {
    pub fn supports_light_plugins(self) -> bool {
        matches!(
            self,
            GameId::SkyrimSE | GameId::Fallout4 | GameId::Starfield
        )
    }

    pub fn supports_medium_plugins(self) -> bool {
        self == GameId::Starfield
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn supports_light_plugins_should_be_false_for_morrowind() {
        assert!(!GameId::Morrowind.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_false_for_oblivion() {
        assert!(!GameId::Oblivion.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_false_for_skyrim() {
        assert!(!GameId::Skyrim.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_false_for_fallout3() {
        assert!(!GameId::Fallout3.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_false_for_falloutnv() {
        assert!(!GameId::FalloutNV.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_true_for_skyrimse() {
        assert!(GameId::SkyrimSE.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_true_for_fallout4() {
        assert!(GameId::Fallout4.supports_light_plugins());
    }

    #[test]
    fn supports_light_plugins_should_be_true_for_starfield() {
        assert!(GameId::Starfield.supports_light_plugins());
    }

    #[test]
    fn supports_medium_plugins_should_be_true_for_only_starfield() {
        assert!(!GameId::Morrowind.supports_medium_plugins());
        assert!(!GameId::Oblivion.supports_medium_plugins());
        assert!(!GameId::Skyrim.supports_medium_plugins());
        assert!(!GameId::SkyrimSE.supports_medium_plugins());
        assert!(!GameId::Fallout3.supports_medium_plugins());
        assert!(!GameId::FalloutNV.supports_medium_plugins());
        assert!(!GameId::Fallout4.supports_medium_plugins());
        assert!(GameId::Starfield.supports_medium_plugins());
    }
}

-- |
-- Module      : Mttt.Tablero.Tui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Interfaz de texto del /meta tres en raya/.
module Mttt.Tablero.Tui where

import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import Mttt.Bloque.Data
import Mttt.Common.Utils
import Mttt.Tablero.Data
import Text.Read (readMaybe)

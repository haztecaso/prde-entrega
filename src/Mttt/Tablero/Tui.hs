{-
Module      : Mttt.Tablero.Tui
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Interfaz de texto del /meta tres en raya/.
-}

module Mttt.Tablero.Tui where

import Mttt.Common.Utils 
import Mttt.Bloque.Data
import Mttt.Tablero.Data

import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (elemIndex)
import Text.Read (readMaybe)


{-
Module      : Mttt.Common.Tui
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Definiciones generales de la intefaz de texto.
-}

module Mttt.Common.Tui where

import Mttt.Common.Utils 

import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (elemIndex)
import Text.Read (readMaybe)

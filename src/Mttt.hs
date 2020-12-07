{-
Module      : Mttt
Description : Meta tres en raya
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementaciones de los juegos /tres en raya/ y /meta tres en raya/.
-}

module Mttt (
    -- * __Generalidades__
    module Mttt.Common.Utils
  , module Mttt.Common.Gui
  , module Mttt.Common.Tui

    -- * __Tres en raya__
  , module Mttt.Bloque.Data
  , module Mttt.Bloque.Gui
  , module Mttt.Bloque.Tui

    -- * __Meta Tres en raya__
  , module Mttt.Tablero.Data
  , module Mttt.Tablero.Gui
  , module Mttt.Tablero.Tui
) where

import Mttt.Common.Utils
import Mttt.Common.Gui
import Mttt.Common.Tui

import Mttt.Bloque.Data
import Mttt.Bloque.Gui
import Mttt.Bloque.Tui

import Mttt.Tablero.Data
import Mttt.Tablero.Gui
import Mttt.Tablero.Tui

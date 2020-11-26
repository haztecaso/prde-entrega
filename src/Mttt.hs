{-
Module      : Mttt
Description : Meta tres en raya
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementaciones de los juegos /tres en raya/ y /meta tres en raya/.
-}

module Mttt (
    module Mttt.Bloque  -- | Implementaciones de los tipos y funciones relativos al juego /tres en raya/
  , module Mttt.Tablero -- | Implementaciones de los tipos y funciones relativos al juego /meta tres en raya/
  , module Mttt.Gui -- | Interfaces gráficas para los juegos
) where

import Mttt.Utils
import Mttt.Bloque
import Mttt.Tablero
import Mttt.Gui

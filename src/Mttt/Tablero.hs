{-
Module      : Mttt.Tablero
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementación del juego /meta tres en raya/.
-}

module Mttt.Tablero (
     Tablero
   , showTablero
   , putTablero
   , tableroVacio 
) where

import Mttt.Utils
import Mttt.Bloque

import Data.Array

-- | Tipo para un tablero de /meta tres en raya/
type Tablero = Array Pos Bloque

-- | Representación en caracteres de un 'Tablero'
showTablero :: Tablero -> String
showTablero _ = ""

-- | Utilidad para imprimir en pantalla un 'Tablero'
putTablero :: Tablero-> IO ()
putTablero = putStr . showTablero

-- | 'Tablero' vacío
tableroVacio :: Tablero
tableroVacio = listArray ((1,1),(3,3)) [bloqueVacio | _ <- listaIndices]

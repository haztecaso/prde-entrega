{-
Module      : Mttt.Tablero
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementación del juego /meta tres en raya/.
-}

module Mttt.Tablero (
     Tablero(bloques, bloqueActivo)
   , showTablero
   , putTablero
   , tableroVacio 
) where

import Mttt.Utils
import Mttt.Bloque

import Data.Array

-- | Tipo para un tablero de /meta tres en raya/
data Tablero = T { bloques :: Array Pos Bloque -- ^ Bloques del tablero
                 , bloqueActivo :: Maybe Pos   -- ^ 'Pos' del 'Bloque' activo para jugar.
                                               -- Si es 'Nothing' se puede jugar en
                                               -- cualquier 'Bloque'.
                 } deriving (Eq, Show, Read)

-- | Representación en caracteres de un 'Tablero'
showTablero :: Tablero -> String
showTablero _ = ""

-- | Utilidad para imprimir en pantalla un 'Tablero'
putTablero :: Tablero-> IO ()
putTablero = putStr . showTablero

-- | 'Tablero' vacío
tableroVacio :: Tablero
tableroVacio =
  T { bloques = listArray ((1,1),(3,3)) [bloqueVacio | _ <- listaIndices]
    , bloqueActivo = Nothing 
    }

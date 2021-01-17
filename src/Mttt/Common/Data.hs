-- |
-- Module      : Mttt.Utils.Data
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
module Mttt.Common.Data where

import Mttt.Common.Utils

-- | Tipo que representa una ficha del juego
data Ficha = X | O deriving (Enum, Eq, Read)

-- | Determina si una 'Ficha' es X ('True') o O ('False')
esX :: Ficha -> Bool
esX X = True
esX _ = False

-- | Utilidad para imprimir una /casilla/ de un 'Bloque' en pantalla
showMaybeFicha :: Maybe Ficha -> Char
showMaybeFicha Nothing = '_'
showMaybeFicha (Just f) = head (show f)

instance Show Ficha where
  show X = "✗"
  show O = "○"

-- | Tipo sinónimo para representar posiciones en los tableros.
-- Puede ser utilizado tanto para 'Tablero' como para 'Bloque'
type Pos = (Int, Int)

-- Utilidad para obtener el índice de una posición
pos2int :: Pos -> Int
pos2int (x, y) = (y -1) + 3 * (x -1)

-- Utilidad para obtener la posición de un índice
int2pos :: Int -> Pos
int2pos n = (n `div` 3 + 1, n `mod` 3 + 1)

-- | Lista de indices de un 'Tablero' o 'Bloque'
listaIndices :: [(Int, Int)]
listaIndices = [(x, y) | x <- [1 .. 3], y <- [1 .. 3]]

class Show j => Juego j where
  turno :: j -> Ficha
  ganador :: j -> Maybe Ficha
  tablas :: j -> Bool
  fin :: j -> Bool
  mov :: j -> Pos -> Ficha -> j

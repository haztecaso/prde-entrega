{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

-- | Tipo sinónimo para representar posiciones de 'Bloque'.
type Pos = (Int, Int)

-- | Tipo sinónimo para representar posiciones de 'Tablero'.
type Pos' = (Pos, Pos)

-- Utilidad para obtener el índice de una posición
pos2int :: Pos -> Int
pos2int (x, y) = (y -1) + 3 * (x -1)

-- Utilidad para obtener la posición de un índice
int2pos :: Int -> Pos
int2pos n = (n `div` 3 + 1, n `mod` 3 + 1)

-- | Lista de indices de un 'Tablero' o 'Bloque'
listaIndices :: [(Int, Int)]
listaIndices = [(x, y) | x <- [1 .. 3], y <- [1 .. 3]]

-- | Clase que acomuna la interfaz de los tipos 'Bloque' y 'Tablero'.
--
-- __Atención__: estamos usando algunas extensiones del lenguaje para poder
-- definir clases con más de un parámetro. Además, para que el compilador pueda
-- inferir bien los tipos, hemos definido una dependencia funcional entre dichos
-- parámetros. De este modo podemos tener instancias de esta clase que utilizan
-- tipos distintos para las posiciones de los tableros.
class Show juego => Juego juego pos | juego -> pos where
  -- | Determina a que 'Ficha' le toca jugar.
  turno :: juego -> Maybe Ficha

  -- | Determina quien es el ganador, en caso de haberlo.
  ganador :: juego -> Maybe Ficha

  -- | Determina si la partida ha acabado en tablas.
  tablas :: juego -> Bool

  -- | Determina si la partida ha acabado o no
  fin :: juego -> Bool

  -- | Insertar una ficha nueva, usando 'turno' para decidir que 'Ficha'
  -- colocar. Si el movimiento es válido se devuelve 'Just f' y en caso
  -- contrario 'Nothing'.
  mov :: juego -> pos -> Maybe juego

  -- | Lista de posibles siguientes posiciones.
  expandir :: juego -> [juego]

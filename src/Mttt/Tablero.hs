{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Tablero
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
module Mttt.Tablero
  ( -- * 'Tablero': el tipo para el meta tres en raya
    Tablero (bloqueActivo),
    tableroVacio,

    -- * Funciones heurísticas
    heur0,
  )
where

import Data.Array (Array, listArray, (!), (//))
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, isJust, isNothing)
import Mttt.Bloque (Bloque, bloqueVacio)
import Mttt.Common

-- | Tipo para un tablero de /meta tres en raya/
data Tablero = T
  { -- | Bloques del tablero
    bloques :: Array Pos Bloque,
    -- | 'Pos' del 'Bloque' activo para jugar. Si es 'Nothing' se puede jugar en
    -- cualquier 'Bloque' que no haya llegado a su 'fin'.
    bloqueActivo :: Maybe Pos
  }
  deriving (Eq)

instance Show Tablero where
  show t = linea 1 ++ sep ++ linea 2 ++ sep ++ linea 3
    where
      sep = "──────┼───────┼──────\n"
      linea n =
        unlines $
          map (intercalate " │ ") $
            transpose [lines $ show $ bs ! (n, i) | i <- [1 .. 3]]
        where
          bs = bloques t

instance Juego Tablero (Pos, Pos) Bloque where
  contarFichas t = foldr1 suma [contarFichas $ bloques t ! i | i <- listaIndices]
    where
      suma (a, b) (c, d) = (a + c, b + d)

  casilla t p = bloques t ! p

  casillasLibres t = maybe [p | p <- listaIndices, not (fin $ casilla t p)] (\b -> [b]) (bloqueActivo t)

  posicionesLibres t = [(p1, p2) | p1 <- casillasLibres t, p2 <- posicionesLibres (bloques t ! p1)]

  ganador t
    | Just X `elem` ganadores = Just X
    | Just O `elem` ganadores = Just O
    | otherwise = Nothing
    where
      ganadores = map ganadorLinea $ lineas t
      ganadorLinea l
        | map ganador l == [Just X, Just X, Just X] = Just X
        | map ganador l == [Just O, Just O, Just O] = Just O
        | otherwise = Nothing

  tablas t = null (posicionesLibres t) && isNothing (ganador t)

  fin t
    | isJust (ganador t) = True
    | tablas t = True
    | otherwise = False

  mov t f (p1, p2)
    | validPos && Just p1 == bloqueActivo t = nuevo
    | validPos && isNothing (bloqueActivo t) = nuevo
    | otherwise = Nothing
    where
      validPos = p1 `elem` casillasLibres t && p2 `elem` listaIndices
      bloque = mov (bloques t ! p1) f p2
      siguienteBloqueActivo
        | fin (bloques t ! p2) = Nothing
        | isJust bloque && fin (fromJust bloque) = Nothing --TODO: ¿Cómo evitar fromJust?
        | otherwise = Just p2
      nuevo =
        ( \b ->
            T
              { bloques = bloques t // [(p1, b)],
                bloqueActivo = siguienteBloqueActivo
              }
        )
          <$> bloque

-- | 'Tablero' vacío
tableroVacio :: Tablero
tableroVacio =
  T
    { bloques = listArray ((1, 1), (3, 3)) [bloqueVacio | _ <- listaIndices],
      bloqueActivo = Nothing
    }

lineas :: Tablero -> [[Bloque]]
lineas t = filas ++ columnas ++ diagonales
  where
    filas = [[bloques t ! (x, y) | x <- [1 .. 3]] | y <- [1 .. 3]]
    columnas = transpose filas
    diagonales =
      [ [bloques t ! (x, x) | x <- [1 .. 3]],
        [bloques t ! (x, 4 - x) | x <- [1 .. 3]]
      ]

{- FUNCIONES HEURÍSTICAS -}

heur0 ::
  -- | 'Ficha' con la que juega el agente
  Tablero ->
  Int
heur0 _ = 0

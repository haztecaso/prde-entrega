{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Bloque
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
module Mttt.Bloque
  ( -- * 'Bloque': el tipo para el tres en raya
    Bloque,
    bloqueVacio,

    -- * Funciones heurísticas
    heur0,
  )
where

import Data.Array (Array, elems, listArray, (!), (//))
import Data.List (intersperse, transpose)
import Data.Maybe (isJust, isNothing)
import Mttt.Common

-- | Tipo para un tablero de /tres en raya/
data Bloque = B (Array Pos (Maybe Ficha))
  deriving (Eq)

instance Show Bloque where
  show (B b) = unlines [intersperse ' ' [showMaybeFicha $ b ! (x, y) | y <- [1 .. 3]] | x <- [1 .. 3]]

instance Juego Bloque Pos (Maybe Ficha) where
  contarFichas (B b) = foldr1 suma $ map f $ elems b
    where
      f Nothing = (0, 0)
      f (Just X) = (1, 0)
      f (Just O) = (0, 1)
      suma (a, b) (c, d) = (a + c, b + d)

  casilla (B b) p = b ! p

  casillasLibres (B b) = map fst $ filter snd [((x, y), isNothing $ b ! (x, y)) | (x, y) <- listaIndices]

  posicionesLibres = casillasLibres

  ganador b
    | Just X `elem` ganadores = Just X
    | Just O `elem` ganadores = Just O
    | otherwise = Nothing
    where
      ganadores = map ganadorLinea $ lineas b
      ganadorLinea l
        | l == [Just X, Just X, Just X] = Just X
        | l == [Just O, Just O, Just O] = Just O
        | otherwise = Nothing

  tablas (B b) = notElem Nothing b && isNothing (ganador $ B b)

  fin b
    | isJust (ganador b) = True
    | tablas b = True
    | otherwise = False

  mov (B b) f p
    | p `elem` listaIndices && isNothing (b ! p) = Just (B (b // [(p, Just f)]))
    | otherwise = Nothing

-- | 'Bloque' vacío
bloqueVacio :: Bloque
bloqueVacio = B $ listArray ((1, 1), (3, 3)) [Nothing | _ <- listaIndices]

lineas :: Bloque -> [[Maybe Ficha]]
lineas (B b) = filas ++ columnas ++ diagonales
  where
    filas = [[b ! (x, y) | x <- [1 .. 3]] | y <- [1 .. 3]]
    columnas = transpose filas
    diagonales = [[b ! (x, x) | x <- [1 .. 3]], [b ! (x, 4 - x) | x <- [1 .. 3]]]

{- FUNCIONES HEURÍSTICAS -}

-- | Función heurística para el tres en raya
heur0 ::
  -- | 'Ficha' con la que juega el agente
  Bloque ->
  Int
heur0 b
  | ganador b == Just X = 1
  | ganador b == Just O = -1
  | otherwise = 0

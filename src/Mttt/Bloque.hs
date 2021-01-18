{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Bloque
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Implementación del juego /tres en raya/.
module Mttt.Bloque
  ( -- * 'Bloque': el tipo para el tres en raya
    Bloque,
    bloqueVacio,
    movLibreBloque,

    -- * Funciones heurísticas
    heur0,
  )
where

import Data.Array (Array, elems, listArray, (!), (//))
import Data.List (intersperse, transpose)
import Data.Maybe (fromJust, isJust, isNothing)
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

  mov b
    | isJust ficha = movFichaBloque (fromJust $ turno b) b
    | otherwise = \_ -> Nothing
    where
      ficha = turno b

  expandir b
    | fin b = []
    | otherwise = map (fromJust . mov b) $ casillasLibres b

-- | 'Bloque' vacío
bloqueVacio :: Bloque
bloqueVacio = B $ listArray ((1, 1), (3, 3)) [Nothing | _ <- listaIndices]

lineas :: Bloque -> [[Maybe Ficha]]
lineas (B b) = filas ++ columnas ++ diagonales
  where
    filas = [[b ! (x, y) | x <- [1 .. 3]] | y <- [1 .. 3]]
    columnas = transpose filas
    diagonales = [[b ! (x, x) | x <- [1 .. 3]], [b ! (x, 4 - x) | x <- [1 .. 3]]]

-- | Insertar una `Ficha` nueva en un 'Bloque'. Esta función es la parte común
-- de las funciones `mov` y `movLibreBloque`.
movFichaBloque ::
  Ficha ->
  Bloque ->
  -- | Posición en la que se añade la ficha
  Pos ->
  Maybe Bloque
movFichaBloque ficha (B b) (x, y)
  | ((x, y) `elem` listaIndices)
      && isNothing (b ! (x, y)) =
    Just (B (b // [((x, y), Just ficha)]))
  | otherwise = Nothing

-- | Insertar una `Maybe Ficha` nueva en un 'Bloque', ignorando el turno.
movLibreBloque ::
  Maybe Ficha ->
  Bloque ->
  -- | Posición en la que se añade la ficha
  Pos ->
  Maybe Bloque
movLibreBloque ficha
  | isJust ficha = movFichaBloque $ fromJust ficha
  | otherwise = \_ -> \_ -> Nothing

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Bloque.Data
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Implementación del juego /tres en raya/.
module Mttt.Bloque.Data
  ( Bloque,
    bloqueVacio,
    casillaBloque,
    movLibreBloque,
    AgenteBloque (funAB, nombreAB),
    agenteBTonto,
    agenteBMinimax,
  )
where

import Data.Array (Array, elems, listArray, (!), (//))
import Data.List (elemIndex, intersperse, transpose)
import Data.Maybe (fromJust, isJust, isNothing)
import Mttt.Common.Data
import Mttt.Common.Utils

-- | Tipo para un tablero de /tres en raya/
data Bloque = B (Array Pos (Maybe Ficha))
  deriving (Eq)

instance Show Bloque where
  show (B b) = unlines [intersperse ' ' [showMaybeFicha $ b ! (x, y) | y <- [1 .. 3]] | x <- [1 .. 3]]

instance Juego Bloque Pos where
  contarFichas (B b) = foldr1 suma $ map f $ elems b
    where
      f Nothing = (0, 0)
      f (Just X) = (1, 0)
      f (Just O) = (0, 1)
      suma (a, b) (c, d) = (a + c, b + d)

  posicionesLibres (B b) = map fst $ filter snd [((x, y), isNothing $ b ! (x, y)) | (x, y) <- listaIndices]

  ganador b
    | Just X `elem` ganadores = Just X
    | Just O `elem` ganadores = Just O
    | otherwise = Nothing
    where
      ganadores = map ganadorLinea $ lineasBloque b
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
    | otherwise = map (fromJust . mov b) $ posicionesLibres b

-- | 'Bloque' vacío
bloqueVacio :: Bloque
bloqueVacio = B $ listArray ((1, 1), (3, 3)) [Nothing | _ <- listaIndices]

-- | Obtener valor de una casilla
casillaBloque :: Bloque -> Pos -> Maybe Ficha
casillaBloque (B b) p = b ! p

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
-- Se comporta como `movFichaBloque` si recibe un Just y en caso contrario
-- devuelve un Nothing.
movLibreBloque ::
  Maybe Ficha ->
  Bloque ->
  -- | Posición en la que se añade la ficha
  Pos ->
  Maybe Bloque
movLibreBloque ficha
  | isJust ficha = movFichaBloque $ fromJust ficha
  | otherwise = \_ -> \_ -> Nothing

-- Dados dos bloques devuelve la posición en la que se ha jugado.
-- Esto no es nada bonito, ya que estamos expandiendo el problema
-- innecesariamente y haciendo una búsqueda en un array. Para evitar esto se
-- podría adaptar el algoritmo minimax para que devuelva automaticamente la
-- posición en la que jugar, en vez de los estados...
posMovimientoBloque :: Bloque -> Bloque -> Pos
posMovimientoBloque bloque expandido =
  libres !! fromJust (elemIndex expandido siguientes)
  where
    siguientes = expandir bloque
    libres = posicionesLibres bloque

-- | Devuelve todas las lineas rectas de un 'Bloque'
lineasBloque :: Bloque -> [[Maybe Ficha]]
lineasBloque (B b) = filas ++ columnas ++ diagonales
  where
    filas = [[b ! (x, y) | x <- [1 .. 3]] | y <- [1 .. 3]]
    columnas = transpose filas
    diagonales = [[b ! (x, x) | x <- [1 .. 3]], [b ! (x, 4 - x) | x <- [1 .. 3]]]

{- FUNCIONES HEURÍSTICAS -}

-- | Función heurística para el tres en raya
heurBloque ::
  -- | 'Ficha' con la que juega el agente
  Ficha ->
  Bloque ->
  Int
heurBloque f b
  | ganador b == Just X = a
  | ganador b == Just O = - a
  | otherwise = 0
  where
    a = if (esX f) then 1 else -1

{- AGENTES -}

-- | Tipo para Agentes de 'Bloque'.
data AgenteBloque = AgenteBloque
  { funAB :: Bloque -> Pos,
    nombreAB :: String
  }

-- | Agente que devuelve la primera posición disponible donde jugar,
-- en el orden generado por 'posicionesLibres'.
agenteBTonto :: AgenteBloque
agenteBTonto =
  AgenteBloque
    { funAB = head . posicionesLibres,
      nombreAB = "tonto"
    }

-- | Agente que juega usando el algoritmo minimax y la función heurística
-- `heurBloque`
agenteBMinimax :: AgenteBloque
agenteBMinimax =
  AgenteBloque
    { funAB = \b ->
        posMovimientoBloque b $
          minimax 9 expandir (heurBloque $ fromJust $ turno b) b,
      nombreAB = "minimax"
    }

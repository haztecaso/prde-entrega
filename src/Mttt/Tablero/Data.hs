{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Tablero.Data
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Implementación del juego /meta tres en raya/.
module Mttt.Tablero.Data
  ( Tablero (bloques, bloqueActivo),
    tableroVacio,
    lineasTablero,
    HeurTablero (funHT, descHT),
    heurTablero0,
    AgenteTablero (funAT, nombreAT),
    agenteTTonto,
    agenteTMinimax,
  )
where

import Data.Array (Array, listArray, (!), (//))
import Data.List (elemIndex, intercalate, transpose)
import Data.Maybe (fromJust, isJust, isNothing)
import Mttt.Bloque.Data
import Mttt.Common.Data
import Mttt.Common.Utils

-- | Tipo para un tablero de /meta tres en raya/
data Tablero = T
  { -- | Bloques del tablero
    bloques :: Array Pos Bloque,
    -- | 'Pos' del 'Bloque' activo para jugar.
    -- Si es 'Nothing' se puede jugar en
    -- cualquier 'Bloque'.
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

instance Juego Tablero (Pos, Pos) where
  contarFichas t = foldr1 suma [contarFichas $ bloques t ! i | i <- listaIndices]
    where
      suma (a, b) (c, d) = (a + c, b + d)

  posicionesLibres t
    | isJust (bloqueActivo t) = [(activo, p) | p <- posicionesLibres (bloques t ! activo)]
    | otherwise = [(p1, p2) | p1 <- listaIndices, p2 <- posicionesLibres (bloques t ! p1)]
    where
      activo = fromJust $ bloqueActivo t

  ganador t
    | Just X `elem` ganadores = Just X
    | Just O `elem` ganadores = Just O
    | otherwise = Nothing
    where
      ganadores = map ganadorLinea $ lineasTablero t
      ganadorLinea l
        | map ganador l == [Just X, Just X, Just X] = Just X
        | map ganador l == [Just O, Just O, Just O] = Just O
        | otherwise = Nothing

  tablas t = null (posicionesLibres t) && isNothing (ganador t)

  fin t
    | isJust (ganador t) = True
    | tablas t = True
    | otherwise = False

  mov t (p1, p2)
    | isNothing bloque = Nothing
    | validPos && Just p1 == bloqueActivo t = Just nuevo
    | validPos && isNothing (bloqueActivo t) = Just nuevo
    | otherwise = Nothing
    where
      bloque = movLibreBloque (turno t) (bloques t ! p1) p2
      siguienteBloque p
        | fin (bloques t ! p) = Nothing -- Si una partida de un bloque ha acabado se puede jugar en cualquier bloque
        | otherwise = Just p
      validPos = p1 `elem` listaIndices && p2 `elem` listaIndices
      nuevo =
        T
          { bloques = bloques t // [(p1, fromJust bloque)],
            bloqueActivo = siguienteBloque p2
          }

  expandir t
    | fin t = []
    | otherwise = map (fromJust . mov t) $ posicionesLibres t

-- | 'Tablero' vacío
tableroVacio :: Tablero
tableroVacio =
  T
    { bloques = listArray ((1, 1), (3, 3)) [bloqueVacio | _ <- listaIndices],
      bloqueActivo = Nothing
    }

-- | Devuelve todas las lineas rectas de un 'Tablero'
lineasTablero :: Tablero -> [[Bloque]]
lineasTablero t = filas ++ columnas ++ diagonales
  where
    filas = [[bloques t ! (x, y) | x <- [1 .. 3]] | y <- [1 .. 3]]
    columnas = transpose filas
    diagonales = [[bloques t ! (x, x) | x <- [1 .. 3]], [bloques t ! (x, 4 - x) | x <- [1 .. 3]]]

-- Dados dos bloques devuelve la posición en la que se ha jugado.
posMovimientoTablero :: Tablero -> Tablero -> (Pos, Pos)
posMovimientoTablero tablero expandido =
  libres !! fromJust (elemIndex expandido siguientes)
  where
    siguientes = expandir tablero
    libres = posicionesLibres tablero

tableroTest' = fromJust $ mov tableroVacio ((2, 2), (1, 1))

tableroTest'' = fromJust $ mov tableroTest' ((1, 1), (1, 1))

tableroTest''' = fromJust $ mov tableroTest'' ((1, 1), (2, 3))

tableroTest'''' = fromJust $ mov tableroTest''' ((2, 3), (3, 1))

tableroTest = fromJust $ mov tableroTest'''' ((3, 1), (2, 2))

{- FUNCIONES HEURÍSTICAS -}

-- | Tipo para funciones heurísticas de 'Tablero'.
data HeurTablero = HeurTablero
  { funHT :: Tablero -> Int,
    descHT :: String
  }

-- | Función heuristica tonta, para testeos
heurTablero0 :: HeurTablero
heurTablero0 =
  HeurTablero
    { funHT = const 0,
      descHT = "0"
    }

{- AGENTES -}

-- | Tipo para Agentes de 'Tablero'.
data AgenteTablero = AgenteTablero
  { funAT :: Tablero -> (Pos, Pos),
    nombreAT :: String
  }

-- | Agente que devuelve la primera posición disponible donde jugar,
-- en el orden generado por 'posicionesLibres'.
agenteTTonto :: AgenteTablero
agenteTTonto =
  AgenteTablero
    { funAT = head . posicionesLibres,
      nombreAT = "tonto"
    }

-- | Dada una 'HeurTablero' y una profundidad devuelve el 'AgenteTablero'
-- que juega con el minimax.
agenteTMinimax :: HeurTablero -> Int -> AgenteTablero
agenteTMinimax h prof =
  AgenteTablero
    { funAT = \t -> posMovimientoTablero t (minimax prof expandir (funHT h) t),
      nombreAT = "minimax - heur " ++ descHT h ++ " (profundidad " ++ show prof ++ ")"
    }

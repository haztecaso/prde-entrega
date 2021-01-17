-- |
-- Module      : Mttt.Tablero.Data
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Implementación del juego /meta tres en raya/.
module Mttt.Tablero.Data
  ( Tablero (bloques, bloqueActivo),
    showTablero,
    putTablero,
    tableroVacio,
    tableroTest,
    turnoTablero,
    ganadorTablero,
    lineasTablero,
    finTablero,
    movTablero,
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
  deriving (Eq, Read, Show)

-- | Representación en caracteres de un 'Tablero'
showTablero :: Tablero -> String
showTablero t =
  showTablero' t 1
    ++ "──────┼───────┼──────\n"
    ++ showTablero' t 2
    ++ "──────┼───────┼──────\n"
    ++ showTablero' t 3

-- | Función auxiliar para imprimir una linea de un tablero
showTablero' :: Tablero -> Int -> String
showTablero' t n =
  unlines $
    map (intercalate " │ ") $
      transpose [lines $ showBloque $ bs ! (n, i) | i <- [1 .. 3]]
  where
    bs = bloques t

-- | Utilidad para imprimir en pantalla un 'Tablero'
putTablero :: Tablero -> IO ()
putTablero = putStr . showTablero

-- | 'Tablero' vacío
tableroVacio :: Tablero
tableroVacio =
  T
    { bloques = listArray ((1, 1), (3, 3)) [bloqueVacio | _ <- listaIndices],
      bloqueActivo = Nothing
    }

-- | Cuenta las fichas de cada tipo que hay en un 'Tablero'.
contarFichasTablero ::
  Tablero ->
  -- | El primer valor corresponde al número de X's y el segundo a las O's
  (Int, Int)
contarFichasTablero t = foldr1 suma [contarFichasBloque $ bloques t ! i | i <- listaIndices]
  where
    suma (a, b) (c, d) = (a + c, b + d)

-- | Devuelve todas las lineas rectas de un 'Tablero'
lineasTablero :: Tablero -> [[Bloque]]
lineasTablero t = filas ++ columnas ++ diagonales
  where
    filas = [[bloques t ! (x, y) | x <- [1 .. 3]] | y <- [1 .. 3]]
    columnas = transpose filas
    diagonales = [[bloques t ! (x, x) | x <- [1 .. 3]], [bloques t ! (x, 4 - x) | x <- [1 .. 3]]]

-- | Determina quien ha ganado la partida si es que alguien ha ganado.
ganadorTablero :: Tablero -> Maybe Ficha
ganadorTablero t
  | Just X `elem` ganadores = Just X
  | Just O `elem` ganadores = Just O
  | otherwise = Nothing
  where
    ganadores = map ganadorLinea $ lineasTablero t
    ganadorLinea l
      | map ganadorBloque l == [Just X, Just X, Just X] = Just X
      | map ganadorBloque l == [Just O, Just O, Just O] = Just O
      | otherwise = Nothing

-- | Determina a quien le toca
turnoTablero :: Tablero -> Maybe Ficha
turnoTablero t
  | isNothing (ganadorTablero t) && (xs - os) == 1 = Just O
  | isNothing (ganadorTablero t) && (xs - os) == 0 = Just X
  | otherwise = Nothing
  where
    (xs, os) = contarFichasTablero t

-- | Determina si la partida ha acabado en tablas.
tablasTablero :: Tablero -> Bool
tablasTablero t = null (casillasDisponiblesTablero t) && isNothing (ganadorTablero t)

-- | Determina si la partida ha acabado o no
finTablero :: Tablero -> Bool
finTablero t
  | isJust (ganadorTablero t) = True
  | tablasTablero t = True
  | otherwise = False

-- | Insertar una ficha nueva en un 'Tablero'. La función hace uso de
-- 'turnoTablero' para decidir que 'Ficha' colocar.
--
-- Si el movimiento es válido se devuelve un 'Just Tablero'.
-- En caso contrario se devuelve 'Nothing'
movTablero ::
  Tablero ->
  -- | 'Bloque' en que jugar
  Pos ->
  -- | Posición del 'Bloque' seleccionado donde poner la ficha
  Pos ->
  Maybe Tablero
movTablero t p1 p2
  | isNothing bloque = Nothing
  | validPos && Just p1 == bloqueActivo t = Just nuevo
  | validPos && isNothing (bloqueActivo t) = Just nuevo
  | otherwise = Nothing
  where
    bloque = movMaybeFichaBloque (turnoTablero t) (bloques t ! p1) p2
    siguienteBloque p
      | finBloque (bloques t ! p) = Nothing -- Si una partida de un bloque ha acabado se puede jugar en cualquier bloque
      | otherwise = Just p
    validPos = p1 `elem` listaIndices && p2 `elem` listaIndices
    nuevo =
      T
        { bloques = bloques t // [(p1, fromJust bloque)],
          bloqueActivo = siguienteBloque p2
        }

-- | Lista de casillas de un bloque donde se puede jugar.
casillasDisponiblesTablero :: Tablero -> [(Pos, Pos)]
casillasDisponiblesTablero t
  | isJust (bloqueActivo t) = [(activo, p) | p <- casillasLibresBloque (bloques t ! activo)]
  | otherwise = [(p1, p2) | p1 <- listaIndices, p2 <- casillasLibresBloque (bloques t ! p1)]
  where
    activo = fromJust $ bloqueActivo t

-- | Posibles jugadas
expandirTablero :: Tablero -> [Tablero]
expandirTablero t
  | finTablero t = []
  | otherwise = map (fromJust . uncurry (movTablero t)) $ casillasDisponiblesTablero t

-- Dados dos bloques devuelve la posición en la que se ha jugado.
posMovimientoTablero :: Tablero -> Tablero -> (Pos, Pos)
posMovimientoTablero tablero expandido =
  libres !! fromJust (elemIndex expandido siguientes)
  where
    siguientes = expandirTablero tablero
    libres = casillasDisponiblesTablero tablero

tableroTest' = (fromJust $ movTablero tableroVacio (2, 2) (1, 1))

tableroTest'' = (fromJust $ movTablero tableroTest' (1, 1) (1, 1))

tableroTest''' = (fromJust $ movTablero tableroTest'' (1, 1) (2, 3))

tableroTest'''' = (fromJust $ movTablero tableroTest''' (2, 3) (3, 1))

tableroTest = (fromJust $ movTablero tableroTest'''' (3, 1) (2, 2))

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
-- en el orden generado por 'casillasDisponiblesTablero'.
agenteTTonto :: AgenteTablero
agenteTTonto =
  AgenteTablero
    { funAT = head . casillasDisponiblesTablero,
      nombreAT = "tonto"
    }

-- | Dada una 'HeurTablero' y una profundidad devuelve el 'AgenteTablero'
-- que juega con el minimax.
agenteTMinimax :: HeurTablero -> Int -> AgenteTablero
agenteTMinimax h prof =
  AgenteTablero
    { funAT = \t -> posMovimientoTablero t (minimax prof expandirTablero (funHT h) t),
      nombreAT = "minimax - heur " ++ descHT h ++ " (profundidad " ++ show prof ++ ")"
    }

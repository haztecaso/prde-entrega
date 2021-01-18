{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Common
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Tipos de datos, clases y funciones asociadas útiles para los dos juegos.
module Mttt.Common
  ( -- * Tipo 'Ficha' y funciones asociadas.
    Ficha (X, O),
    showMaybeFicha,

    -- * Tipo 'Pos' y utilidades
    Pos,
    pos2int,
    int2pos,
    listaIndices,

    -- * Clase 'Juego'
    Juego
      ( contarFichas,
        casilla,
        casillasLibres,
        posicionesLibres,
        ganador,
        tablas,
        fin,
        mov,
        expandir
      ),
    turno,
    movTonto,
    mov2pos,

    -- * Agentes
    Agente (A, f, nombre),
    agenteTonto,
    agenteMinimax,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)
import Mttt.Inteligencia

-- | Tipo que representa una ficha del juego
data Ficha = X | O deriving (Eq)

instance Show Ficha where
  show X = "✗"
  show O = "○"

-- | Utilidad para imprimir una /casilla/ de un 'Bloque' en pantalla
showMaybeFicha :: Maybe Ficha -> Char
showMaybeFicha Nothing = '_'
showMaybeFicha (Just f) = head (show f)

-- | Tipo sinónimo para representar posiciones de 'Bloque'.
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

-- | Clase que acomuna la interfaz de los tipos 'Bloque' y 'Tablero'.
--
-- __Atención__: estamos usando algunas extensiones del lenguaje para poder
-- definir clases con más de un parámetro. Además, para que el compilador pueda
-- inferir bien los tipos, hemos definido una dependencia funcional entre dichos
-- parámetros. De este modo podemos tener instancias de esta clase que utilizan
-- tipos distintos para las posiciones de los tableros.
class
  (Show juego, Eq juego, Read pos, Show pos) =>
  Juego juego pos casilla
    | juego -> pos casilla
  where
  -- | Cuenta las fichas de cada tipo que hay en el juego. El primer valor es la
  -- cantidad de 'X's y el segundo de 'O's.
  contarFichas :: juego -> (Int, Int)

  -- | Obtener el valor de una casilla. Aquí el tipo es 'Pos' en vez de 'pos'
  -- porque en este caso las casillas siempre están indexadas por valores de
  -- tipo 'Pos'.
  casilla :: juego -> Pos -> casilla

  -- | Casillas (indexadas por 'Pos') donde se puede jugar
  casillasLibres :: juego -> [Pos]

  -- | Posiciones ('pos') donde se puede jugar.
  posicionesLibres :: juego -> [pos]

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

-- | Devuelve la 'Ficha' a la que le toca jugar
turno :: Juego j p c => j -> Maybe Ficha
turno j
  | isNothing (ganador j) && (xs - os) == 1 = Just O
  | isNothing (ganador j) && (xs - os) == 0 = Just X
  | otherwise = Nothing
  where
    (xs, os) = contarFichas j

movTonto :: Juego j p c => j -> j
movTonto j = fromJust (mov j $ (head . posicionesLibres) j)

-- | Dados dos juegos devuelve la posición en la que se ha jugado.
--
-- Esto no es nada bonito, ya que estamos expandiendo el problema
-- innecesariamente y haciendo una búsqueda en un array. Para evitar esto se
-- podría adaptar el algoritmo minimax para que devuelva automaticamente la
-- posición en la que jugar, en vez de los propios juegos...
mov2pos :: Juego j p c => j -> j -> p
mov2pos j1 j2 = posicionesLibres j1 !! fromJust (elemIndex j2 $ expandir j1)

data Agente a = A {f :: (a -> a), nombre :: String}

-- | Ajusta una función heurística para que corresponda a la ficha dada.
ajustaHeur ::
  -- | Función heurística
  (a -> Int) ->
  -- | 'Ficha' del agente
  Ficha ->
  -- | Función heurística ajustada
  (a -> Int)
ajustaHeur heur f = (* x) . heur
  where
    x = if f == X then 1 else -1

-- | Agente que devuelve la primera posición disponible donde jugar,
-- en el orden generado por 'posicionesLibres'.
agenteTonto :: Juego j p c => Ficha -> Agente j
agenteTonto = \_ -> A {f = movTonto, nombre = "Agente tonto"}

agenteMinimax ::
  -- | 'Ficha' del agente
  Ficha ->
  -- | Nombre de la función heurística
  String ->
  -- | Función heurística
  (a -> Int) ->
  -- | Función de expansión
  (a -> [a]) ->
  -- | Profundidad del 'minimax'
  Int ->
  Agente a
agenteMinimax ficha nombreHeur heur exp prof =
  A
    { f = \j -> minimax prof exp (ajustaHeur heur ficha) j,
      nombre = "minimax-" ++ nombreHeur ++ " (prof " ++ show prof ++ ")"
    }

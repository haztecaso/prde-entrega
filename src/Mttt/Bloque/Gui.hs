-- |
-- Module      : Mttt.Bloque.Gui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Interfaz gráfica del /tres en raya/.
module Mttt.Bloque.Gui
  ( EstadoBloque (EB, bloqueEB, centroEB, tamEB, temaEB),
    estadoBloqueInicial,
    guiBloqueAgente,
  )
where

import Data.Maybe (fromJust, isJust)
import Graphics.Gloss (Picture (Blank), Point, color, pictures, play)
import Mttt.Bloque.Data
import Mttt.Common.Data (Ficha (O, X), Pos, casilla, fin, ganador, listaIndices, mov, tablas, turno)
import Mttt.Common.Gui

-- | Tipo que encapsula los datos necesarios para dibujar un 'Bloque' en pantalla
data EstadoBloque = EB
  { -- | 'Bloque' a dibujar
    bloqueEB :: Bloque,
    -- | Posición del centro del tablero
    centroEB :: Point,
    -- | Tamaño del tablero
    tamEB :: Float,
    -- | 'Tema' con el que dibujar el tablero
    temaEB :: Tema
  }
  deriving (Show)

instance Estado EstadoBloque where
  tam = tamEB
  centro = centroEB
  tema = temaEB

  dibuja e = pictures $ [dibujaCasilla pos | pos <- listaIndices]
    where
      dibujaCasilla pos
        | isJust casilla' = dibujaFicha (modTemaEB e) (tam e * 0.2) origen (fromJust casilla')
        | otherwise = Blank
        where
          casilla' = casilla (bloqueEB e) pos
          origen = posPoint (tam e / 3) pos

  modifica p e
    | isJust nuevo = e {bloqueEB = fromJust nuevo}
    | fin b = e {bloqueEB = bloqueVacio}
    | otherwise = e
    where
      b = bloqueEB e
      nuevo = mov b $ pointPos p (tam e) (centro e)

-- | Función para construir un 'EstadoBloque' con un 'bloqueVacio'tableroVacio'
estadoBloqueInicial ::
  -- | Tamaño
  Float ->
  Tema ->
  EstadoBloque
estadoBloqueInicial tam tema =
  EB
    { bloqueEB = bloqueVacio,
      centroEB = (0, 0),
      tamEB = tam,
      temaEB = tema
    }

-- | Pintar el ganador de la partida. Estaría bien que se pinten solo las
--  casillas ganadoras, pero para esto habría que cambiar el modo en que se usan
--  los temas.
modTemaEB ::
  EstadoBloque ->
  Tema
modTemaEB estado
  | ganador b == Just X = t {secundario = n}
  | ganador b == Just O = t {principal = n}
  | tablas b = t {secundario = n, principal = n}
  | otherwise = t
  where
    b = bloqueEB estado
    t = temaEB estado
    n = neutro t

-- | Función que ejecuta la jugada de un 'AgenteBloque'.
modificaEBAgente ::
  -- | 'AgenteBloque' con el que calcular la jugada
  AgenteBloque ->
  -- | 'Ficha' del 'AgenteBloque'
  Ficha ->
  -- | Frame actual del juego (parámetro ignorado)
  Float ->
  -- | Estado actual del tablero
  EstadoBloque ->
  EstadoBloque
modificaEBAgente agente fichaAgente _ estado =
  if turno b == Just fichaAgente && not (fin b)
    then (estado {bloqueEB = fromJust $ mov b $ funAB agente b})
    else estado
  where
    b = bloqueEB estado

-- | Función IO para jugar al /tres en raya/ contra un agente
--
-- __NOTA:__ Debido a que la librería /gloss/ es algo limitada hemos tenido que usar de una manera un poco cutre la función 'play'.
guiBloqueAgente ::
  -- | Tema con el que dibujar la interfaz
  Tema ->
  -- | Tamaño del tablero
  Float ->
  -- | 'AgenteBloque' contra el que jugar
  AgenteBloque ->
  -- | Ficha del 'AgenteBloque'
  Ficha ->
  IO ()
guiBloqueAgente tema tam agente fichaAgente =
  play (ventana tam) (fondo tema) 3 (estadoBloqueInicial tam tema) dibuja' modificaEvent (modificaEBAgente agente fichaAgente)

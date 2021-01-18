-- |
-- Module      : Mttt.Tablero.Gui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Interfaz gráfica del /meta tres en raya/.
module Mttt.Tablero.Gui
  ( EstadoTablero,
    estadoTableroInicial,
    guiAgenteTablero,
  )
where

import Data.Maybe (fromJust, isJust)
import Graphics.Gloss (Picture (Blank, Scale, Text), Point, bright, color, pictures)
import Mttt.Bloque.Data (bloqueVacio)
import Mttt.Bloque.Gui (EstadoBloque (EB, bloqueEB, centroEB, tamEB, temaEB))
import Mttt.Common.Data
import Mttt.Common.Data (Ficha (O, X), Pos, casilla, casillasLibres, fin, listaIndices, mov, turno)
import Mttt.Common.Gui
import Mttt.Tablero.Data

-- | Tipo que encapsula los datos necesarios para dibujar un 'Tablero' en
-- pantalla
data EstadoTablero = ET
  { -- | 'Tablero' a dibujar
    tableroET :: Tablero,
    -- | Tamaño del tablero
    tamET :: Float,
    -- | 'Tema' con el que dibujar el tablero
    temaET :: Tema
  }
  deriving (Show)

instance Estado EstadoTablero where
  tam = tamET
  tema = temaET

  dibuja e =
    pictures $
      dibujaTurno e :
      dibujaCasillasTerminadas e :
      dibujaCasillasLibres e :
        [dibuja' $ eBloque pos | pos <- listaIndices]
    where
      eBloque pos =
        EB
          { bloqueEB = casilla (tableroET e) pos,
            centroEB = posPoint (tam e / 3) pos,
            tamEB = tam e / 3 * 0.8,
            temaEB = tema e
          }

  modifica p e
    | isJust nuevo = e {tableroET = fromJust nuevo}
    | fin t = e {tableroET = tableroVacio}
    | otherwise = e
    where
      t = tableroET e
      positions = pointPos' p $ tam e
      nuevo = mov t (fst positions, snd positions)

pointPos' ::
  -- | Posición del puntero en la pantalla
  Point ->
  -- | Tamaño
  Float ->
  -- | Posición del puntero en el 'bloqueEB'
  (Pos, Pos)
pointPos' p t = (posBloque, pointPos p (t / 3 * 0.8) (centroBloque))
  where
    posBloque = pointPos p t (0, 0)
    centroBloque = sumP (posPoint (t / 3) posBloque) (- t / 2, - t / 2)

-- | Función para construir un 'EstadoTablero' con un 'tableroVacio'
estadoTableroInicial ::
  -- | Tamaño
  Float ->
  Tema ->
  EstadoTablero
estadoTableroInicial tam tema =
  ET
    { tableroET = tableroVacio,
      tamET = tam,
      temaET = tema
    }

-- | Resalta los bloques activos de un 'EstadoTablero'
dibujaCasillasLibres :: EstadoTablero -> Picture
dibujaCasillasLibres e =
  color (bright $ bright $ fondo $ tema e) $
    pictures
      [translateP (posPoint t p) $ cuadrado $ t * 0.9 | p <- pos]
  where
    t = tam e / 3
    f = \x -> [x]
    pos = maybe (casillasLibres $ tableroET e) f $ bloqueActivo $ tableroET e

-- | Resalta los bloques terminados de un 'EstadoTablero'
dibujaCasillasTerminadas :: EstadoTablero -> Picture
dibujaCasillasTerminadas e =
  pictures $
    [ dibujaFicha (tema e) (t * 0.8) (posPoint t p) f
      | f <- [X, O],
        p <- (pos f)
    ]
  where
    t = tam e / 3
    ganador' pos = ganador $ casilla (tableroET e) pos
    pos ficha = [p | p <- listaIndices, ganador' p == Just ficha]

dibujaTurno :: EstadoTablero -> Picture
dibujaTurno e
  | turno (tableroET e) == Just X = tf c1 (Text "Turno de X")
  | turno (tableroET e) == Just O = tf c2 (Text "Turno de O")
  | otherwise = Blank
  where
    c1 = principal $ temaET e
    c2 = secundario $ temaET e
    t = tam e
    s = t / 2000
    tf c p = color c $ translateP (0, - t / 14) $ Scale s s p

-- | Función que ejecuta la jugada de un 'Agente Tablero'.
modificaEstadoTableroAgente ::
  -- | 'AgenteBloque' con el que calcular la jugada
  Agente Tablero ->
  -- | 'Ficha' del 'AgenteBloque'
  Ficha ->
  -- | Frame actual del juego (parámetro ignorado)
  Float ->
  -- | Estado actual del tablero
  EstadoTablero ->
  EstadoTablero
modificaEstadoTableroAgente agente fichaAgente _ estado =
  if turno t == Just fichaAgente && not (fin t)
    then (estado {tableroET = f agente t})
    else estado
  where
    t = tableroET estado

-- | Función IO para jugar al /tres en raya/ contra un agente
guiAgenteTablero ::
  -- | Estado inicial
  EstadoTablero ->
  -- | 'Ficha' del 'Agente'
  Ficha ->
  -- | 'Agente' contra el que jugar
  Agente Tablero ->
  IO ()
guiAgenteTablero = guiAgente modificaEstadoTableroAgente

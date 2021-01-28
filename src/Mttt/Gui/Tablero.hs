{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Gui.Tablero
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
module Mttt.Gui.Tablero
  ( -- * Estado
    EstadoTablero,
  )
where

import           Graphics.Gloss  (Picture (Blank, Scale, Text), bright, color,
                                  pictures)
import           Mttt.Bloque     (Bloque)
import           Mttt.Common
import           Mttt.Gui.Bloque (EstadoBloque (EB, bloqueEB, centroEB, tamEB, temaEB))
import           Mttt.Gui.Common
import           Mttt.Tablero

-- | Tipo que encapsula los datos necesarios para dibujar un 'Tablero' en
-- pantalla
data EstadoTablero = ET
  { -- | 'Tablero' a dibujar
    tableroET :: Tablero,
    -- | Tamaño del tablero
    tamET     :: Float,
    -- | 'Tema' con el que dibujar el tablero
    temaET    :: Tema
  }
  deriving (Show)

instance Estado EstadoTablero Tablero (Pos, Pos) Bloque where
  inicial tam tema =
    ET
      { tableroET = vacio,
        tamET = tam,
        temaET = tema
      }

  juego = tableroET
  tam = tamET
  tema = temaET

  reemplazaJuego e t = e {tableroET = t}

  pointPos e p = (posBloque, pointPos' (t / 3 * 0.8) (centroBloque) p)
    where
      t = tam e
      posBloque = pointPos' t (0, 0) p
      centroBloque = sumP (posPoint (t / 3) posBloque) (- t / 2, - t / 2)

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

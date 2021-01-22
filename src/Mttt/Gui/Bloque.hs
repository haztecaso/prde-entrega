{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Mttt.Bloque.Gui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
module Mttt.Gui.Bloque
  ( -- * Estado
    EstadoBloque (EB, bloqueEB, centroEB, tamEB, temaEB),
  )
where

import           Graphics.Gloss  (Picture (Blank), Point, color, pictures, play)
import           Mttt.Bloque
import           Mttt.Common
import           Mttt.Gui.Common

-- | Tipo que encapsula los datos necesarios para dibujar un 'Bloque' en pantalla
data EstadoBloque = EB
  { -- | 'Bloque' a dibujar
    bloqueEB :: Bloque,
    -- | Posición del centro del tablero
    centroEB :: Point,
    -- | Tamaño del tablero
    tamEB    :: Float,
    -- | 'Tema' con el que dibujar el tablero
    temaEB   :: Tema
  }
  deriving (Show)

instance Estado EstadoBloque Bloque Pos (Maybe Ficha) where
  inicial tam tema =
    EB
      { bloqueEB = vacio,
        centroEB = (0, 0),
        tamEB = tam,
        temaEB = tema
      }

  juego = bloqueEB
  tam = tamEB
  centro = centroEB
  tema = temaEB

  reemplazaJuego e b = e {bloqueEB = b}

  pointPos e = pointPos' (tam e) (centro e)

  dibuja e = pictures $ [dibujaCasilla pos | pos <- listaIndices]
    where
      dibujaCasilla pos =
        maybe
          Blank
          (dibujaFicha (modTema e) (tam e * 0.2) origen)
          casilla'
        where
          casilla' = casilla (bloqueEB e) pos
          origen = posPoint (tam e / 3) pos

-- | Pintar el ganador de la partida. Estaría bien que se pinten solo las
--  casillas ganadoras.
modTema ::
  EstadoBloque ->
  Tema
modTema estado
  | ganador b == Just X = t {secundario = n}
  | ganador b == Just O = t {principal = n}
  | tablas b = t {secundario = n, principal = n}
  | otherwise = t
  where
    b = bloqueEB estado
    t = temaEB estado
    n = neutro t

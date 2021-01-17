-- |
-- Module      : Mttt.Tablero.Gui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Interfaz gráfica del /meta tres en raya/.
module Mttt.Tablero.Gui where

-- module Mttt.Gui (
--     Tema
--   , temaClaro
--   , temaOscuro
--   , guiBoard
-- ) where

import Data.Array
import Data.Maybe (fromJust, isJust)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Mttt.Bloque.Data (bloqueVacio)
import Mttt.Bloque.Gui
import Mttt.Common.Data
import Mttt.Common.Gui
import Mttt.Common.Utils
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
      dibujaBloquesActivos e : [dibuja' $ eBloque pos | pos <- listaIndices]
    where
      eBloque pos =
        EB
          { bloqueEB = bloques (tableroET e) ! pos,
            posEB = posPoint (tam e / 3) pos,
            tamEB = tam e / 3 * 0.8,
            temaEB = tema e
          }

  modifica pos e
    | isJust nuevo = e {tableroET = fromJust nuevo}
    | finTablero t = e {tableroET = tableroVacio}
    | otherwise = e
    where
      t = tableroET e
      positions = pointPosET pos e
      nuevo = movTablero t (fst positions) (snd positions)

-- | Dada una posición del puntero y un 'EstadoTablero' devuelve las
-- 'Pos' del 'Bloque' y casilla donde está el puntero.
pointPosET ::
  -- | Posición del puntero en la pantalla
  Point ->
  EstadoTablero ->
  -- | Posición del puntero en el 'bloqueEB'
  (Pos, Pos)
pointPosET p e =
  (posBloque, posFicha)
  where
    posBloque = pointPos p (tam e) (pos e)
    posFicha = (2, 2)

estadoTableroInicial ::
  -- | Tamaño
  Float ->
  Tema ->
  EstadoTablero
estadoTableroInicial tam tema =
  ET
    { tableroET = tableroTest,
      tamET = tam,
      temaET = tema
    }

-- | Resalta los bloques activos de un 'EstadoTablero'
dibujaBloquesActivos :: EstadoTablero -> Picture
dibujaBloquesActivos e =
  color (bright $ bright $ fondo $ tema e) $
    pictures
      [translateP (posPoint tam' p) $ cuadrado $ tam' * 0.9 | p <- pos]
  where
    tam' = tam e / 3
    pos = maybe listaIndices (\x -> [x]) $ bloqueActivo $ tableroET e

-- | Ventana para jugar al meta tres en raya
tableroVentana ::
  -- | Tamaño de la ventana
  Int ->
  Display
tableroVentana tam = InWindow "Meta tres en raya" (tam, tam) (0, 0)

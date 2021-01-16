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

  dibuja estado =
    pictures $
      dibujaBloquesActivos estado :
        [dibuja' $ estadoBloque pos | pos <- listaIndices]
    where
      estadoBloque pos =
        EB
          { bloqueEB = bloques (tableroET estado) ! pos,
            posEB = posPoint (tam estado / 3) pos,
            tamEB = tam estado / 3 * 0.8,
            temaEB = tema estado
          }

  modifica pos estado
    | isJust nuevo = estado {tableroET = fromJust nuevo}
    | finTablero t = estado {tableroET = tableroVacio}
    | otherwise = estado
    where
      t = tableroET estado
      positions = pointPosET pos estado
      nuevo = movTablero t (fst positions) (snd positions)

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
dibujaBloquesActivos estado =
  color (bright $ bright $ fondo tema) $
    pictures
      [translateP (posPoint tam p) $ cuadrado $ tam * 0.9 | p <- pos]
  where
    tam = tamET estado / 3
    tema = temaET estado
    pos = maybe listaIndices (\x -> [x]) $ bloqueActivo $ tableroET estado

-- | Dada una posición del puntero y un 'EstadoTablero' devuelve las
-- 'Pos' del 'Bloque' y casilla donde está el puntero.
pointPosET ::
  -- | Posición del puntero en la pantalla
  Point ->
  EstadoTablero ->
  -- | Posición del puntero en el 'bloqueEB'
  (Pos, Pos)
pointPosET p estado =
  (posBloque, posFicha)
  where
    posBloque = pointPos p (tamET estado) (pos estado)
    posFicha = (2, 2)

-- | Ventana para jugar al meta tres en raya
tableroVentana ::
  -- | Tamaño de la ventana
  Int ->
  Display
tableroVentana tam = InWindow "Meta tres en raya" (tam, tam) (0, 0)

{-
Module      : Mttt.Tablero.Gui
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Interfaz gráfica del /meta tres en raya/.
-}

module Mttt.Tablero.Gui where

-- module Mttt.Gui (
--     Tema
--   , temaClaro
--   , temaOscuro
--   , guiBoard
-- ) where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Mttt.Bloque.Gui
import Mttt.Common.Gui
import Mttt.Common.Utils
import Mttt.Tablero.Data

-- | Tipo que encapsula los datos necesarios para dibujar un 'Tablero' en pantalla
data EstadoTablero = ET
  { -- | 'Tablero' a dibujar
    tableroET :: Tablero,
    -- | Posición del centro del tablero
    posET :: Point,
    -- | Tamaño del tablero
    tamET :: Float,
    -- | 'Tema' con el que dibujar el tablero
    temaET :: Tema
  }
  deriving (Show)

-- | 'EstadoTablero' inicial ('tableroVacio')
eTInicial ::
  -- | 'tamET'
  Float ->
  -- |  'temaET'
  Tema ->
  EstadoTablero
eTInicial tam tema =
  ET
    { tableroET = tableroVacio,
      posET = (0, 0),
      tamET = tam,
      temaET = tema
    }

-- | Dibuja un 'EstadoTablero'
dibujaET :: EstadoTablero -> Picture
dibujaET estado =
  translate (x - tam / 2) (y - tam / 2) $
    pictures $
      dibujaLineas (0, 0) tam (contraste $ temaET estado) :
        [dibujaEB $ estadoBloque pos | pos <- listaIndices]
  where
    (x, y) = posET estado
    tam = tamET estado
    tema = temaET estado
    estadoBloque pos =
      EB
        { bloqueEB = bloques (tableroET estado) ! pos,
          posEB = posPoint (tam / 3) pos,
          tamEB = tam / 3 * 0.8,
          temaEB = tema
        }

-- | Modifica el 'EstadoTablero' actual del juego cuando se hace click
modificaET :: Event -> EstadoTablero -> EstadoTablero
modificaET _ estado = estado

-- | Ventana para jugar al meta tres en raya
tableroVentana ::
  -- | Tamaño de la ventana
  Int ->
  Display
tableroVentana tam = InWindow "Meta tres en raya" (tam, tam) (0, 0)

-- | Función IO para pintar un 'EstadoTablero' en pantalla.
-- Tiene la misma interfaz que 'dibujaET'
displayET :: EstadoTablero -> IO ()
displayET estado = display (tableroVentana tam) (fondo $ temaET estado) (dibujaET estado)
  where
    tam = floor $ 1.15 * tamET estado

-- | Función IO para jugar al /meta tres en raya/
guiTableroMulti ::
  -- | Tema con el que dibujar la interfaz
  Tema ->
  -- | Tamaño del tablero
  Float ->
  IO ()
guiTableroMulti tema tam = play (tableroVentana tamV) (fondo tema) 15 (eTInicial tam tema) dibujaET modificaET (const id)
  where
    tamV = floor $ 1.15 * tam

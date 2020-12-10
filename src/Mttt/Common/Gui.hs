{-
Module      : Mttt.Common.Gui
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Definiciones generales de la intefaz gráfica.
-}

module Mttt.Common.Gui where
-- module Mttt.Gui (
--     Tema
--   , temaClaro
--   , temaOscuro
--   , guiBoard
-- ) where

import Mttt.Common.Utils
import Mttt.Bloque.Data
import Mttt.Tablero.Data

import Data.Array
import Data.Maybe (isJust, fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.Picture

{-
  DEFINICIONES GENERALES
-}

-- | Tipo para representar temas (grupos de colores)
-- de la intefaz gráfica del juego.
data Tema = Tema { fondo      :: Color
                 , contraste  :: Color
                 , principal  :: Color
                 , secundario :: Color
                 , neutro     :: Color
                 } deriving (Show)

-- | Tema claro
temaClaro :: Tema
temaClaro = Tema { fondo      = greyN 0.75
                 , contraste  = greyN 0.05
                 , principal  = dim red
                 , secundario = iterate dim green !! 3
                 , neutro     = greyN 0.50
                 }

-- | Tema oscuro, por defecto.
temaOscuro :: Tema
temaOscuro = Tema { fondo     = greyN 0.15
                 , contraste  = greyN 0.95
                 , principal  = red
                 , secundario = green
                 , neutro     = greyN 0.50
                 }

-- | Dado un 'Tema' elimina la variedad de colores,
-- dejando solo el de 'fondo' y el de 'contraste'.
temaBicolor :: Tema -> Tema
temaBicolor tema =
  tema { principal = c
       , secundario = c
       }
  where c = contraste tema

-- | Dibuja lineas del tablero en un cuadrado
dibujaLineas :: Point -- ^ Esquina inferior izquierda del tablero
             -> Float -- ^ Tamaño del tablero
             -> Color -- ^ Color de las líneas
             -> Picture
dibujaLineas p t c = color c $ pictures $
  [ line [(x, 0),(x, t)] | x <- pos]
  ++ [ line [(0, y),(t, y)] | y <- pos]
      where pos = map (+ fst p) [t/3, t*2/3]

-- | Convertir de 'Pos' a posición 'Point'
--
-- __TODO__: explicar mejor
posPoint :: Float -- ^ Tamaño de la ficha
         -> Pos   -- ^ Posición de la ficha
         -> Point -- ^ Posición del centro de la ficha en el dibujo
posPoint tam pos = (tam*(y-1+0.5), tam*(3-x+0.5))
    where (x, y) = (fromIntegral $ fst pos, fromIntegral $ snd pos)

{-
Module      : Mttt.Gui
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Interfaz gráfica para los juegos de /tres en raya/ y /meta tres en raya/
-}

module Mttt.Gui where
-- module Mttt.Gui ( 
--     Tema
--   , temaClaro
--   , temaOscuro
--   , guiBoard
-- ) where

import Mttt.Utils 
import Mttt.Bloque
import Mttt.Tablero

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.Picture

-- | Tipo para representar temas (grupos de colores)
-- de la intefaz gráfica del juego.
data Tema = Tema { fondo :: Color
            , contraste :: Color
            , principal :: Color
            , secundario :: Color
            , neutro :: Color
            } deriving (Show)

-- | Tema claro
temaClaro :: Tema
temaClaro = Tema { fondo      = greyN 0.85
                 , contraste  = greyN 0.05
                 , principal  = red
                 , secundario = green
                 , neutro     = greyN 0.5
                 }

-- | Tema oscuro, por defecto.
temaOscuro :: Tema
temaOscuro = Tema { fondo     = greyN 0.15
                 , contraste  = greyN 0.95
                 , principal  = red
                 , secundario = green
                 , neutro     = greyN 0.4
                 }

-- | Dibujar una cruz
dibujaX :: Point -- ^ Posición del centro de la X
        -> Float -- ^ Tamaño de la X
        -> Float -- ^ Grosor de la X
        -> Color -- ^ Color del círculo
        -> Picture
dibujaX pos tam gros color = translate x y $ pictures $ zipWith rotate [45.0,-45.0] [rect, rect]
                               where rect   = rectangleSolid (sqrt 2 * tam)  gros
                                     (x, y) = pos
-- | Dibujar un círculo
dibujaO :: Point -- ^ Posición del centro del círculo
        -> Float -- ^ Tamaño del círculo
        -> Float -- ^ Grosor del círculo
        -> Color -- ^ Color del círculo
        -> Picture
dibujaO pos tam gros color = translate x y $ pictures $ zipWith rotate [45.0,-45.0] [rect, rect]
                               where rect   = rectangleSolid (sqrt 2 * tam)  gros
                                     (x, y) = pos

-- | Dibujar una ficha
dibujaFicha :: Tema  -- ^ Tema con el que dibujar la 'Ficha'
            -> Float -- ^ Tamaño de la ficha
            -> Point -- ^ Posición de la ficha (esquina superior izquierda)
            -> Ficha -- ^ Ficha a dibujar
            -> Picture
dibujaFicha tema tam pos ficha
  | esX ficha = dibujaX centro tam (tam*0.15) (principal tema)
  | otherwise = dibujaO centro tam (tam*0.15) (secundario tema)
  where centro = (fst pos + tam/2, snd pos + tam/2)

-- | Dibujar lineas del tablero en un cuadrado
dibujaLineas :: Point -- ^ Esquina superior izquierda del tablero
             -> Float -- ^ Tamaño del tablero
             -> Color -- ^ Color de las líneas
             -> Picture
dibujaLineas p t c = color c $ pictures $
  [ line [(x, 0),(x, t)] | x <- pos]
  ++ [ line [(0, y),(t, y)] | y <- pos]
      where pos = map (+ fst p) [t/3, t*2/3]
            linea = 

-- | Dibujar un bloque
dibujaBloque :: Tema -- ^ Tema con el que dibujar el 'Bloque'
             -> Float -- ^ Tamaño del tablero
             -> Bloque
             -> Picture
dibujaBloque tema tam _ = pictures [ dibujaLineas (0,0) tam (contraste tema)
                                   , dibujaFicha tema (tam/3) (0,0) X
                                   ]

-- | Dibuja la celda de un 'Bloque' correspondiente a una 'Pos'ición.
-- dibujaPosBloque :: Bloque -> Pos -> Picture
-- dibujaPosBloque b p = 

-- | TODO
modificarBloque :: Event -> Bloque -> Bloque
modificarBloque _ b = b

bloqueVentana = InWindow "Tres en raya" (640, 480) (100, 100)

guiBoard :: Tema  -- ^ Tema con el que dibujar la interfaz
         -> Float -- ^ Tamaño del tablero
         -> IO ()
guiBoard tema tam = play bloqueVentana (fondo tema) 30 bloqueVacio (dibujaBloque tema tam) modificarBloque (const id)


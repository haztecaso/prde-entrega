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
data Tema = Tema { fondo :: Color
            , contraste :: Color
            , principal :: Color
            , secundario :: Color
            } deriving (Show)

-- | Tema claro
temaClaro :: Tema
temaClaro = Tema { fondo      = greyN 0.85
                 , contraste  = greyN 0.05
                 , principal  = red
                 , secundario = green
                 }

-- | Tema oscuro, por defecto.
temaOscuro :: Tema
temaOscuro = Tema { fondo     = greyN 0.15
                 , contraste  = greyN 0.95
                 , principal  = red
                 , secundario = green
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


{-
  PARTE RELATIVA AL TIPO Ficha
-}

-- | Dibuja una cruz
dibujaX :: Point -- ^ Posición del centro de la X
        -> Float -- ^ Tamaño de la X
        -> Float -- ^ Grosor de la X
        -> Color -- ^ Color del círculo
        -> Picture
dibujaX pos tam gros col =
  translate x y $ color col $ pictures $
  zipWith rotate [45.0,-45.0] [rect, rect]
    where rect   = rectangleSolid (sqrt 2 * (tam - gros))  gros
          (x, y) = pos
-- | Dibuja un círculo
dibujaO :: Point -- ^ Posición del centro del círculo
        -> Float -- ^ Tamaño del círculo
        -> Float -- ^ Grosor del círculo
        -> Color -- ^ Color del círculo
        -> Picture
dibujaO pos tam gros col =
  translate x y $ color col $ thickCircle ((tam - gros)/2)  gros
    where (x, y) = pos

-- | Dibuja una ficha
dibujaFicha :: Tema  -- ^ Tema con el que dibujar la 'Ficha'
            -> Float -- ^ Tamaño de la ficha
            -> Point -- ^ Posición de la ficha (esquina inferior izquierda)
            -> Ficha -- ^ 'Ficha' a dibujar
            -> Picture
dibujaFicha tema tam pos ficha
  | esX ficha = dibujaX pos tam (tam*0.10) (principal tema)
  | otherwise = dibujaO pos tam (tam*0.10) (secundario tema)

{-
  PARTE RELATIVA AL TIPO Bloque
-}

-- | Tipo que encapsula los datos necesarios para dibujar un 'Bloque' en pantalla
data EstadoBloque = EB { bloqueEB :: Bloque -- ^ 'Bloque' a dibujar
                       , posEB :: Point    -- ^ Posición del centro del tablero
                       , tamEB :: Float    -- ^ Tamaño del tablero
                       , temaEB :: Tema    -- ^ 'Tema' con el que dibujar el tablero
                       } deriving (Show)

-- | 'EstadoBloque' inicial ('bloqueVacio')
eBInicial :: Float -- ^ 'tamEB'
          -> Tema  -- ^  'temaEB'
          -> EstadoBloque
eBInicial tam tema = 
  EB { bloqueEB = bloqueVacio
     , posEB    = (-250,0)
     , tamEB    = tam
     , temaEB   = tema
     }

-- | Dibuja una casilla del 'EstadoBloque'
dibujaCasilla :: EstadoBloque
              -> Pos
              -> Picture
dibujaCasilla estado pos
  | isJust(casilla) = dibujaFicha tema (tam*0.2) origen (fromJust casilla)
  | otherwise = Blank
  where casilla = (bloqueEB estado)!pos
        tam = tamEB estado
        tema = temaEB estado
        origen = posPoint (tam/3) pos

-- | Dibuja un 'EstadoBloque'
dibujaEB :: EstadoBloque -> Picture
dibujaEB estado = translate (x-tam/2) (y-tam/2) $ pictures $
                  [dibujaLineas (0,0) tam (contraste tema)] ++
                  [dibujaCasilla estado pos | pos <- listaIndices]
                    where (x, y) = posEB estado
                          tam    = tamEB estado
                          tema   = temaEB estado

-- | Modifica el 'EstadoBloque' actual del juego cuando se hace click
modificaEB :: Event -> EstadoBloque -> EstadoBloque
modificaEB (EventKey (MouseButton LeftButton) Up _ posPuntero) estado
  | isJust(nuevo) = estado {bloqueEB = fromJust(nuevo)}
  | otherwise = estado
  where b      = bloqueEB estado
        nuevo  = movimientoBloque b (turnoBloque b) (pointPosEB posPuntero estado)
modificaEB _ estado = estado

pointPosEB :: Point -- ^ Posición del puntero en la pantalla
           -> EstadoBloque 
           -> Pos -- ^ Posición del puntero en el 'bloqueEB'
pointPosEB (x,y) estado = floor' (4-3*(y-py+tam/2)/tam,
                                  1+3*(x-px+tam/2)/tam)
  where tam     = (tamEB estado)
        tamCelda     = tam/3
        (px, py) = posEB estado
        floor' (a,b) = (floor a, floor b)

-- | Ventana para jugar al tres en raya
bloqueVentana :: Display
bloqueVentana = InWindow "Tres en raya" (800, 800) (0,0)

-- | Función IO para pintar un 'EstadoBloque' en pantalla.
-- Tiene la misma interfaz que 'dibujaEB'
displayEB :: EstadoBloque -> IO ()
displayEB estado = display bloqueVentana (fondo $ temaEB estado) (dibujaEB estado)

-- | Función IO para jugar al /tres en raya/
guiBoard :: Tema  -- ^ Tema con el que dibujar la interfaz
         -> Float -- ^ Tamaño del tablero
         -> IO ()
guiBoard tema tam = play bloqueVentana (fondo tema) 15 (eBInicial tam tema) dibujaEB modificaEB (const id)

{-
  PARTE RELATIVA AL TIPO Tablero
-}

-- | Tipo que encapsula los datos necesarios para dibujar un 'Tablero' en pantalla
data EstadoTablero = ET { tableroET :: Tablero -- ^ 'Tablero' a dibujar
                       , posET :: Point    -- ^ Posición del centro del tablero
                       , tamET :: Float    -- ^ Tamaño del tablero
                       , temaET :: Tema    -- ^ 'Tema' con el que dibujar el tablero
                       } deriving (Show)

-- | 'EstadoTablero' inicial ('tableroVacio')
eTInicial :: Float -- ^ 'tamEB'
          -> Tema  -- ^  'temaEB'
          -> EstadoTablero
eTInicial tam tema = 
  ET { tableroET = tableroVacio
     , posET     = (0,0)
     , tamET     = tam
     , temaET    = tema
     }

-- | Dibuja un 'EstadoTablero'
dibujaET :: EstadoTablero -> Picture
dibujaET estado =
  translate (x-tam/2) (y-tam/2) $ pictures $
  [dibujaLineas (0,0) tam $ contraste $ temaET estado]
  ++ [dibujaEB $ estadoBloque pos| pos <- listaIndices]
    where
      (x, y)           = posET estado
      tam              = tamET estado
      tema pos         = temaET estado
      estadoBloque pos =
        EB { bloqueEB = (bloques $ tableroET estado)!pos
           , posEB = posPoint (tam/3) pos
           , tamEB = tam/3*0.8
           , temaEB = tema pos
           }

-- | Modifica el 'EstadoTablero' actual del juego cuando se hace click
modificaET :: Event -> EstadoTablero -> EstadoTablero
modificaET _ estado = estado

-- | Ventana para jugar al meta tres en raya
tableroVentana :: Display
tableroVentana = InWindow "Meta tres en raya" (800, 800) (0,0)

-- | Función IO para pintar un 'EstadoTablero' en pantalla.
-- Tiene la misma interfaz que 'dibujaET'
displayET :: EstadoTablero -> IO ()
displayET estado = display tableroVentana (fondo $ temaET estado) (dibujaET estado)

-- | Función IO para jugar al /meta tres en raya/
guiTablero :: Tema  -- ^ Tema con el que dibujar la interfaz
           -> Float -- ^ Tamaño del tablero
           -> IO ()
guiTablero tema tam = play tableroVentana (fondo tema) 15 (eTInicial tam tema) dibujaET modificaET (const id)

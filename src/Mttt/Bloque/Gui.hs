{-
Module      : Mttt.Bloque.Gui
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Interfaz gráfica del /tres en raya/.
-}

module Mttt.Bloque.Gui where
-- module Mttt.Gui (
--     Tema
--   , temaClaro
--   , temaOscuro
--   , guiBoard
-- ) where

import Mttt.Bloque.Data
import Mttt.Common.Gui
import Mttt.Common.Utils

import Data.Array
import Data.Maybe                           (fromJust, isJust)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

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

modTemaEB :: EstadoBloque
          -> Tema
modTemaEB estado
  | ganadorBloque b == Just X = t { secundario = n }
  | ganadorBloque b == Just O = t { principal = n}
  | tablasBloque b            = t { secundario = n, principal  = n }
  | otherwise                 = t
  where b = bloqueEB estado
        t = temaEB estado
        n = neutro t

-- | Tipo que encapsula los datos necesarios para dibujar un 'Bloque' en pantalla
data EstadoBloque
  = EB
      { bloqueEB :: Bloque
        -- ^ 'Bloque' a dibujar
      , posEB    :: Point
        -- ^ Posición del centro del tablero
      , tamEB    :: Float
        -- ^ Tamaño del tablero
      , temaEB   :: Tema
        -- ^ 'Tema' con el que dibujar el tablero
      }
  deriving (Show)

-- | 'EstadoBloque' inicial ('bloqueVacio')
eBInicial :: Float -- ^ 'tamEB'
          -> Tema  -- ^  'temaEB'
          -> EstadoBloque
eBInicial tam tema =
  EB { bloqueEB = bloqueVacio
     , posEB    = (0,0)
     , tamEB    = tam
     , temaEB   = tema
     }

-- | Dibuja una casilla del 'EstadoBloque'
dibujaCasilla :: EstadoBloque
              -> Pos
              -> Picture
dibujaCasilla estado pos
  | isJust casilla = dibujaFicha tema (tam*0.2) origen (fromJust casilla)
  | otherwise = Blank
  where casilla = bloqueEB estado ! pos
        tam = tamEB estado
        tema = modTemaEB estado
        origen = posPoint (tam/3) pos

-- | Dibuja un 'EstadoBloque'
dibujaEB :: EstadoBloque -> Picture
dibujaEB estado = translate (x-tam/2) (y-tam/2) $ pictures $
                  dibujaLineas (0,0) tam (contraste tema)
                  : [dibujaCasilla estado pos | pos <- listaIndices]
                    where (x, y) = posEB estado
                          tam    = tamEB estado
                          tema   = temaEB estado


pointPosEB :: Point -- ^ Posición del puntero en la pantalla
           -> EstadoBloque
           -> Pos -- ^ Posición del puntero en el 'bloqueEB'
pointPosEB (x,y) estado = floor' (4-3*(y-py+tam/2)/tam,
                                  1+3*(x-px+tam/2)/tam)
  where tam     = tamEB estado
        (px, py) = posEB estado
        floor' (a,b) = (floor a, floor b)

-- | Modifica el 'EstadoBloque' actual del juego cuando se hace click
modificaEB :: Event -> EstadoBloque -> EstadoBloque
modificaEB (EventKey (MouseButton LeftButton) Up _ posPuntero) estado
  | isJust nuevo   = estado {bloqueEB = fromJust nuevo}
  | finBloque b     = estado {bloqueEB = bloqueVacio}
  | otherwise       = estado
  where b      = bloqueEB estado
        nuevo  = movBloque b (pointPosEB posPuntero estado)
modificaEB _ estado = estado

-- | Ventana para jugar al tres en raya
bloqueVentana :: Int -- ^ Tamaño de la ventana
              -> Display
bloqueVentana tam = InWindow "Tres en raya" (tam, tam) (0,0)

-- | Función IO para pintar un 'EstadoBloque' en pantalla.
-- Tiene la misma interfaz que 'dibujaEB'
displayEB :: EstadoBloque -> IO ()
displayEB estado = display (bloqueVentana tam) (fondo $ temaEB estado) (dibujaEB estado)
  where tam = floor $ 1.15 * tamEB estado

-- | Función IO para jugar al /tres en raya/ en modo multijugador
guiBloqueMulti :: Tema -- ^ Tema con el que dibujar la interfaz
               -> Float -- ^ Tamaño del tablero
               -> IO ()
guiBloqueMulti tema tam = play (bloqueVentana tamV) (fondo tema) 15 (eBInicial tam tema) dibujaEB modificaEB (const id)
  where tamV = floor $ 1.15 * tam


-- | Función que ejecuta la jugada de un 'AgenteBloque'.
modificaEBAgente :: AgenteBloque -- ^ 'AgenteBloque' con el que calcular la jugada
                 -> Ficha        -- ^ 'Ficha' del 'AgenteBloque'
                 -> Float        -- ^ Frame actual del juego (parámetro ignorado)
                 -> EstadoBloque -- ^ Estado actual del tablero
                 -> EstadoBloque
modificaEBAgente agente fichaAgente _ estado =
  if turnoBloque b == Just fichaAgente && not (finBloque b)
     then (estado {bloqueEB = fromJust $ movBloque b $ funAB agente b})
     else estado
         where b = bloqueEB estado

-- | Función IO para jugar al /tres en raya/ contra un agente
--
-- __NOTA:__ Debido a que la librería /gloss/ es algo limitada hemos tenido que usar de una manera un poco cutre la función 'play'.
guiBloqueAgente :: Tema -- ^ Tema con el que dibujar la interfaz
                -> Float -- ^ Tamaño del tablero
                -> AgenteBloque -- ^ 'AgenteBloque' contra el que jugar
                -> Ficha -- ^ Ficha del 'AgenteBloque'
                -> IO ()
guiBloqueAgente tema tam agente fichaAgente =
  play (bloqueVentana tamV) (fondo tema) 3 (eBInicial tam tema) dibujaEB modificaEB (modificaEBAgente agente fichaAgente)
  where tamV = floor $ 1.15 * tam

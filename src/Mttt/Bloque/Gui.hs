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

import Data.Array
import Data.Maybe                           (fromJust, isJust)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Mttt.Bloque.Data
import Mttt.Common.Gui
import Mttt.Common.Utils

{-
  PARTE RELATIVA AL TIPO Ficha
-}

-- | Dibuja una cruz
dibujaX ::
  -- | Posición del centro de la X
  Point ->
  -- | Tamaño de la X
  Float ->
  -- | Grosor de la X
  Float ->
  -- | Color del círculo
  Color ->
  Picture
dibujaX pos tam gros col =
  translate x y $
    color col $
      pictures $
        zipWith rotate [45.0, -45.0] [rect, rect]
  where
    rect = rectangleSolid (sqrt 2 * (tam - gros)) gros
    (x, y) = pos

-- | Dibuja un círculo
dibujaO ::
  -- | Posición del centro del círculo
  Point ->
  -- | Tamaño del círculo
  Float ->
  -- | Grosor del círculo
  Float ->
  -- | Color del círculo
  Color ->
  Picture
dibujaO pos tam gros col =
  translate x y $ color col $ thickCircle ((tam - gros) / 2) gros
  where
    (x, y) = pos

-- | Dibuja una ficha
dibujaFicha ::
  -- | Tema con el que dibujar la 'Ficha'
  Tema ->
  -- | Tamaño de la ficha
  Float ->
  -- | Posición de la ficha (esquina inferior izquierda)
  Point ->
  -- | 'Ficha' a dibujar
  Ficha ->
  Picture
dibujaFicha tema tam pos ficha
  | esX ficha = dibujaX pos tam (tam * 0.10) (principal tema)
  | otherwise = dibujaO pos tam (tam * 0.10) (secundario tema)

{-
  PARTE RELATIVA AL TIPO Bloque
-}

modTemaEB ::
  EstadoBloque ->
  Tema
modTemaEB estado
  | ganadorBloque b == Just X = t {secundario = n}
  | ganadorBloque b == Just O = t {principal = n}
  | tablasBloque b = t {secundario = n, principal = n}
  | otherwise = t
  where
    b = bloqueEB estado
    t = temaEB estado
    n = neutro t

-- | Tipo que encapsula los datos necesarios para dibujar un 'Bloque' en pantalla
data EstadoBloque
  = EB
      { -- | 'Bloque' a dibujar
        bloqueEB :: Bloque
        -- | Posición del centro del tablero
      , posEB    :: Point
        -- | Tamaño del tablero
      , tamEB    :: Float
        -- | 'Tema' con el que dibujar el tablero
      , temaEB   :: Tema
      }
  deriving (Show)

-- | 'EstadoBloque' inicial ('bloqueVacio')
eBInicial ::
  -- | 'tamEB'
  Float ->
  -- |  'temaEB'
  Tema ->
  EstadoBloque
eBInicial tam tema =
  EB
    { bloqueEB = bloqueVacio,
      posEB = (0, 0),
      tamEB = tam,
      temaEB = tema
    }

-- | Dibuja una casilla del 'EstadoBloque'
dibujaCasilla ::
  EstadoBloque ->
  Pos ->
  Picture
dibujaCasilla estado pos
  | isJust casilla = dibujaFicha tema (tam * 0.2) origen (fromJust casilla)
  | otherwise = Blank
  where
    casilla = bloqueEB estado ! pos
    tam = tamEB estado
    tema = modTemaEB estado
    origen = posPoint (tam / 3) pos

-- | Dibuja un 'EstadoBloque'
dibujaEB :: EstadoBloque -> Picture
dibujaEB estado =
  translate (x - tam / 2) (y - tam / 2) $
    pictures $
      dibujaLineas (0, 0) tam (contraste tema) :
        [dibujaCasilla estado pos | pos <- listaIndices]
  where
    (x, y) = posEB estado
    tam = tamEB estado
    tema = temaEB estado

-- | Dada una posición del puntero y un 'EstadoBloque' devuelve la
-- 'Pos' de la casilla del 'Bloque' donde está el puntero.
pointPosEB ::
  -- | Posición del puntero en la pantalla
  Point ->
  EstadoBloque ->
  -- | Posición del puntero en el 'bloqueEB'
  Pos
pointPosEB (x, y) estado =
  floor'
    ( 4 -3 * (y - py + tam / 2) / tam,
      1 + 3 * (x - px + tam / 2) / tam
    )
  where
    tam = tamEB estado
    (px, py) = posEB estado
    floor' (a, b) = (floor a, floor b)

-- | Modifica el 'EstadoBloque' actual del juego cuando se hace click
modificaEB :: Event -> EstadoBloque -> EstadoBloque
modificaEB (EventKey (MouseButton LeftButton) Up _ posPuntero) estado
  | isJust nuevo = estado {bloqueEB = fromJust nuevo}
  | finBloque b = estado {bloqueEB = bloqueVacio}
  | otherwise = estado
  where
    b = bloqueEB estado
    nuevo = movBloque b (pointPosEB posPuntero estado)
modificaEB _ estado = estado

-- | Ventana para jugar al tres en raya
bloqueVentana ::
  -- | Tamaño de la ventana
  Int ->
  Display
bloqueVentana tam = InWindow "Tres en raya" (tam, tam) (0, 0)

-- | Función IO para pintar un 'EstadoBloque' en pantalla.
-- Tiene la misma interfaz que 'dibujaEB'
displayEB :: EstadoBloque -> IO ()
displayEB estado = display (bloqueVentana tam) (fondo $ temaEB estado) (dibujaEB estado)
  where
    tam = floor $ 1.15 * tamEB estado

-- | Función IO para jugar al /tres en raya/ en modo multijugador
guiBloqueMulti ::
  -- | Tema con el que dibujar la interfaz
  Tema ->
  -- | Tamaño del tablero
  Float ->
  IO ()
guiBloqueMulti tema tam = play (bloqueVentana tamV) (fondo tema) 15 (eBInicial tam tema) dibujaEB modificaEB (const id)
  where
    tamV = floor $ 1.15 * tam

-- | Función que ejecuta la jugada de un 'AgenteBloque'.
modificaEBAgente ::
  -- | 'AgenteBloque' con el que calcular la jugada
  AgenteBloque ->
  -- | 'Ficha' del 'AgenteBloque'
  Ficha ->
  -- | Frame actual del juego (parámetro ignorado)
  Float ->
  -- | Estado actual del tablero
  EstadoBloque ->
  EstadoBloque
modificaEBAgente agente fichaAgente _ estado =
  if turnoBloque b == Just fichaAgente && not (finBloque b)
    then (estado {bloqueEB = fromJust $ movBloque b $ funAB agente b})
    else estado
  where
    b = bloqueEB estado

-- | Función IO para jugar al /tres en raya/ contra un agente
--
-- __NOTA:__ Debido a que la librería /gloss/ es algo limitada hemos tenido que usar de una manera un poco cutre la función 'play'.
guiBloqueAgente ::
  -- | Tema con el que dibujar la interfaz
  Tema ->
  -- | Tamaño del tablero
  Float ->
  -- | 'AgenteBloque' contra el que jugar
  AgenteBloque ->
  -- | Ficha del 'AgenteBloque'
  Ficha ->
  IO ()
guiBloqueAgente tema tam agente fichaAgente =
  play (bloqueVentana tamV) (fondo tema) 3 (eBInicial tam tema) dibujaEB modificaEB (modificaEBAgente agente fichaAgente)
  where
    tamV = floor $ 1.15 * tam

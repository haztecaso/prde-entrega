-- |
-- Module      : Mttt.Gui.Common
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Definiciones generales de la intefaz gráfica.
module Mttt.Gui.Common
  ( -- * Utilidades
    translateP,
    sumP,
    cuadrado,
    dibujaFicha,
    posPoint,
    pointPos,

    -- * Temas
    Tema (fondo, contraste, principal, secundario, neutro),
    temaClaro,
    temaOscuro,
    temaBicolor,

    -- * Estados
    Estado (tam, centro, tema, dibuja, modifica),
    displayEstado,
    dibuja',

    -- * Interfaz gráfica
    guiMulti,
    guiAgente,
  )
where

import Data.Bifunctor (bimap)
import Graphics.Gloss (Picture, display, play)
import Graphics.Gloss.Interface.IO.Interact
import Mttt.Common

ventana ::
  -- | Tamaño de la ventana
  Float ->
  Display
ventana tam = InWindow "Meta tres en raya" (tamV, tamV) (0, 0)
  where
    tamV = floor $ 1.25 * tam

-- | Transladar a un 'Punto'
translateP :: Point -> Picture -> Picture
translateP (x, y) = translate x y

-- | Sumar dos puntos
sumP :: Point -> Point -> Point
sumP (x, y) (x', y') = (x + x', y + y')

-- | Distancia eucídea entre dos puntos
distancia :: Point -> Point -> Float
distancia (x1, y1) (x2, y2) =
  sqrt (x' * x' + y' * y')
  where
    x' = x1 - x2
    y' = y1 - y2

-- | Convertir de radianes a grados
rad2grad :: Float -> Float
rad2grad = (* (180 / pi))

-- | Tipo para representar temas (grupos de colores)
-- de la intefaz gráfica del juego.
data Tema = Tema
  { fondo :: Color,
    contraste :: Color,
    principal :: Color,
    secundario :: Color,
    neutro :: Color
  }
  deriving (Show)

-- | Tema claro
temaClaro :: Tema
temaClaro =
  Tema
    { fondo = greyN 0.75,
      contraste = greyN 0.05,
      principal = dim red,
      secundario = iterate dim green !! 3,
      neutro = greyN 0.50
    }

-- | Tema oscuro, por defecto.
temaOscuro :: Tema
temaOscuro =
  Tema
    { fondo = greyN 0.15,
      contraste = greyN 0.95,
      principal = red,
      secundario = green,
      neutro = greyN 0.50
    }

-- | Dado un 'Tema' elimina la variedad de colores,
-- dejando solo el de 'fondo' y el de 'contraste'.
temaBicolor :: Tema -> Tema
temaBicolor tema =
  tema
    { principal = c,
      secundario = c
    }
  where
    c = contraste tema

-- | Dibuja lineas del tablero en un cuadrado
segmento ::
  -- | Extremos del segmento
  (Point, Point) ->
  -- | Grosor del segmento
  Float ->
  Picture
segmento (p1@(x1, y1), p2@(x2, y2)) gros =
  translate ((x1 + x2) / 2) ((y1 + y2) / 2) $
    rotate (rad2grad (atan ((y1 - y2) / (x1 - x2)))) $
      rectangleSolid (distancia p1 p2) gros

-- | Dibuja un cuadrado
cuadrado :: Float -> Picture
cuadrado l = rectangleSolid l l

-- | Dibuja una cruz
cruz ::
  -- | Posición del centro de la X
  Point ->
  -- | Tamaño de la X
  Float ->
  -- | Grosor de la X
  Float ->
  -- | Color del círculo
  Color ->
  Picture
cruz pos tam gros col =
  translate x y $
    color col $
      pictures $
        zipWith rotate [45.0, -45.0] [rect, rect]
  where
    rect = rectangleSolid (sqrt 2 * (tam - gros)) gros
    (x, y) = pos

-- | Dibuja un círculo
circulo ::
  -- | Posición del centro del círculo
  Point ->
  -- | Tamaño del círculo
  Float ->
  -- | Grosor del círculo
  Float ->
  -- | Color del círculo
  Color ->
  Picture
circulo pos tam gros col =
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
  | ficha == X = cruz pos tam (tam * 0.10) (principal tema)
  | otherwise = circulo pos tam (tam * 0.10) (secundario tema)

-- | Dibuja una cuadrícula
cuadricula ::
  -- | Esquina inferior izquierda del tablero
  Point ->
  -- | Tamaño del tablero
  Float ->
  -- | Grosor de las líneas
  Float ->
  -- | Color de las líneas
  Color ->
  Picture
cuadricula p t g c =
  color c $
    pictures $
      [segmento ((x, 0), (x, t)) g | x <- pos]
        ++ [segmento ((0, y), (t, y)) g | y <- pos]
  where
    pos = map (+ fst p) [t / 3, t * 2 / 3]

-- | Convertir de 'Pos' a posición 'Point'
posPoint ::
  -- | Tamaño de la 'Ficha' o 'Bloque'
  Float ->
  -- | Posición de la 'Ficha' o 'Bloque'
  Pos ->
  -- | Posición del centro de la 'Ficha' o 'Bloque' en el dibujo
  Point
posPoint tam pos = (tam * (y -1 + 0.5), tam * (3 - x + 0.5))
  where
    (x, y) = bimap fromIntegral fromIntegral pos

-- | Dada una posición del puntero y un tamaño y centro de un estado devuelve
-- la 'Pos' correspondiente.
pointPos ::
  -- | Posición del puntero en pantalla
  Point ->
  -- | Tamaño
  Float ->
  -- | Centro del 'Bloque' o 'Tablero'
  Point ->
  Pos
pointPos (x, y) tam (cx, cy) =
  floor'
    ( 4 -3 * (y - cy + tam / 2) / tam,
      1 + 3 * (x - cx + tam / 2) / tam
    )
  where
    floor' (a, b) = (floor a, floor b)

-- | Clase para los estados de 'Bloque' y 'Tablero'. Seguramente haya una manera
-- mejor de definir los estados, sin pasar los getters de los tipos
-- 'EstadoBloque' y 'EstadoTablero'.
class Estado e where
  -- | Tamaño del tablero
  tam :: e -> Float

  -- | Posición del centro del tablero
  centro :: e -> Point
  centro _ = (0, 0)

  tema :: e -> Tema

  -- | Función para dibujar un estado
  dibuja :: e -> Picture

  -- | Función para modificar un estado
  modifica :: Point -> e -> e

-- | Display de un estado. Útil para hacer testing.
displayEstado :: Estado e => e -> IO ()
displayEstado e = display (ventana $ tam e) (fondo $ tema e) (dibuja' e)

-- | Parte común de los dibujos. Posiciona correctamente y añade una cuadrícula
-- al dibujo de un estado.
dibuja' :: Estado e => e -> Picture
dibuja' estado =
  translateP centro' $
    pictures
      [ cuadricula (0, 0) (tam estado) 1 (contraste $ tema estado),
        dibuja estado
      ]
  where
    centro' = sumP (- (tam estado) / 2, - (tam estado) / 2) $ centro estado

-- | Función para modificar un estado cuando se hace click
modificaEvent :: Estado e => Event -> e -> e
modificaEvent (EventKey (MouseButton LeftButton) Up _ point) = modifica point
modificaEvent _ = id

-- | Función IO para jugar en modo multijugador
guiMulti ::
  Estado e =>
  -- | Estado inicial
  e ->
  IO ()
guiMulti e =
  play
    (ventana $ tam e)
    (fondo $ tema e)
    15
    e
    dibuja'
    modificaEvent
    (const id)

-- | Función IO para jugar contra un agente
--
-- __Nota:__ Debido a que la librería /gloss/ es algo limitada hemos tenido que
-- usar de una manera un poco cutre la función 'play'.
guiAgente ::
  (Juego j p c, Estado e) =>
  -- | Función que modifica un estado
  (Agente j -> Ficha -> Float -> e -> e) ->
  -- | Estado inicial
  e ->
  -- | 'Ficha' del 'Agente'
  Ficha ->
  -- | 'Agente' contra el que jugar
  Agente j ->
  IO ()
guiAgente modificaAgente e f a =
  play
    (ventana $ tam e)
    (fondo $ tema e)
    15
    e
    dibuja'
    modificaEvent
    $ modificaAgente a f

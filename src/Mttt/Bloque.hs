{-
Module      : Mttt.Bloque
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementación del juego /tres en raya/.
-}

module Mttt.Bloque (
    Ficha(X,O)
  , esX 
  , Bloque 
  , showBloque 
  , putBloque 
  , movimientoBloque
  , bloqueVacio
  , bloqueEjemplo
) where

import Mttt.Utils 

import Data.Array
import Data.List (intersperse, transpose)
import Data.Maybe (isJust, isNothing, fromJust)

-- | Tipo que representa una ficha del juego
data Ficha = X -- ^ Cruz. Ficha del primer jugador
           | O -- ^ Circulo. Ficha del segundo jugador
    deriving (Eq, Read, Enum)

instance Show Ficha where
    show X = "✗"
    show O = "○"

-- | Determina si una 'Ficha' es X ('True') o O ('False'=
esX :: Ficha -> Bool
esX X = True
esX _ = False

-- | Tipo para un tablero de /tres en raya/
type Bloque = Array Pos (Maybe Ficha)

-- | Utilidad para imprimir una /casilla/ de un 'Bloque' en pantalla
showMaybeFicha :: Maybe Ficha -> Char
showMaybeFicha Nothing = '_'
showMaybeFicha (Just f)  = head (show f)

-- | Representación en caracteres de un 'Bloque'
showBloque :: Bloque -> String
showBloque b = unlines [intersperse ' ' [showMaybeFicha $ b!(x, y) | y <- [1..3]] | x <- [1..3]] 

-- | Utilidad para imprimir en pantalla un 'Bloque'
putBloque :: Bloque -> IO ()
putBloque = putStr . showBloque

-- | 'Bloque' vacío
bloqueVacio :: Bloque
bloqueVacio = listArray ((1,1),(3,3)) [Nothing | _ <- listaIndices]

-- | Insertar una ficha nueva en un 'Bloque'.
-- Si el movimiento es válido se devuelve un 'Just Bloque'.
-- En caso contrario se devuelve 'Nothing' 
movimientoBloque :: Bloque
                  -> Ficha -- ^ Ficha a añadir
                  -> Pos   -- ^ Posición en la que se añade la ficha
                  -> Maybe Bloque
movimientoBloque b f (x,y)
  | (isNothing (b!(x,y))) && turnoBloque b == f = Just (b // [((x,y), Just f)])
  | otherwise                                   = Nothing

-- | Hace lo mismo que 'movimientoBloque' salvo que en vez de
-- devolver un 'Maybe Bloque' devuelve directamente un 'Bloque'.
-- Si la jugada no es correcta se lanza un error.
-- Esta función debe ser utilizada únicamente cuando está garantizado
-- de antemano que la jugada es válida.
movimientoBloque' :: Bloque
                  -> Ficha -- ^ Ficha a añadir
                  -> Pos   -- ^ Posición en la que se añade la ficha
                  -> Bloque
movimientoBloque' b f p = fromJust $ movimientoBloque b f p

-- | Lista de posiciones vacías de un 'Bloque'
casillasLibresBloque :: Bloque -> [Pos]
casillasLibresBloque b = map fst $ filter snd [((x, y), isNothing $ b!(x,y)) | (x,y) <- listaIndices]

-- | Posibles jugadas
expandirBloque :: Bloque -> [Bloque]
expandirBloque t
  | finBloque t = []
  | otherwise   = map (movimientoBloque' t f) $ casillasLibresBloque t
  where f = turnoBloque t

-- | Cuenta las fichas de cada tipo que hay en un 'Bloque'.
contarFichasBloque :: Bloque
              -> (Int, Int) -- ^ El primer valor corresponde al número de X's y el segundo a las O's
contarFichasBloque b = foldr1 suma $ map f $ elems b
                       where f Nothing = (0,0)
                             f (Just X) = (1,0)
                             f (Just O) = (0,1)
                             suma (a,b) (c, d) = (a+c, b+d)

-- | Devuelve las filas de un 'Bloque'
filasBloque :: Bloque -> [[Maybe Ficha]]
filasBloque b = [[b!(x, y)| x <- [1..3]] | y <- [1..3]]

-- | Devuelve las columnas de un tablero de tres en raya
columnasBloque :: Bloque -> [[Maybe Ficha]]
columnasBloque = transpose . filasBloque

-- | Devuelve las diagonales de un tablero de tres en raya
diagonalesBloque :: Bloque -> [[Maybe Ficha]]
diagonalesBloque b = [[b!(x,x)| x<-[1..3]],[b!(x, 4-x) |x<-[1..3]]]

-- | Devuelve todas las lineas rectas de un tablero de tres en raya
lineasBloque :: Bloque-> [[Maybe Ficha]]
lineasBloque = concat . sequence [filasBloque, columnasBloque, diagonalesBloque]
-- | Determina a quien le toca
turnoBloque :: Bloque -> Ficha
turnoBloque b
  | (xs - os) == 1 = O
  | (xs - os) == 0 = X
  where (xs, os) = contarFichasBloque b

-- | Determina quien ha ganado la partida de tres en raya, si es que alguien ha ganado.
ganadorBloque :: Bloque -> Maybe Ficha
ganadorBloque t
  | Just X `elem` ganadores = Just X
  | Just O `elem` ganadores = Just O
  | otherwise = Nothing
  where ganadores = map ganadorLinea $ lineasBloque t
        ganadorLinea l
          | l == [Just X, Just X, Just X] = Just X
          | l == [Just O, Just O, Just O] = Just O
          | otherwise = Nothing

-- | Determina si la partida ha acabado o no
finBloque :: Bloque -> Bool
finBloque t
  | isJust (ganadorBloque t) = True
  | tablasBloque t           = True
  | otherwise                = False


-- | Determina si la partida de tres en raya ha acabado en tablas.
tablasBloque :: Bloque -> Bool
tablasBloque b = notElem Nothing b && isNothing (ganadorBloque b)

-- | Determina si una posición de un tablero de tres en raya es válida o no.
--
-- __TODO:__ Completar; Tener en cuenta que la partida debe acabar cuando alguien gana, por lo que no se puede ganar dos veces la partida.
validarBloque:: Bloque -> Bool
validarBloque b = (xs - os) == 0 || (xs - os) == 1
    where (xs, os) = contarFichasBloque b

-- | Implementación del algoritmo minimax que recorre el árbol hasta las hojas.
-- minimaxBloqueDeep :: ([Int] -> Int) -> ([a] -> a) -> Bloque -> a
-- minimaxBloqueDeep peor mejor tablero
--   | finBloque tablero = heurBloque tablero
--   | otherwise    = mejor (map (minimaxBloqueDeep mejor peor) siguientes)
--   where siguientes = expandirBloque tablero

minimaxBloqueDeep = minimax 10 expandirBloque heurBloque

-- | Función heurística para el tres en raya
-- Toma el valor 0 si nadie ha ganado, 1 si ganan X y -1 si gana O.
heurBloque :: Bloque -> Int
heurBloque b
  | ganador == Just X = 1
  | ganador == Just O = -1
  | otherwise = 0
  where ganador = ganadorBloque b

-- | Utilidad para imprimir lineas de un tablero de tres en raya
showLinea :: [Maybe Ficha] -> String
showLinea = intersperse ' ' . map showMaybeFicha 

-- | Ejemplo de un posible 'Bloque'
bloqueEjemplo :: Bloque
bloqueEjemplo =  movimientoBloque' (
                     movimientoBloque' (
                       movimientoBloque' (
                         movimientoBloque' bloqueVacio X (2,2)
                       ) O (1,1)
                     ) X (2,1)
                   ) O (2,3)

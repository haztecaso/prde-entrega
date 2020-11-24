{-|
Module      : Mttt
Description : Meta tres en raya
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementaciones de los juegos /tres en raya/ y /meta tres en raya/.
-}

module Mttt (
  -- * Tipos
    Ficha
  , TableroP
  , Tablero
  -- * Algunos tableros
  , tableroPVacio
  , tableroVacio
) where

import Data.Array
import Data.List (intersperse, replicate, transpose)
import Data.Maybe (isJust, isNothing)

-- | Tipo que representa una ficha del juego
data Ficha =  X -- ^ Cruz. Es la ficha que utiliza el primer jugador
           | O  -- ^ Circulo. Ficha del segundo jugador
    deriving (Eq, Read, Enum)

instance Show Ficha where
    show X = "✗"
    show O = "○"

-- | Utilidad para imprimir una casilla del tablero en pantalla
maybeFichaToChar :: Maybe Ficha -> Char
maybeFichaToChar Nothing = '_'
maybeFichaToChar (Just f)  = head (show f)

-- | Tipo para un tablero de /tres en raya/
newtype TableroP = Tp (Array (Int,Int) (Maybe Ficha))
  deriving Eq 

instance Show TableroP where
  show (Tp arr) = unlines [intersperse ' ' [maybeFichaToChar $ arr!(x, y) | y <- [1..3]] | x <- [1..3]] 

-- | Tipo para un tablero de /meta tres en raya/
newtype Tablero = T (Array (Int,Int) TableroP)
  deriving Eq 

instance Show Tablero where
  show _ = ""
showTablero (T arr) = insertCol (1,1) col
    where insertCol t = zipWith (++) $ map showLinea $ lineasP $ arr!t 
          col = replicate 3 " |"

-- | Devuelve el array interno de un TableroP
fromTableroP :: TableroP -> Array (Int, Int) (Maybe Ficha)
fromTableroP (Tp arr) = arr

-- | Devuelve el array interno de un Tablero
fromTablero :: Tablero -> Array (Int, Int) TableroP
fromTablero (T arr) = arr

-- | Lista de indices de un Tablero o TableroP
indicesTablero :: [(Int, Int)]
indicesTablero = [(x, y) | x <- [1..3], y <- [1..3]]

-- | Tablero de tres en raya vacío
tableroPVacio :: TableroP
tableroPVacio = Tp (listArray ((1,1),(3,3)) [Nothing | _ <- indicesTablero])

-- | Tablero de meta tres en raya vacío
tableroVacio :: Tablero
tableroVacio = T (listArray ((1,1),(3,3)) [tableroPVacio | _ <- indicesTablero])

-- | Insertar (o modificar) una ficha en un 'TableroP'
ponerFichaP :: TableroP
            -> (Int,Int) -- ^ Posición en la que se añade la ficha
            -> Ficha     -- ^ Ficha a añadir
            -> TableroP
ponerFichaP (Tp arr) (x,y) f = Tp (arr // [((x,y), Just f)])

-- | Lista de posiciones vacías de un 'TableroP'
casillasLibresP :: TableroP -> [(Int, Int)]
casillasLibresP (Tp arr) = map fst $ filter snd [((x, y), isNothing $ arr!(x,y)) | (x,y) <- indicesTablero]

-- | Posibles jugadas
expandirP :: TableroP -> [TableroP]
expandirP t
  | finP t    = []
  | otherwise = map (\x -> ponerFichaP t x f) $ casillasLibresP t
  where f = turnoP t

-- | Cuenta las fichas de cada tipo que hay en un tablero de tres en raya.
contarFichasP :: TableroP
              -> (Int, Int) -- ^ El primer valor corresponde al número de X's y el segundo a las O's
contarFichasP (Tp arr) = foldr1 suma $ map f $ elems arr
                       where f Nothing = (0,0)
                             f (Just X) = (1,0)
                             f (Just O) = (0,1)
                             suma (a,b) (c, d) = (a+c, b+d)

-- | Devuelve las filas de un tablero de tres en raya
filasP :: TableroP -> [[Maybe Ficha]]
filasP (Tp arr) = [[arr!(x, y)| x <- [1..3]] | y <- [1..3]]

-- | Devuelve las columnas de un tablero de tres en raya
columnasP :: TableroP -> [[Maybe Ficha]]
columnasP = transpose . filasP

-- | Devuelve las diagonales de un tablero de tres en raya
diagonalesP :: TableroP -> [[Maybe Ficha]]
diagonalesP (Tp arr) = [[arr!(x,x)| x<-[1..3]],[arr!(x, 4-x) |x<-[1..3]]]

-- | Devuelve todas las lineas rectas de un tablero de tres en raya
lineasP :: TableroP -> [[Maybe Ficha]]
lineasP = concat . sequence [filasP, columnasP, diagonalesP]
-- | Determina a quien le toca
turnoP :: TableroP -> Ficha
turnoP t
  | (xs - os) == 1 = O
  | (xs - os) == 0 = X
  where (xs, os) = contarFichasP t

-- | Determina quien ha ganado la partida de tres en raya, si es que alguien ha ganado.
ganadorP :: TableroP -> Maybe Ficha
ganadorP t
  | Just X `elem` ganadores = Just X
  | Just O `elem` ganadores = Just O
  | otherwise = Nothing
  where ganadores = map ganadorLinea $ lineasP t
        ganadorLinea l
          | l == [Just X, Just X, Just X] = Just X
          | l == [Just O, Just O, Just O] = Just O
          | otherwise = Nothing

-- | Determina si la partida ha acabado o no
finP :: TableroP -> Bool
finP t
  | isJust (ganadorP t) = True
  | tablasP t           = True
  | otherwise           = False


-- | Determina si la partida de tres en raya ha acabado en tablas.
tablasP :: TableroP -> Bool
tablasP t@(Tp arr) = notElem Nothing arr && isNothing (ganadorP t)

-- | Determina si una posición de un tablero de tres en raya es válida o no.
--
-- __TODO:__ Completar; Tener en cuenta que la partida debe acabar cuando alguien gana, por lo que no se puede ganar dos veces la partida.
validarTableroP:: TableroP -> Bool
validarTableroP t = (xs - os) == 0 || (xs - os) == 1
    where (xs, os) = contarFichasP t

-- | Implementación del algoritmo minimax que recorre el árbol hasta las hojas.
-- minimaxPDeep :: ([Int] -> Int) -> ([a] -> a) -> TableroP -> a
-- minimaxPDeep peor mejor tablero
--   | finP tablero = heurPDeep tablero
--   | otherwise    = mejor (map (minimaxPDeep mejor peor) siguientes)
--   where siguientes = expandirP tablero

minimaxPDeep = minimaxMain 10 expandirP heurP

-- | Parte /interna/ del algoritmo minimax visto en clase
minimax :: Ord b => Int -> (a->[a]) -> (a->b) -> ([b]->b) -> ([b]->b) -> a -> b
minimax prof expandir evaluar peor mejor prob
  | (prof == 0) || null siguientes = evaluar prob
  | otherwise = mejor (map (minimax (prof-1) expandir evaluar mejor peor) siguientes)
  where siguientes = expandir prob

-- | Parte /externa/ del algoritmo minimax visto en clase
minimaxMain :: Ord b => Int -> (a->[a]) -> (a-> b) -> a -> a
minimaxMain prof expandir evaluar prob
  | (prof==0) || null siguientes = prob
  | otherwise = snd (maximum' sigVals) 
  where siguientes   = expandir prob
        valoraciones = map (minimax (prof-1) expandir evaluar maximum minimum) siguientes
        sigVals      = zip valoraciones siguientes
        maximum'     = foldr1 max'
        max' a@(x,_) b@(y,_)
          | x >= y = a
          | otherwise = b

-- | Función heurística para el tres en raya
-- Toma el valor 0 si nadie ha ganado, 1 si ganan X y -1 si gana O.
heurP :: Num a => TableroP -> a
heurP t
  | ganador == Just X = 1
  | ganador == Just O = -1
  | otherwise = 0
  where ganador = ganadorP t

-- | Utilidad para imprimir lineas de un tablero de tres en raya
showLinea :: [Maybe Ficha] -> String
showLinea = intersperse ' ' . map maybeFichaToChar

-- | Ejemplo de un posible tablero de tres en raya
ejemploTableroP :: TableroP
ejemploTableroP =  ponerFichaP (
                     ponerFichaP (
                       ponerFichaP (
                         ponerFichaP tableroPVacio (2,2) X
                       ) (1,1) O
                     ) (2,1) X
                   ) (3,3) O

module Mttt
  ( tableroPVacio
  , ponerFichaP
  , tableroVacio
) where

import Data.Array
import Data.List (intersperse, replicate)

data Ficha =  X | O
    deriving (Eq, Read, Enum)

instance Show Ficha where
    show X = "✗"
    show O = "○"

maybeFichaToChar :: Maybe Ficha -> Char
maybeFichaToChar Nothing = '_'
maybeFichaToChar (Just f)  = head (show f)

newtype TableroP = Tp (Array (Int,Int) (Maybe Ficha))
  deriving Eq 

instance Show TableroP where
  show (Tp arr) = init $ unlines [intersperse ' ' [maybeFichaToChar $ arr!(x, y) | x <- [1..3]] | y <- [1..3]] 

newtype Tablero = T (Array (Int,Int) TableroP)
  deriving Eq 

instance Show Tablero where
  show _ = ""
showTablero (T arr) = insertCol (1,1) col
    where insertCol t = zipWith (++) $ map showLinea $ lineasP $ arr!t 
          col = replicate 3 " |"


fromTableroP :: TableroP -> Array (Int, Int) (Maybe Ficha)
fromTableroP (Tp arr) = arr

fromTablero :: Tablero -> Array (Int, Int) TableroP
fromTablero (T arr) = arr

tableroPIndices :: [(Int, Int)]
tableroPIndices = [(x, y) | x <- [1..3], y <- [1..3]]

tableroPVacio :: TableroP
tableroPVacio = Tp (listArray ((1,1),(3,3)) [Nothing | _ <- [1..9]])

tableroVacio :: Tablero
tableroVacio = T (listArray ((1,1),(3,3)) [tableroPVacio | _ <- [1..9]])

ponerFichaP :: TableroP -> (Int,Int) -> Ficha -> TableroP
ponerFichaP (Tp arr) (x,y) f = Tp (arr // [((x,y), Just f)])

contarFichas :: TableroP -> (Int, Int)
contarFichas (Tp arr) = foldr1 suma $ map f $ elems arr
                       where f Nothing = (0,0)
                             f (Just X) = (1,0)
                             f (Just O) = (0,1)
                             suma (a,b) (c, d) = (a+c, b+d)

validarTableroP:: TableroP -> Bool
validarTableroP t = (xs - os) == 0 || (xs - os) == 1
    where (xs, os) = contarFichas t

turnoP :: TableroP -> Ficha
turnoP t
  | (xs - os) == 1 = O
  | (xs - os) == 0 = X
  where (xs, os) = contarFichas t

filasP :: TableroP -> [[Maybe Ficha]]
filasP (Tp arr) = [[arr!(x, y)| x <- [1..3]] | y <- [1..3]]

columnasP :: TableroP -> [[Maybe Ficha]]
columnasP (Tp arr) = [[arr!(x, y) | y <- [1..3]] | x <- [1..3]]

diagonalesP :: TableroP -> [[Maybe Ficha]]
diagonalesP (Tp arr) = [[arr!(x,x)| x<-[1..3]],[arr!(x, 4-x) |x<-[1..3]]]

lineasP :: TableroP -> [[Maybe Ficha]]
lineasP = concat . sequence [filasP, columnasP, diagonalesP]

showLinea = intersperse ' ' . map maybeFichaToChar

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

ejemploTableroP :: TableroP
ejemploTableroP = ponerFichaP (
                   ponerFichaP (
                     ponerFichaP (
                       ponerFichaP (
                         ponerFichaP tableroPVacio (2,2) X
                       ) (1,1) O
                     ) (2,1) X
                   ) (3,3) O
                 ) (2,3) X

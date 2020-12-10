{-
Module      : Mttt.Tablero.Data
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementación del juego /meta tres en raya/.
-}

module Mttt.Tablero.Data (
     Tablero(bloques, bloqueActivo)
   , showTablero
   , putTablero
   , tableroVacio 
   , turnoTablero
   , ganadorTablero
                         , lineasTablero
) where

import Mttt.Common.Utils
import Mttt.Bloque.Data

import Data.List (transpose, intersperse)
import Data.Array (Array, (!), (//), listArray, elems)
import Data.Maybe (isJust, isNothing, fromJust)

-- | Tipo para un tablero de /meta tres en raya/
data Tablero = T { bloques :: Array Pos Bloque -- ^ Bloques del tablero
                 , bloqueActivo :: Maybe Pos   -- ^ 'Pos' del 'Bloque' activo para jugar.
                                               -- Si es 'Nothing' se puede jugar en
                                               -- cualquier 'Bloque'.
                 } deriving (Eq, Show, Read)

-- | Representación en caracteres de un 'Tablero'
showTablero :: Tablero -> String
showTablero t = showTablero' t 1 ++ "──────┼───────┼──────\n" 
             ++ showTablero' t 2 ++ "──────┼───────┼──────\n" 
             ++ showTablero' t 3

-- | Función auxiliar para imprimir una linea de un tablero
showTablero' :: Tablero -> Int -> String
showTablero' t n = unlines $ map (concat . intersperse " │ ")
                 $ transpose [lines $ showBloque $ bs ! (n,i) | i<-[1..3]]
  where bs = bloques t

-- | Utilidad para imprimir en pantalla un 'Tablero'
putTablero :: Tablero -> IO ()
putTablero = putStr . showTablero

-- | 'Tablero' vacío
tableroVacio :: Tablero
tableroVacio =
  T { bloques = listArray ((1,1),(3,3)) [bloqueVacio | _ <- listaIndices]
    , bloqueActivo = Nothing 
    }

-- | Cuenta las fichas de cada tipo que hay en un 'Tablero'.
contarFichasTablero :: Tablero
              -> (Int, Int) -- ^ El primer valor corresponde al número de X's y el segundo a las O's
contarFichasTablero t = foldr1 suma [ contarFichasBloque $ (bloques t) ! i | i <- listaIndices]
                       where suma (a,b) (c, d) = (a+c, b+d)

-- | Devuelve todas las lineas rectas de un 'Tablero'
lineasTablero :: Tablero -> [[Bloque]]
lineasTablero t = filas ++ columnas ++ diagonales
  where filas      = [[(bloques t)!(x, y)| x <- [1..3]] | y <- [1..3]]
        columnas   = transpose filas
        diagonales = [[(bloques t)!(x,x)| x<-[1..3]],[(bloques t)!(x, 4-x) |x<-[1..3]]]

-- | Determina quien ha ganado la partida si es que alguien ha ganado.
ganadorTablero :: Tablero -> Maybe Ficha
ganadorTablero t
  | Just X `elem` ganadores = Just X
  | Just O `elem` ganadores = Just O
  | otherwise = Nothing
    where ganadores = map ganadorLinea $ lineasTablero t
          ganadorLinea l
            | map ganadorBloque l == [Just X, Just X, Just X] = Just X
            | map ganadorBloque l == [Just O, Just O, Just O] = Just O
            | otherwise = Nothing

-- | Determina a quien le toca
turnoTablero :: Tablero -> Maybe Ficha
turnoTablero t
  | (isNothing $ ganadorTablero t) && (xs - os) == 1 = Just O
  | (isNothing $ ganadorTablero t) && (xs - os) == 0 = Just X
  | otherwise = Nothing
  where (xs, os) = contarFichasTablero t

-- | Insertar una ficha nueva en un 'Tablero'. La función hace uso de
-- 'turnoTablero' para decidir que 'Ficha' colocar.
--
-- Si el movimiento es válido se devuelve un 'Just Tablero'.
-- En caso contrario se devuelve 'Nothing' 
    {- TODO
movTablero :: Tablero
           -> Pos   -- ^ 'Bloque' en que jugar
           -> Pos   -- ^ Posición del 'Bloque' seleccionado donde poner la ficha
           -> Maybe Tablero
movTablero t p1 p2
  | Just p1 == bloqueActivo t = 
  | otherwise = error "F"
  where bloque = 
-}

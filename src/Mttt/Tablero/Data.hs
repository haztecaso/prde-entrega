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
   , showTablero'
   , putTablero
   , tableroVacio 
) where

import Mttt.Common.Utils
import Mttt.Bloque.Data

import Data.List (transpose, intersperse)
import Data.Array

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

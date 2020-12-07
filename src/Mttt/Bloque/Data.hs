{-
Module      : Mttt.Bloque.Data
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Implementación del juego /tres en raya/.
-}

module Mttt.Bloque.Data (
    Ficha(X,O) -- TODO: Es inofensivo exportar los constructores?¿
  , esX 
  , Bloque 
  , showBloque 
  , showListaPosBloque
  , putBloque 
  , turnoBloque
  , movBloque 
  , casillasLibresBloque
  , ganadorBloque
  , finBloque
  , tablasBloque
  , bloqueVacio
  , AgenteBloque (funAB, nombreAB)
  , agenteBTonto
  , agenteBMinimaxHeur1
) where

import Mttt.Common.Utils 

import Data.Array
import Data.List (intersperse, transpose, elemIndex)
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

-- | Dibujo de un 'Bloque' resaltando unas 'Pos'.
showListaPosBloque :: Bloque -> [Pos] -> String
showListaPosBloque b ps = unlines [intersperse ' ' [casilla (x, y) | y <- [1..3]] | x <- [1..3]] 
  where
    casilla pos 
      | isJust (b!pos) = showMaybeFicha $ b!pos
      | otherwise      = head $ show $ pos2int pos

-- | Utilidad para imprimir en pantalla un 'Bloque'
putBloque :: Bloque -> IO ()
putBloque = putStr . showBloque

-- | 'Bloque' vacío
bloqueVacio :: Bloque
bloqueVacio = listArray ((1,1),(3,3)) [Nothing | _ <- listaIndices]

-- | Insertar una ficha nueva en un 'Bloque'. La función hace uso de
-- 'turnoBloque' para decidir que 'Ficha' colocar.
--
-- Si el movimiento es válido se devuelve un 'Just Bloque'.
-- En caso contrario se devuelve 'Nothing' 
movBloque :: Bloque
                  -> Pos   -- ^ Posición en la que se añade la ficha
                  -> Maybe Bloque
movBloque b (x,y)
  | ((x,y) `elem` listaIndices)
    && (isNothing (b!(x,y)))
    && isJust (turnoBloque b) = Just (b // [((x,y), turnoBloque b)])
  | otherwise                  = Nothing

-- | Lista de posiciones vacías de un 'Bloque'
casillasLibresBloque :: Bloque -> [Pos]
casillasLibresBloque b = map fst $ filter snd [((x, y), isNothing $ b!(x,y)) | (x,y) <- listaIndices]

-- | Posibles jugadas
expandirBloque :: Bloque -> [Bloque]
expandirBloque b
  | finBloque b = []
  | otherwise = map (fromJust . ( movBloque b )) $ casillasLibresBloque b

-- | Movimiento de bloque expandido
posMovimientoBloque :: Bloque -> Bloque -> Pos
posMovimientoBloque bloque expandido =
  libres !! (fromJust $ elemIndex expandido siguientes)
  where siguientes = expandirBloque bloque
        libres = casillasLibresBloque bloque


-- | Cuenta las fichas de cada tipo que hay en un 'Bloque'.
contarFichasBloque :: Bloque
              -> (Int, Int) -- ^ El primer valor corresponde al número de X's y el segundo a las O's
contarFichasBloque b = foldr1 suma $ map f $ elems b
                       where f Nothing = (0,0)
                             f (Just X) = (1,0)
                             f (Just O) = (0,1)
                             suma (a,b) (c, d) = (a+c, b+d)

-- | Determina a quien le toca
turnoBloque :: Bloque -> Maybe Ficha
turnoBloque b
  | (isNothing $ ganadorBloque b) && (xs - os) == 1 = Just O
  | (isNothing $ ganadorBloque b) && (xs - os) == 0 = Just X
  | otherwise = Nothing
  where (xs, os) = contarFichasBloque b

-- | Determina si la partida ha acabado o no
finBloque :: Bloque -> Bool
finBloque t
  | isJust (ganadorBloque t) = True
  | tablasBloque t           = True
  | otherwise                = False

-- | Determina si la partida de tres en raya ha acabado en tablas.
tablasBloque :: Bloque -> Bool
tablasBloque b = notElem Nothing b && isNothing (ganadorBloque b)


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

{- FUNCIONES HEURÍSTICAS -}

-- | Función heurística para el tres en raya
-- Toma el valor 0 si nadie ha ganado, 1 si ganan X y -1 si gana O.
heurBloque :: Bloque -> Int
heurBloque b
  | ganador == Just X = -1
  | ganador == Just O = 1
  | otherwise = 0
  where ganador = ganadorBloque b

{- AGENTES -}

-- | Tipo para Agentes de 'Bloque'.
data AgenteBloque = AgenteBloque { funAB :: Bloque -> Pos
                                 , nombreAB :: String
                                 }

-- | Agente que devuelve la primera posición disponible donde jugar,
-- en el orden generado por 'casillasLibresBloque'.
agenteBTonto :: AgenteBloque
agenteBTonto = AgenteBloque { funAB = head . casillasLibresBloque
                           , nombreAB = "tonto"
                           }

agenteBMinimaxHeur1 :: AgenteBloque
agenteBMinimaxHeur1 = AgenteBloque { funAB = \b -> posMovimientoBloque b (minimax 9 expandirBloque heurBloque b)
                                   , nombreAB = "minimax-heur1"
                                   }


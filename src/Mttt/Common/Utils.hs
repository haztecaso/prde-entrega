{-
Module      : Mttt.Utils
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Utilidades
-}

module Mttt.Common.Utils
  ( Jugador (Persona, Agente),
    Pos,
    listaIndices,
    pos2int,
    int2pos,
    prompt,
    selOpcion,
    minimax,
  )
where

import System.IO (hFlush, stdout)

-- | Tipo para diferenciar entre personas y agentes inteligentes
data Jugador = Persona | Agente deriving (Read, Show)

-- | Tipo sinónimo para representar posiciones en los tableros.
-- Puede ser utilizado tanto para 'Tablero' como para 'Bloque'
type Pos = (Int, Int)

-- | Lista de indices de un 'Tablero' o 'Bloque'
listaIndices :: [(Int, Int)]
listaIndices = [(x, y) | x <- [1 .. 3], y <- [1 .. 3]]

pos2int :: Pos -> Int
pos2int (x, y) = (y -1) + 3 * (x -1)

int2pos :: Int -> Pos
int2pos n = (n `div` 3 + 1, n `mod` 3 + 1)

{- IO -}

-- | Utilidad para pedir y recibir valores a un usuario
-- Copiada de https://stackoverflow.com/questions/13190314/
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- | Función que formatea e imprime una lista de opciones
putOps :: [String] -> IO ()
putOps xs = putOps' xs 0

putOps' :: [String] -> Int -> IO ()
putOps' [] _ = return ()
putOps' (x : xs) n = do
  putStrLn $ show n ++ ": " ++ x
  putOps' xs (n + 1)

-- | Seleccionar una opción
selOpcion :: [String] -> IO Int
selOpcion ops = do
  putOps ops
  n <- prompt "Selecciona una opción: "
  if (read n >= 0) && (read n < length ops)
    then return (read n)
    else selOpcion ops

{- ALGORITMOS -}

-- | Parte /interna/ del algoritmo minimax visto en clase
-- __TODO__: REVISAR!!!
minimaxInt :: Ord b => Int -> (a -> [a]) -> (a -> b) -> ([b] -> b) -> ([b] -> b) -> a -> b
minimaxInt prof expandir evaluar peor mejor prob
  | (prof == 0) || null siguientes = evaluar prob
  | otherwise = mejor (map (minimaxInt (prof -1) expandir evaluar mejor peor) siguientes)
  where
    siguientes = expandir prob

-- | Parte /externa/ del algoritmo minimax visto en clase
-- __TODO__: REVISAR!!!
minimax :: Ord b => Int -> (a -> [a]) -> (a -> b) -> a -> a
minimax prof expandir evaluar prob
  | (prof == 0) || null siguientes = prob
  | otherwise = snd (maximum' sigVals)
  where
    siguientes = expandir prob
    valoraciones = map (minimaxInt (prof -1) expandir evaluar maximum minimum) siguientes
    sigVals = zip valoraciones siguientes
    maximum' = foldr1 max'
    max' a@(x, _) b@(y, _)
      | x >= y = a
      | otherwise = b

{-
Module      : Mttt.Utils
Copyright   : (c) Adrián Lattes y David Diez
License     : GPL-3
Stability   : experimental

Utilidades
-}

module Mttt.Utils (
  Pos
  , listaIndices
  , minimax
) where

-- | Tipo sinónimo para representar posiciones en los tableros.
-- Puede ser utilizado tanto para 'Tablero' como para 'Bloque'
type Pos = (Int, Int)

-- | Lista de indices de un 'Tablero' o 'Bloque'
listaIndices :: [(Int, Int)]
listaIndices = [(x, y) | x <- [1..3], y <- [1..3]]

-- | Parte /interna/ del algoritmo minimax visto en clase
-- __TODO__: REVISAR!!!
minimaxInt :: Ord b => Int -> (a->[a]) -> (a->b) -> ([b]->b) -> ([b]->b) -> a -> b
minimaxInt prof expandir evaluar peor mejor prob
  | (prof == 0) || null siguientes = evaluar prob
  | otherwise = mejor (map (minimaxInt (prof-1) expandir evaluar mejor peor) siguientes)
  where siguientes = expandir prob

-- | Parte /externa/ del algoritmo minimax visto en clase
-- __TODO__: REVISAR!!!
minimax :: Ord b => Int -> (a->[a]) -> (a-> b) -> a -> a
minimax prof expandir evaluar prob
  | (prof==0) || null siguientes = prob
  | otherwise = snd (maximum' sigVals) 
  where siguientes   = expandir prob
        valoraciones = map (minimaxInt (prof-1) expandir evaluar maximum minimum) siguientes
        sigVals      = zip valoraciones siguientes
        maximum'     = foldr1 max'
        max' a@(x,_) b@(y,_)
          | x >= y = a
          | otherwise = b

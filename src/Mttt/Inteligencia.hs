-- |
-- Module      : Mttt.Inteligencia
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Funciones para implementar inteligencias artificiales
module Mttt.Inteligencia (minimax) where

-- | Parte /interna/ del algoritmo minimax visto en clase
minimaxInt :: Ord b => Int -> (a -> [a]) -> (a -> b) -> ([b] -> b) -> ([b] -> b) -> a -> b
minimaxInt prof expandir evaluar peor mejor prob
  | (prof == 0) || null siguientes = evaluar prob
  | otherwise = mejor (map (minimaxInt (prof -1) expandir evaluar mejor peor) siguientes)
  where
    siguientes = expandir prob

-- | Parte /externa/ del algoritmo minimax visto en clase
minimax ::
  Ord b =>
  -- | Profundidad de búsqueda
  Int ->
  -- | Función  de expansión. Dado un estado devuelve todos los posibles
  -- estados siguientes.
  (a -> [a]) ->
  -- | Función heurística. Dado un estado devuelve una puntuación asociada a tal
  -- estado.
  (a -> b) ->
  -- | Estado de entrada
  a ->
  a
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

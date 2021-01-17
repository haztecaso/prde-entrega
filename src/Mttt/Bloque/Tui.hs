-- |
-- Module      : Mttt.Bloque.Tui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Interfaz de texto del /tres en raya/.
module Mttt.Bloque.Tui where

import Data.List (intersperse)
import Data.Maybe (fromJust, isJust)
import Mttt.Bloque.Data
import Mttt.Common.Data
import Mttt.Common.Tui
import Mttt.Common.Utils

{-
  BLOQUE: Generalidades
-}

-- | Dibujo de un 'Bloque' resaltando unas 'Pos'.
showListaPosBloque :: Bloque -> [Pos] -> String
showListaPosBloque b ps = unlines [intersperse ' ' [casilla (x, y) | y <- [1 .. 3]] | x <- [1 .. 3]]
  where
    casilla pos
      | isJust (casillaBloque b pos) = showMaybeFicha $ casillaBloque b pos
      | otherwise = head $ show $ pos2int pos

-- | Pregunta donde se quiere jugar.
-- | TODO: arreglar
preguntarMovB :: Bloque -> IO Pos
preguntarMovB b = do
  putStr $ showListaPosBloque b $ casillasLibresBloque b
  let jugador = fromJust $ turnoBloque b -- Atención: fromJust puede lanzar errores! Usar esta función con cuidado...
  putStr $ "\n[Turno de " ++ show jugador ++ "] "
  n <- prompt "Número de casilla donde jugar: "
  return (int2pos $ read n)

-- | Modifica un 'Bloque' insertando una ficha si es posible.
jugarBloque :: Bloque -> Pos -> IO Bloque
jugarBloque b pos = do
  let nuevo = movBloque b pos
  maybe (putStrLn "¡Movimiento incorrecto!" >> return b) return nuevo

{-
  BLOQUE: Multijugador
-}

loopBPartidaMulti ::
  -- | 'Bloque' actual
  Bloque ->
  -- | Lista de jugadas (ordenadas empezando por la más reciente)
  [Pos] ->
  IO (Bloque, [Pos])
loopBPartidaMulti b jugadas = do
  pos <- preguntarMovB b
  nuevo <- jugarBloque b pos
  if finBloque nuevo
    then return (nuevo, jugadas)
    else loopBPartidaMulti nuevo $ pos : jugadas

bloqueMsgMulti :: Bloque -> String
bloqueMsgMulti b
  | tablasBloque b = "Tablas"
  | isJust $ ganadorBloque b = show (fromJust $ ganadorBloque b) ++ " ha ganado"
  | otherwise = "Partida en curso"

-- | TODO: revisar
tuiBloqueMulti :: IO ()
tuiBloqueMulti = do
  putStrLn "Nueva partida de tres en raya [Multijugador]"
  (b, partida) <- loopBPartidaMulti bloqueVacio []
  putStr "Jugadas: "
  print $ reverse partida
  print b
  putStrLn $ "\n[FIN] " ++ bloqueMsgMulti b

{-
  BLOQUE: Agente
-}

loopBPartidaAgente ::
  -- | 'Bloque' actual
  Bloque ->
  -- | 'AgenteBloque' contra el que jugar
  AgenteBloque ->
  -- | 'Ficha' del agente
  Ficha ->
  -- | Lista de jugadas (ordenadas empezando por la más reciente)
  [Pos] ->
  IO (Bloque, [Pos])
loopBPartidaAgente b agente fichaAgente jugadas = do
  pos <-
    if turnoBloque b == Just fichaAgente
      then return (funAB agente b)
      else preguntarMovB b
  nuevo <- jugarBloque b pos
  if finBloque nuevo
    then return (nuevo, jugadas)
    else loopBPartidaAgente nuevo agente fichaAgente (pos : jugadas)

{-
bloqueMsgAgente :: Bloque -> Ficha -> String
bloqueMsgAgente b fichaAgente
  | tablasBloque b = "Tablas"
  | isJust $ ganadorBloque b
  && Just fichaAgente == fromJust $ ganadorBloque b
    = "El agente ha ganado"
  | isJust $ ganadorBloque b
  && Just fichaAgente /= fromJust $ ganadorBloque b
    = "Has vencido al agente"
  | otherwise = "Partida en curso"
-}

tuiBloqueAgente ::
  AgenteBloque ->
  -- | 'Ficha' del 'AgenteBloque'
  Ficha ->
  IO ()
tuiBloqueAgente agente fichaAgente = do
  putStrLn "Nueva partida de tres en raya"
  putStrLn $ "Jugando contra agente " ++ nombreAB agente
  (b, partida) <- loopBPartidaAgente bloqueVacio agente fichaAgente []
  print $ reverse partida
  print b

-- print $ "\n[FIN] " ++ bloqueMsgAgente b fichaAgente

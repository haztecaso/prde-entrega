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
    if turno b == Just fichaAgente
      then return (funAB agente b)
      else preguntarMovB b
  nuevo <- jugarBloque b pos
  if fin nuevo
    then return (nuevo, jugadas)
    else loopBPartidaAgente nuevo agente fichaAgente (pos : jugadas)

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

-- (b, partida) <- loopAgente bloqueVacio agente fichaAgente []

-- print $ "\n[FIN] " ++ bloqueMsgAgente b fichaAgente

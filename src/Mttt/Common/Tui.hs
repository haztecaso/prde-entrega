-- |
-- Module      : Mttt.Common.Tui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Definiciones generales de la intefaz de texto.
module Mttt.Common.Tui where

import Data.List (intersperse)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Mttt.Common.Data
import System.IO (hFlush, stdout)

-- | Utilidad para evitar los errores generados por 'read'.
-- Copiada de https://stackoverflow.com/questions/5121371/#answer-5121537
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

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
getOpcion :: [String] -> IO Int
getOpcion ops = do
  putOps ops
  n <- prompt "Selecciona una opción: "
  if (read n >= 0) && (read n < length ops)
    then return (read n)
    else getOpcion ops

printCasillasLibres :: Juego juego p c => juego -> IO ()
printCasillasLibres j =
  putStrLn
    ( "Casillas donde se puede jugar: "
        ++ (concat $ intersperse " - " $ map (tail . init) [show c | c <- casillasLibres j])
    )

-- | Pregunta donde se quiere jugar.
preguntarJugada :: Juego juego pos c => juego -> IO pos
preguntarJugada j = do
  let jugador = fromJust $ turno j -- Atención: este fromJust no debería causar problemas
  putStr $ "[Turno de " ++ show jugador ++ "] "
  posStr <- prompt "Posición donde jugar: "
  let pos = maybeRead $ '(' : posStr ++ ")"
  maybe (putStrLn "¡Posición incorrecta!" >> preguntarJugada j) return pos

-- | Modifica un juego insertando una ficha si es posible.
jugar :: Juego juego pos c => juego -> IO (juego, pos)
jugar j = do
  print j
  printCasillasLibres j
  pos <- preguntarJugada j
  let intento = mov j pos
  if isJust intento
    then return (fromJust intento, pos)
    else do
      putStrLn "¡Movimiento incorrecto!"
      jugar j

-- | Loop para jugar una partida en modo multijugador.
loopMulti ::
  Juego j p c =>
  j ->
  -- | Lista de jugadas ordenadas de la más reciente a la más antigua
  [p] ->
  IO (j, [p])
loopMulti juego jugadas = do
  (siguiente, pos) <- jugar juego
  if fin siguiente
    then return (siguiente, pos : jugadas)
    else loopMulti siguiente $ pos : jugadas

-- | Mensaje que enseñar al final de la partida.
mensajeFin :: Juego j p c => j -> String
mensajeFin j
  | tablas j = "Tablas."
  | isJust $ ganador j = show (fromJust $ ganador j) ++ " ha ganado."
  | otherwise = "Algo va mal: la partida no ha acabado todavía."

-- | Función IO para jugar una partida en modo multijugador.
tuiMulti :: Juego j p c => j -> IO ()
tuiMulti j = do
  (final, jugadas) <- loopMulti j []
  print final
  putStrLn $ "\n[FIN] " ++ mensajeFin final
  putStrLn "Lista de jugadas: "
  print $ reverse jugadas

-- | Loop para jugar una partida contra un agente.
loopAgente ::
  Juego j p c =>
  -- | 'Agente' contra el que jugar
  Agente j ->
  -- | 'Ficha' del agente
  Ficha ->
  j ->
  -- | Lista de jugadas ordenadas de la más reciente a la más antigua
  [p] ->
  IO (j, [p])
loopAgente agente ficha juego jugadas = do
  (siguiente, pos) <-
    if turno juego == Just ficha
      then return (juego', mov2pos juego juego')
      else jugar juego
  if fin siguiente
    then return (siguiente, pos : jugadas)
    else loopMulti siguiente $ pos : jugadas
  where
    juego' = f agente juego

-- | Mensaje que enseñar al final de una partida contra un agente.
mensajeFinAgente ::
  Juego j p c =>
  -- | Juego final
  j ->
  -- | 'Ficha' del agente
  Ficha ->
  String
mensajeFinAgente j f
  | tablas j = "Tablas."
  | isJust $ ganador j = mensajeGanador (fromJust $ ganador j)
  | otherwise = "Algo va mal: la partida no ha acabado todavía."
  where
    mensajeGanador ficha
      | f == ficha = "El agente te ha ganado."
      | otherwise = "Has ganado al agente."

-- | Función IO para jugar una partida contra un agente.
tuiAgente ::
  Juego j p c =>
  -- | 'Agente' contra el que jugar
  Agente j ->
  -- | 'Ficha' del agente
  Ficha ->
  j ->
  IO ()
tuiAgente agente ficha j = do
  (final, jugadas) <- loopAgente agente ficha j []
  print final
  putStrLn $ "\n[FIN] " ++ mensajeFinAgente final ficha
  putStrLn "Lista de jugadas: "
  print $ reverse jugadas

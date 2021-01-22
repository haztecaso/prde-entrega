-- |
-- Module      : Mttt
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Módulo de alto nivel útil para lanzar los juegos en sus distintas
-- modalidades.
module Mttt (nuevoJuego) where

import           Mttt.Bloque      (Bloque)
import qualified Mttt.Bloque      as B (heur0)
import           Mttt.Common
import           Mttt.Gui.Bloque
import           Mttt.Gui.Common
import           Mttt.Gui.Tablero
import           Mttt.Tablero     (Tablero)
import qualified Mttt.Tablero     as T (heur0)
import           Mttt.Tui

getHeurTablero :: IO (String, Tablero -> Int)
getHeurTablero = do
  putStrLn "Selecciona una función heurística"
  opc <- getOpcion ["heur0"]
  case opc of
    0 -> return ("heur0", T.heur0)

getProf :: IO (Int)
getProf = do
  prof <- prompt "Profundidad para el minimax: "
  maybe
    (repetir)
    (\n -> if n < 2 then repetir else return n)
    (maybeRead prof)
  where
    m = "¡Profundidad incorrecta! La profundidad tiene que ser un entero > 1."
    repetir = putStrLn m >> getProf

-- | Útil para cuando se definan más funciones heurísticas
getAgenteTablero :: IO (Ficha -> Agente Tablero)
getAgenteTablero = do
  (nombreHeur, heur) <- getHeurTablero
  prof <- getProf
  return (\f -> agenteMinimax f nombreHeur heur expandir prof)

-- | Función para lanzar un juego
nuevoJuego ::
  -- | ¿Jugar a /ttt/? No: /mttt/
  Bool ->
  -- | ¿Usar tui? No: /gui/
  Bool ->
  -- | ¿Multijugador? No: /agente/
  Bool ->
  -- | Ficha del Agente
  Ficha ->
  IO ()
nuevoJuego True True True _ = tuiMulti (vacio :: Bloque)
nuevoJuego True True False p = tuiAgente (agenteMinimax p "heur0" B.heur0 expandir 9) p (vacio :: Bloque)
nuevoJuego True False True _ = guiMulti (inicial 350 temaOscuro :: EstadoBloque)
nuevoJuego True False False p = guiAgente (inicial 350 temaOscuro :: EstadoBloque) p (agenteMinimax p "heur0" B.heur0 expandir 9)
nuevoJuego False False True _ = guiMulti (inicial 350 temaOscuro :: EstadoTablero)
nuevoJuego False False False p = guiAgente (inicial 350 temaOscuro :: EstadoTablero) p (agenteMinimax p "heur0" T.heur0 expandir 5)
nuevoJuego False True True _ = tuiMulti (vacio :: Tablero)
nuevoJuego False True False p = tuiAgente (agenteMinimax p "heur0" T.heur0 expandir 5) p (vacio :: Tablero)

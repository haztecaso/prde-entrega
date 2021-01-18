module Main where

import Mttt.Bloque.Data (Bloque, bloqueVacio)
import qualified Mttt.Bloque.Data as B (heur0)
import Mttt.Bloque.Gui
import Mttt.Common.Data
import Mttt.Common.Gui
import Mttt.Common.Tui
import Mttt.Common.Utils
import Mttt.Tablero.Data (Tablero, tableroVacio)
import qualified Mttt.Tablero.Data as T (heur0)
import Mttt.Tablero.Gui
import System.Console.ParseArgs

-- | Tipo de dato para diferenciar las opciones de la interfaz cli
data Opciones = OAyuda | OTui | OSimple | OMulti | OPrimero
  deriving
    ( Eq,
      Ord,
      Show
    )

-- | Detalles de las opciones de la interfaz cli
argd :: [Arg Opciones]
argd =
  [ Arg
      { argIndex = OAyuda,
        argName = Just "help",
        argAbbr = Just 'h',
        argData = Nothing,
        argDesc = "Lista de opciones para la interfaz cli"
      },
    Arg
      { argIndex = OTui,
        argName = Just "tui",
        argAbbr = Just 't',
        argData = Nothing,
        argDesc = "Interfaz de texto"
      },
    Arg
      { argIndex = OSimple,
        argName = Just "simple",
        argAbbr = Just 's',
        argData = Nothing,
        argDesc = "Jugar al tres en raya"
      },
    Arg
      { argIndex = OMulti,
        argName = Just "multi",
        argAbbr = Just 'm',
        argData = Nothing,
        argDesc = "Jugar en modo multijugador"
      },
    Arg
      { argIndex = OPrimero,
        argName = Just "primero",
        argAbbr = Just 'p',
        argData = Nothing,
        argDesc = "Dejar que empiece el agente"
      }
  ]

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

getAgenteTablero :: IO (Ficha -> Agente Tablero)
getAgenteTablero = do
  (nombreHeur, heur) <- getHeurTablero
  prof <- getProf
  return (\f -> agenteMinimax f nombreHeur heur expandir prof)

-- | Función para seleccionar que debe hacer este programa
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
nuevoJuego True True True _ = tuiMulti bloqueVacio
nuevoJuego True True False p = tuiAgente (agenteMinimax p "heur0" B.heur0 expandir 9) p bloqueVacio
nuevoJuego True False True _ = guiMulti $ estadoBloqueInicial 350 temaOscuro
nuevoJuego True False False p = guiAgenteBloque (estadoBloqueInicial 350 temaOscuro) p (agenteMinimax p "heur1" B.heur0 expandir 9)
nuevoJuego False False True _ = guiMulti $ estadoTableroInicial 350 temaOscuro
nuevoJuego False True True _ = tuiMulti tableroVacio
nuevoJuego _ _ _ _ = putStrLn "¡¡¡PENDIENTE DE IMPLEMENTAR!!!"

main :: IO ()
main = do
  -- guiBloque temaOscuro 250
  args <-
    parseArgsIO
      (ArgsParseControl ArgsComplete ArgsHardDash)
      argd
  let ayuda = gotArg args OAyuda
  let simple = gotArg args OSimple
  let tui = gotArg args OTui
  let multi = gotArg args OMulti
  let fichaAgente =
        if gotArg args OPrimero
          then X
          else O
  if ayuda
    then putStr $ argsUsage args
    else nuevoJuego simple tui multi fichaAgente

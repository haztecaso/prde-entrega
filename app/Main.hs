module Main where

import Mttt.Bloque.Data
import Mttt.Bloque.Gui
import Mttt.Bloque.Tui
import Mttt.Common.Data
import Mttt.Common.Gui
import Mttt.Common.Tui
import Mttt.Common.Utils
import Mttt.Tablero.Data
import Mttt.Tablero.Gui
import Mttt.Tablero.Tui
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

selAgenteBloque :: IO AgenteBloque
selAgenteBloque = do
  putStrLn "¿Contra que agente quieres jugar?"
  opc <- selOpcion ["Agente tonto", "minimax"]
  case opc of
    0 -> return agenteBTonto
    1 -> return agenteBMinimax

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
nuevoJuego True True True _ = tuiBloqueMulti
nuevoJuego True True False p = do
  tuiBloqueAgente agenteBMinimax p
nuevoJuego True False True _ = guiMulti $ estadoBloqueInicial 350 temaOscuro
nuevoJuego True False False p = do
  guiBloqueAgente temaOscuro 350 agenteBMinimax p
nuevoJuego False False True _ = guiMulti $ estadoTableroInicial 350 temaOscuro
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

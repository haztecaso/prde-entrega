module Main where
import System.Console.ParseArgs

import Mttt.Common.Utils
import Mttt.Common.Gui
import Mttt.Common.Tui

import Mttt.Bloque.Data
import Mttt.Bloque.Tui
import Mttt.Bloque.Gui

import Mttt.Tablero.Data
import Mttt.Tablero.Tui
import Mttt.Tablero.Gui

-- | Tipo de dato para diferenciar las opciones de la interfaz cli
data Opciones =
  OAyuda  |
  OTui    |
  OSimple |
  OMulti  |
  OPrimero
  deriving (Ord, Eq, Show)

-- | Detalles de las opciones de la interfaz cli
argd :: [Arg Opciones]
argd = [ Arg { argIndex = OAyuda
             , argName  = Just "help"
             , argAbbr  = Just 'h'
             , argData  = Nothing
             , argDesc  = "Lista de opciones para la interfaz cli"
             }
       , Arg { argIndex = OTui
             , argName  = Just "tui"
             , argAbbr  = Just 't'
             , argData  = Nothing
             , argDesc  = "Interfaz de texto"
             }
       , Arg { argIndex = OSimple
             , argName  = Just "simple"
             , argAbbr  = Just 's'
             , argData  = Nothing
             , argDesc  = "Jugar al tres en raya"
             }
       , Arg { argIndex = OMulti
             , argName  = Just "multi"
             , argAbbr  = Just 'm'
             , argData  = Nothing
             , argDesc  = "Jugar en modo multijugador"
             }
       , Arg { argIndex = OPrimero
             , argName  = Just "primero"
             , argAbbr  = Just 'p'
             , argData  = Nothing
             , argDesc  = "Dejar que empiece el agente"
             }
       ]

selAgenteBloque :: IO AgenteBloque
selAgenteBloque = do putStrLn "¿Contra que agente quieres jugar?"
                     opc <- selOpcion ["Agente tonto", "minimax"]
                     case opc of
                       0 -> return agenteBTonto
                       1 -> return agenteBMinimaxHeur1

-- | Función para seleccionar que debe hacer este programa
nuevoJuego :: Bool -- ^ ¿Jugar a /ttt/? No: /mttt/
           -> Bool -- ^ ¿Usar tui? No: /gui/
           -> Bool -- ^ ¿Multijugador? No: /agente/
           -> Ficha -- ^ Ficha del Agente
           -> IO ()
nuevoJuego True True True _   = tuiBloqueMulti
nuevoJuego True True False p  = do agente <- selAgenteBloque
                                   tuiBloqueAgente agente p
nuevoJuego True False True _  = guiBloqueMulti temaOscuro 450
nuevoJuego True False False p = do agente <- selAgenteBloque
                                   guiBloqueAgente temaOscuro 350 agente p
nuevoJuego False False True _ = guiTableroMulti temaOscuro 350
nuevoJuego _ _ _ _            = putStrLn "¡¡¡PENDIENTE DE IMPLEMENTAR!!!"

main :: IO ()
main = do
  --guiBloque temaOscuro 250
  args <- parseArgsIO
            (ArgsParseControl ArgsComplete ArgsHardDash)
            argd
  let ayuda   = gotArg args OAyuda
  let simple  = gotArg args OSimple
  let tui     = gotArg args OTui
  let multi   = gotArg args OMulti
  let fichaAgente = if gotArg args OPrimero
                       then X
                       else O
  if ayuda
     then putStr $ argsUsage args
     else nuevoJuego simple tui multi fichaAgente

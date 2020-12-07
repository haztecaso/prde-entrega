module Main where

import Mttt
import System.Console.ParseArgs

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

-- | Función para seleccionar que debe hacer este programa
nuevoJuego :: Bool -- ^ ¿Jugar a /ttt/? No: /mttt/
           -> Bool -- ^ ¿Usar tui? No: /gui/
           -> Bool -- ^ ¿Multijugador? No: /agente/
           -> IO ()
nuevoJuego True True True = tuiBloqueMulti
-- nuevoJuego True True False = tuiBloqueAgente agenteTonto
nuevoJuego True False True = guiBloque temaOscuro 450
nuevoJuego False False True = guiTablero temaOscuro 450
nuevoJuego _ _ _= putStrLn "[PENDIENTE DE IMPLEMENTAR]"

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
  let primero = gotArg args OPrimero
  print (simple, tui, multi)
  if ayuda
     then putStr $ argsUsage args
     else nuevoJuego simple tui multi

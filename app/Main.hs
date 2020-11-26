module Main where

import Mttt

verBloque :: Bloque -> IO ()
verBloque bloque = do
  putBloque bloque
  displayEB (crearEB bloque (0,0) 600 temaOscuro)

main :: IO ()
main = do
  verBloque bloqueEjemplo
  guiBoard temaOscuro 600

-- |
-- Module      : Mttt.Common.Tui
-- Copyright   : (c) Adrián Lattes y David Diez
-- License     : GPL-3
-- Stability   : experimental
--
-- Definiciones generales de la intefaz de texto.
module Mttt.Common.Tui where

import System.IO (hFlush, stdout)

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
selOpcion :: [String] -> IO Int
selOpcion ops = do
  putOps ops
  n <- prompt "Selecciona una opción: "
  if (read n >= 0) && (read n < length ops)
    then return (read n)
    else selOpcion ops

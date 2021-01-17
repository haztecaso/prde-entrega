import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Interact
import Mttt.Common.Data (Pos)
import Mttt.Common.Gui
import Mttt.Tablero.Gui

testTableroModifica :: IO ()
testTableroModifica = do
  guiMultiIO (estadoTableroInicial 700 temaOscuro)

dibujaIO :: Estado e => e -> IO Picture
dibujaIO e = return (dibuja' e)

-- | Dada una posición del puntero y un 'EstadoTablero' devuelve las
-- 'Pos' del 'Bloque' y casilla donde está el puntero.
pointPos' ::
  -- | Posición del puntero en la pantalla
  Point ->
  -- | Tamaño
  Float ->
  -- | Posición del puntero en el 'bloqueEB'
  (Pos, Pos)
pointPos' p t =
  ( posBloque,
    pointPos p (t / 3 * 0.8) (centroBloque)
  )
  where
    posBloque = pointPos p t (0, 0)
    centroBloque = sumP (posPoint (t / 3) posBloque) (- t / 2, - t / 2)

modificaEventIO :: Estado e => Event -> e -> IO e
modificaEventIO (EventKey (MouseButton LeftButton) Up _ p) e = do
  putStrLn (show p ++ " --> " ++ show (pointPos' p $ tam e))
  return (modifica p e)
modificaEventIO _ e = return e

guiMultiIO :: Estado e => e -> IO ()
guiMultiIO e = playIO (ventana $ tam e) (fondo $ tema e) 15 e dibujaIO modificaEventIO (\_ -> return)

main :: IO ()
main = testTableroModifica

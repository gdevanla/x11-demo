module Main1 where

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Data.Bits

mkUnmanagedWindow :: Display
  -> Screen
  -> Window
  -> Position -> Position
  -> Dimension -> Dimension
  -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect .|. cWBorderPixel .|. cWBackPixel
  win <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    set_background_pixel attributes $ whitePixel dpy (defaultScreen dpy)
    set_border_pixel attributes $ blackPixel dpy (defaultScreen dpy)
    createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes
  return win

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
      --border = blackPixel dpy dflt
      --background = whitePixel dpy dflt
  rootw <- rootWindow dpy dflt
  -- win <- createSimpleWindow dpy rootw 0 0 100 100 1 border background
  win <- mkUnmanagedWindow dpy scr rootw 0 0 1000 1000
  setTextProperty dpy win "Hello World" wM_NAME
  mapWindow dpy win
  sync dpy False
  threadDelay (10 * 10000000)
  exitWith ExitSuccess

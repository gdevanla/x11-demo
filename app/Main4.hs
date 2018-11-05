module Main4 where

-- draw rectangle demo

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Data.Bits


drawInWindow :: Display -> Window -> IO ()
drawInWindow dpy win = do
  fgcolor <-  initColor dpy "blue"
  gc <- createGC dpy win
  setForeground dpy gc fgcolor
  fillRectangle dpy win gc 25 25 90 90
  freeGC dpy gc


initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (approx, _) <- allocNamedColor dpy colormap color
  return $ color_pixel approx

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
    -- set_background_pixel attributes $ whitePixel dpy (defaultScreen dpy)
    background_color <- initColor dpy "red"
    set_background_pixel attributes $ background_color
    -- set_border_pixel attributes $ blackPixel dpy (defaultScreen dpy)
    border_color <- initColor dpy "blue"
    set_border_pixel attributes $ border_color
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
  win <- mkUnmanagedWindow dpy scr rootw 50 50 200 200
  setTextProperty dpy win "Hello World-Main4" wM_NAME
  mapWindow dpy win
  drawInWindow dpy win
  sync dpy False
  threadDelay (10 * 1000000)
  exitWith ExitSuccess

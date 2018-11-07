module Main where

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

data Ptr1 a

newtype XEvent1 = XEvent1 XEventPtr1  --  XEvent1 (Ptr1 XEvent1)

type XEventPtr1 = Ptr1 XEvent1


-- getInt:: XEvent1
-- getInt = XEvent1 (Ptr1 )


mkUnmanagedWindow :: Display
  -> Screen
  -> Window
  -> Position -> Position
  -> Dimension -> Dimension
  -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect
  win <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes
  return win

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      border = blackPixel dpy dflt
      background = whitePixel dpy dflt
  rootw <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 50 50 100 100 1 border background
  setTextProperty dpy win "Main-Hello World" wM_NAME
  mapWindow dpy win
  sync dpy False
  threadDelay (10 * 10000000)
  exitWith ExitSuccess

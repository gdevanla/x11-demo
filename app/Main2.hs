module Main2 where

import System.Environment
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras -- where changeWindowAttributes
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

usage :: [Char] -> [Char]
usage pn = "Usage: " ++ pn ++ " manage/unmanage windowID"

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  let (win, action) = case args of
        [] -> error $ usage pn
        w -> case (w !! 0) of
          "manage" -> (window w, False)
          "unmange" -> (window w, True)
          _ -> error $ usage pn
        where
          window w1 = case (w1 !! 1) of
                     [] -> error $ usage pn
                     w -> read w::Window
  dpy <- openDisplay ""
  unmapWindow dpy win
  sync dpy False
  allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes action
    set_background_pixel attributes $ blackPixel dpy (defaultScreen dpy)
    changeWindowAttributes dpy win (cWOverrideRedirect .|. cWBackPixel) attributes
    mapWindow dpy win
    sync dpy False

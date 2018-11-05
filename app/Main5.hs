module Main5 where

-- draw rectangle demo

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Data.Bits
import Data.Time

drawInWindow :: Display -> Window -> String -> IO ()
drawInWindow dpy win str = do
  bgColor <- initColor dpy "green"
  fgColor <- initColor dpy "blue"
  gc <- createGC dpy win
  fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
  setForeground dpy gc bgColor
  fillRectangle dpy win gc 0 0 200 100
  setForeground dpy gc fgColor
  fillRectangle dpy win gc 0 0 200 100
  printString dpy win gc fontStruc str
  freeGC dpy gc
  freeFont dpy fontStruc

printString :: Display -> Drawable -> GC -> FontStruct -> String -> IO ()
printString dpy win gc fontStruc str = do
  let strLen = textWidth fontStruc str
      (_, asc, _, _) = textExtents fontStruc str
      valign = (100 + fromIntegral asc) `div` 2
      remWidth = 200 - strLen
      offset = remWidth `div` 2
  fgcolor <- initColor dpy "white"
  bgcolor <- initColor dpy "blue"
  setForeground dpy gc fgcolor
  setBackground dpy gc bgcolor
  drawImageString dpy win gc offset valign str


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

-- date :: IO String
-- date = do
--   --t <- toCalendarTime =<< getClockTime
--   t <- (getClockTime >>= toCalendarTime)
--   return $ calendarTimeToString t

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
      --border = blackPixel dpy dflt
      --background = whitePixel dpy dflt
  rootw <- rootWindow dpy dflt
  -- win <- createSimpleWindow dpy rootw 0 0 100 100 1 border background
  win <- mkUnmanagedWindow dpy scr rootw 50 50 250 250
  setTextProperty dpy win "Hello World-Main4" wM_NAME
  mapWindow dpy win
  t <- getCurrentTime
  drawInWindow dpy win $ show t
  sync dpy False
  threadDelay (10 * 1000000)
  exitWith ExitSuccess

module Main7 where

-- draw rectangle demo

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Concurrent (threadDelay, forkIO)
import Data.Bits
import Data.Time


drawInWindow :: Display -> Window -> String -> IO ()
drawInWindow dpy win str = do
  bgColor <- initColor dpy "green"
  fgColor <- initColor dpy "blue"
  gc <- createGC dpy win
  fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
  p <- createPixmap dpy win 200 100 (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
  --let p = win
  setForeground dpy gc bgColor
  fillRectangle dpy p gc 0 0 200 100
  setForeground dpy gc fgColor
  fillRectangle dpy p gc 0 0 200 100
  printString dpy p gc fontStruc str
  copyArea dpy p win gc 0 0 200 100 0 0
  freeGC dpy gc
  freeFont dpy fontStruc
  freePixmap dpy p

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

updateWin :: Display -> Window -> IO ()
updateWin dpy win = do
  t <- getCurrentTime
  drawInWindow dpy win $ show t
  sync dpy True
  allocaXEvent $ \e -> do
    nextEvent dpy e
    e1 <- get_EventType e
    putStrLn $ show e1
  updateWin dpy win

sendExposeEvent :: Display -> Window -> IO ()
sendExposeEvent dpy w =
  do threadDelay (10 * 1000000)
     putStrLn "Inside send expose event"
     allocaXEvent $ \e -> do
       setEventType e expose
       sendEvent dpy w False noEventMask e
     sync dpy False
     sendExposeEvent dpy w

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
  selectInput dpy win (exposureMask .|. buttonPressMask)
  mapWindow dpy win
  _ <- forkIO $ sendExposeEvent dpy win
  updateWin dpy win

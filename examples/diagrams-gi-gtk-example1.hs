{- | A first example of drawing diagrams from within GTK.  This
     program draws a Koch snowflake with the depth controllable
     via a GTK widget.
-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
module Main where

import Control.Monad
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Diagrams.Prelude hiding (set)
import Diagrams.Size (requiredScaling)
import Diagrams.Backend.GIGtk
import Diagrams.Backend.Cairo (Cairo)
import qualified Data.Colour as C
import Data.Text (Text)
import qualified Data.Text as T

hilbert :: Int -> Diagram Cairo
hilbert = frame 1 . lw medium . lc (colors!!1) . strokeT . hilbert'
  where
    hilbert' :: Int -> Trail V2 Double
    hilbert' 0 = mempty
    hilbert' n =
        hilbert'' (n-1) # reflectY <> vrule 1
        <> hilbert'  (n-1) <> hrule 1
        <> hilbert'  (n-1) <> vrule (-1)
        <> hilbert'' (n-1) # reflectX
      where
        hilbert'' :: Int -> Trail V2 Double
        hilbert'' m = hilbert' m # rotateBy (1/4)

-- Our drawing code, copied from
-- projects.haskell.org/diagrams/gallery/Pentaflake.html
colors ::[Colour Double]
colors = iterate (C.blend 0.1 white) red

p ::Diagram Cairo
p = regPoly 5 1 # lwO 0

-- | create a snowflake diagram of depth @n@
--
-- specifying a type here because the monoidal query type needs to be specified
-- for @drawToGtk@, otherwise get a "No instance for (PathLike ..." error.
pentaflake :: Int -> Diagram Cairo
pentaflake 0 = p
pentaflake n = appends (p' # fc (colors !! (n-1)))
                       (zip vs (repeat (rotateBy (1/2) p')))
  where vs = take 5 . iterate (rotateBy (1/5))
                    . (if odd n then negated else id) $ unitY
        p' = pentaflake (n-1)

pentaflake' ::Int -> Diagram Cairo
pentaflake' n = pentaflake n # fc (colors !! n)

-- end of diagrams code

-- A function to set up the main window and signal handlers
createMainWindow :: IO Gtk.Window
createMainWindow = do
    win <- new Gtk.Window []

    on win #keyPressEvent $ \event -> do
        name <- event `get` #keyval >>= Gdk.keyvalName
        when (name == Just "Escape") Gtk.mainQuit
        return False

    depthWidget <- Gtk.spinButtonNewWithRange 1 10 1
    -- when the spinButton changes, redraw the window
    on depthWidget #valueChanged $ do
        Gtk.widgetQueueDraw win --drawArea
        return ()

    rbHilbert <- Gtk.radioButtonNewWithLabelFromWidget Gtk.noRadioButton "Hilbert"
    set rbHilbert [#active := True]
    rbPentaFlake <- Gtk.radioButtonNewWithLabelFromWidget (Just rbHilbert) "Penta Flake"
    set rbPentaFlake [#active := False]
    boxRB <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxPackStart boxRB rbHilbert False False 0
    Gtk.boxPackStart boxRB rbPentaFlake False False 0

    drawArea <- new Gtk.DrawingArea [#widthRequest := 512, #heightRequest := 512]

    -- add the depthWidget control and drawArea to the main window
    hbox <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxPackStart hbox boxRB False False 0 -- box child expand fill extraPadding
    Gtk.boxPackStart hbox depthWidget False False 0 -- box child expand fill extraPadding
    Gtk.boxPackStart hbox drawArea True True 0
    #add win hbox

    on rbHilbert #toggled $ do
        Gtk.widgetQueueDraw drawArea
        hilbertActive <- get rbHilbert #active
        putStrLn $ "Hilbert curve is: "++(show hilbertActive)
        return ()

    on rbPentaFlake #toggled $ do
        pentaFlakeActive <- get rbPentaFlake #active
        putStrLn $ "Penta flake is: "++(show pentaFlakeActive)
        return ()

    -- handle the drawArea's @onExpose@ signal.  We provide a function
    -- that takes an area marked as dirty and redraws it.
    -- This program simply redraws the entire drawArea.
    --
    -- Many gtk signal handlers return True if the signal was handled, and False
    -- otherwise (in which case the signal will be propagated to the parent).
    on drawArea #draw $ \context -> do
        rect <- Gtk.widgetGetAllocation drawArea  -- size in pixels (Int)
        canvasX <- get rect #width
        canvasY <- get rect #height
        curDepth <- fromIntegral <$> Gtk.spinButtonGetValueAsInt depthWidget
        hilbertActive <- get rbHilbert #active
        let dia = if hilbertActive then hilbert curDepth else pentaflake curDepth
            w = width dia
            h = height dia
            spec = mkSizeSpec2D (Just $ fromIntegral canvasX) (Just $ fromIntegral canvasY)
            scaledDia = toGtkCoords $ transform (requiredScaling spec (V2 w h)) dia
        renderToGtk context scaledDia
        return True

    return win

-- Gtk application
--
-- Initialize the library, create and show the main window,
-- finally enter the main loop
main :: IO ()
main = do
    Gtk.init Nothing
    win <- createMainWindow
    on win #destroy Gtk.mainQuit
    Gtk.widgetShowAll win
    Gtk.main


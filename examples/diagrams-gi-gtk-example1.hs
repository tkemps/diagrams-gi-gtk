{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A first example of drawing diagrams from within GTK.  This
--     program draws a Koch snowflake with the depth controllable
--     via a GTK widget.
module Main where

import qualified Data.Colour as C
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.GIGtk ( defaultRender )
import Diagrams.Prelude
import GI.Gtk (AttrOp (..))
import qualified GI.Gtk as Gtk

-- The classic Hilbert curves from the diagrams gallery:
hilbert :: Int -> Diagram Cairo
hilbert = frame 1 . lw medium . lc (colors !! 1) . strokeT . hilbert'
  where
    hilbert' :: Int -> Trail V2 Double
    hilbert' 0 = mempty
    hilbert' n =
      hilbert'' (n - 1) # reflectY <> vrule 1
        <> hilbert' (n - 1)
        <> hrule 1
        <> hilbert' (n - 1)
        <> vrule (-1)
        <> hilbert'' (n - 1) # reflectX
      where
        hilbert'' :: Int -> Trail V2 Double
        hilbert'' m = hilbert' m # rotateBy (1 / 4)

-- Some more drawing code, copied from
-- projects.haskell.org/diagrams/gallery/Pentaflake.html
colors :: [Colour Double]
colors = iterate (C.blend 0.1 white) red

p :: Diagram Cairo
p = regPoly 5 1 # lwO 0

-- | create a snowflake diagram of depth @n@
--
-- specifying a type here because the monoidal query type needs to be specified
-- for @drawToGtk@, otherwise get a "No instance for (PathLike ..." error.
pentaflake :: Int -> Diagram Cairo
pentaflake 0 = p
pentaflake n =
  appends
    (p' # fc (colors !! (n - 1)))
    (zip vs (repeat (rotateBy (1 / 2) p')))
  where
    vs =
      take 5 . iterate (rotateBy (1 / 5))
        . (if odd n then negated else id)
        $ unitY
    p' = pentaflake (n - 1)

pentaflake' :: Int -> Diagram Cairo
pentaflake' n = pentaflake n # fc (colors !! n)

-- Set up the application window and add the diagrams
activate :: Gtk.Application -> IO ()
activate app = do
  win <- Gtk.new Gtk.ApplicationWindow [#application := app, #title := "Diagrams GI-GTK Example"]

  depthWidget <- Gtk.spinButtonNewWithRange 1 10 1
  -- when the spinButton changes, redraw the window
  Gtk.on depthWidget #valueChanged $ do
    Gtk.widgetQueueDraw win -- drawArea
    return ()

  rbHilbert <- Gtk.new Gtk.CheckButton [#label := "Hilbert", #active := True]
  rbPentaFlake <- Gtk.new Gtk.CheckButton [#label := "Penta Flake", #active := False, #group := rbHilbert]
  radioButtons <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationVertical]

  #append radioButtons rbHilbert
  #append radioButtons rbPentaFlake

  drawArea <- Gtk.new Gtk.DrawingArea [#widthRequest := 512, #heightRequest := 512]

  hbox <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationVertical]

  #append hbox radioButtons
  #append hbox depthWidget
  #append hbox drawArea

  Gtk.windowSetChild win (Just hbox)

  -- Redraw on changes to input parameters of the diagram
  Gtk.on rbHilbert #toggled $ Gtk.widgetQueueDraw drawArea
  Gtk.on depthWidget #changed $ Gtk.widgetQueueDraw drawArea

  Gtk.drawingAreaSetDrawFunc drawArea $
    Just $ \_ context width height -> do
      curDepth <- fromIntegral <$> Gtk.spinButtonGetValueAsInt depthWidget
      hilbertActive <- Gtk.get rbHilbert #active
      let diagram = if hilbertActive then hilbert curDepth else pentaflake curDepth
      defaultRender True diagram context width height

  #show win

-- Gtk application
--
-- Start the GTK application
main :: IO ()
main = do
  app <-
    Gtk.new
      Gtk.Application
      [ #applicationId Gtk.:= "Diagrams GI-GTK Example",
        Gtk.On #activate (activate ?self)
      ]

  #run app Nothing
  pure ()

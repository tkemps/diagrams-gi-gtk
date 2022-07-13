{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Diagrams.Backend.Gtk
-- Copyright   :  (c) 2019 Torsten Kemps-Benedix
-- License     :  MIT-style (see LICENSE)
-- Maintainer  :  tkx68@icloud.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Convenient interface to rendering diagrams directly
-- on Gtk DrawingArea widgets using the Cairo backend. This package uses Cairo
-- double buffering (see <https://developer.gnome.org/gtk3/stable/chap-drawing-model.html#double-buffering>).
--
-- See the following example for a practical use case. Have a close look at the
-- use of 'renderToGtk' in the `on drawArea #draw` code block. See
-- <https://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects-Widget.html#g:47>
-- for details on the draw signal or
-- <https://developer.gnome.org/gtk3/stable/GtkWidget.html#GtkWidget-draw>
-- for the original GTK3 documentation.
--
-- @
-- {-# LANGUAGE ImplicitParams #-}
-- {-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
--
-- -- | A first example of drawing diagrams from within GTK.  This
-- --     program draws a Koch snowflake with the depth controllable
-- --     via a GTK widget.
-- module Main where
--
-- import qualified Data.Colour as C
-- import Diagrams.Backend.Cairo (Cairo)
-- import Diagrams.Backend.GIGtk ( defaultRender )
-- import Diagrams.Prelude
-- import GI.Gtk (AttrOp (..))
-- import GI.Gtk qualified as Gtk
--
-- -- The classic Hilbert curves from the diagrams gallery:
-- hilbert :: Int -> Diagram Cairo
-- hilbert = frame 1 . lw medium . lc (colors !! 1) . strokeT . hilbert'
--   where
--     hilbert' :: Int -> Trail V2 Double
--     hilbert' 0 = mempty
--     hilbert' n =
--       hilbert'' (n - 1) # reflectY <> vrule 1
--         <> hilbert' (n - 1)
--         <> hrule 1
--         <> hilbert' (n - 1)
--         <> vrule (-1)
--         <> hilbert'' (n - 1) # reflectX
--       where
--         hilbert'' :: Int -> Trail V2 Double
--         hilbert'' m = hilbert' m # rotateBy (1 / 4)
--
-- -- Some more drawing code, copied from
-- -- projects.haskell.org/diagrams/gallery/Pentaflake.html
-- colors :: [Colour Double]
-- colors = iterate (C.blend 0.1 white) red
--
-- p :: Diagram Cairo
-- p = regPoly 5 1 # lwO 0
--
-- -- | create a snowflake diagram of depth @n@
-- --
-- -- specifying a type here because the monoidal query type needs to be specified
-- -- for @drawToGtk@, otherwise get a "No instance for (PathLike ..." error.
-- pentaflake :: Int -> Diagram Cairo
-- pentaflake 0 = p
-- pentaflake n =
--   appends
--     (p' # fc (colors !! (n - 1)))
--     (zip vs (repeat (rotateBy (1 / 2) p')))
--   where
--     vs =
--       take 5 . iterate (rotateBy (1 / 5))
--         . (if odd n then negated else id)
--         $ unitY
--     p' = pentaflake (n - 1)
--
-- pentaflake' :: Int -> Diagram Cairo
-- pentaflake' n = pentaflake n # fc (colors !! n)
--
-- -- Set up the application window and add the diagrams
-- activate :: Gtk.Application -> IO ()
-- activate app = do
--   win <- Gtk.new Gtk.ApplicationWindow [#application := app, #title := "Diagrams GI-GTK Example"]
--
--   depthWidget <- Gtk.spinButtonNewWithRange 1 10 1
--   -- when the spinButton changes, redraw the window
--   Gtk.on depthWidget #valueChanged $ do
--     Gtk.widgetQueueDraw win -- drawArea
--     return ()
--
--   rbHilbert <- Gtk.new Gtk.CheckButton [#label := "Hilbert", #active := True]
--   rbPentaFlake <- Gtk.new Gtk.CheckButton [#label := "Penta Flake", #active := False, #group := rbHilbert]
--   radioButtons <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationVertical]
--
--   #append radioButtons rbHilbert
--   #append radioButtons rbPentaFlake
--
--   drawArea <- Gtk.new Gtk.DrawingArea [#widthRequest := 512, #heightRequest := 512]
--
--   hbox <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationVertical]
--
--   #append hbox radioButtons
--   #append hbox depthWidget
--   #append hbox drawArea
--
--   Gtk.windowSetChild win (Just hbox)
--
--   -- Redraw on changes to input parameters of the diagram
--   Gtk.on rbHilbert #toggled $ Gtk.widgetQueueDraw drawArea
--   Gtk.on depthWidget #changed $ Gtk.widgetQueueDraw drawArea
--
--   Gtk.drawingAreaSetDrawFunc drawArea $
--     Just $ \_ context width height -> do
--       curDepth <- fromIntegral <$> Gtk.spinButtonGetValueAsInt depthWidget
--       hilbertActive <- Gtk.get rbHilbert #active
--       let diagram = if hilbertActive then hilbert curDepth else pentaflake curDepth
--       defaultRender True diagram context width height
--
--   #show win
--
-- -- Gtk application
-- --
-- -- Start the GTK application
-- main :: IO ()
-- main = do
--   app <-
--     Gtk.new
--       Gtk.Application
--       [ #applicationId Gtk.:= "Diagrams GI-GTK Example",
--         Gtk.On #activate (activate ?self)
--       ]
--
--   #run app Nothing
--   pure ()
--
-- @
module Diagrams.Backend.GIGtk
  ( defaultRender,
    toGtkCoords,
    renderToGtk,
  )
where

import Control.Monad.Trans.Reader (runReaderT)
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude hiding (height, render, width)
import Foreign.Ptr (castPtr)
import GHC.Int (Int32)
import qualified GI.Cairo (Context (..))
import GI.Gtk (withManagedPtr)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo (Render (runRender))
import qualified Graphics.Rendering.Cairo.Types as Cairo (Cairo (Cairo))

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Cairo.Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
  runReaderT (Cairo.runRender r) (Cairo.Cairo (castPtr p))

-- | Convert a Diagram to the backend coordinates.
--
-- Provided to Query the diagram with coordinates from a mouse click
-- event.
--
-- > widget `on` buttonPressEvent $ tryEvent $ do
-- >   click <- eventClick
-- >   (x,y) <- eventCoordinates
-- >   let result = runQuery (query $ toGtkCoords myDiagram) (x ^& y)
-- >   do_something_with result
--
-- `toGtkCoords` does no rescaling of the diagram, however it is centered in
-- the window.
toGtkCoords :: Monoid' m => QDiagram Cairo V2 Double m -> QDiagram Cairo V2 Double m
toGtkCoords d =
  (\(_, _, d') -> d') $
    adjustDia
      Cairo
      (CairoOptions "" absolute RenderOnly False)
      d

-- | Render a diagram to a 'DrawingArea''s context with double buffering if needed,
--   rescaling to fit the full area.
defaultRender ::
  Monoid' m =>
  -- | render double buffered?
  Bool ->
  -- | Diagram
  QDiagram Cairo V2 Double m ->
  -- | DrawingArea's context to render onto
  GI.Cairo.Context ->
  -- | Width
  Int32 ->
  -- | Height
  Int32 ->
  IO ()
defaultRender db diagram ctx w h = render db opts diagram ctx
  where
    opts =
      ( CairoOptions
          { _cairoFileName = "",
            _cairoSizeSpec = dims (V2 (fromIntegral w) (fromIntegral h)),
            _cairoOutputType = RenderOnly,
            _cairoBypassAdjust = False
          }
      )

-- | Render a diagram to a 'DrawArea''s context with double buffering.  No
--   rescaling or transformations will be performed.
--
--   Typically the diagram will already have been transformed by
--   'toGtkCoords'.
renderToGtk ::
  (Monoid' m) =>
  -- | Render double-buffered
  Bool ->
  -- | Diagram
  QDiagram Cairo V2 Double m ->
  -- | DrawingArea's context to render onto --  provided by the draw event
  GI.Cairo.Context ->
  -- | Width
  Int32 ->
  -- | Height
  Int32 ->
  IO ()
renderToGtk db diagram ctx w h = render db opts diagram ctx
  where
    opts =
      ( CairoOptions
          { _cairoFileName = "",
            _cairoSizeSpec = absolute,
            _cairoOutputType = RenderOnly,
            _cairoBypassAdjust = True
          }
      )

-- | Render a diagram onto a 'GI.Cairo.Context' using the given CairoOptions. Place this within a 'draw' event callback which provides the DrawArea's context.
--
--   This uses cairo double-buffering if the thirs parameter is set to True..
render ::
  (Monoid' m) =>
  -- | DrawingArea's 'GI.Cairo.Context' to render the digram onto
  Bool ->
  Options Cairo V2 Double ->
  QDiagram Cairo V2 Double m ->
  GI.Cairo.Context ->
  -- | render double buffered?
  -- | options, depending on drawable width and height
  -- | Diagram
  IO ()
render db renderOpts diagram ctx =
  renderWithContext
    ctx
    ( do
        a@(x1, x2, y1, y2) <- Cairo.clipExtents
        let w = x2 - x1
            h = y2 - y1
        if db
          then doubleBuffer $ do
            delete w h
            snd (renderDia Cairo renderOpts diagram)
          else snd (renderDia Cairo renderOpts diagram)
    )

--
--   Used to clear canvas when using double buffering.
delete :: Double -> Double -> Cairo.Render ()
delete w h = do
  Cairo.setSourceRGB 1 1 1
  Cairo.rectangle 0 0 (w) (h)
  Cairo.fill

-- | Wrap the given render action in double buffering.
doubleBuffer :: Cairo.Render () -> Cairo.Render ()
doubleBuffer renderAction = do
  Cairo.pushGroup
  renderAction
  Cairo.popGroupToSource
  Cairo.paint

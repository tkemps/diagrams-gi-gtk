{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Gtk
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient interface to rendering diagrams directly
-- on Gtk widgets using the Cairo backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.GIGtk
       ( defaultRender
       , toGtkCoords
       , renderToGtk
       ) where

import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Maybe (fromJust)
import           Diagrams.Prelude hiding (height, width)
import           Diagrams.Backend.Cairo.Internal
import           Foreign.Ptr (castPtr)
import           GHC.Int
import qualified GI.Cairo (Context(..))
import           GI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo (Render(runRender))
import qualified Graphics.Rendering.Cairo.Types as Cairo (Cairo(Cairo))

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
toGtkCoords d = (\(_,_,d') -> d') $
  adjustDia Cairo
            (CairoOptions "" absolute RenderOnly False)
            d

-- | Render a diagram to a DrawingArea with double buffering,
--   rescaling to fit the full area.
defaultRender :: Monoid' m => GI.Cairo.Context -> QDiagram Cairo V2 Double m -> IO ()
defaultRender ctx diagram = do
  renderDoubleBuffered ctx opts diagram
    where opts w h = (CairoOptions
              { _cairoFileName     = ""
              , _cairoSizeSpec     = dims (V2 (fromIntegral w) (fromIntegral h))
              , _cairoOutputType   = RenderOnly
              , _cairoBypassAdjust = False
              }
           )

-- | Render a diagram to a 'DrawableClass' with double buffering.  No
--   rescaling or transformations will be performed.
--
--   Typically the diagram will already have been transformed by
--   'toGtkCoords'.
renderToGtk ::
  (Monoid' m)
  => GI.Cairo.Context -- ^ DrawingArea widget to render onto
  -> QDiagram Cairo V2 Double m  -- ^ Diagram
  -> IO ()
renderToGtk ctx = renderDoubleBuffered ctx opts
  where opts _ _ = (CairoOptions
                    { _cairoFileName     = ""
                    , _cairoSizeSpec     = absolute
                    , _cairoOutputType   = RenderOnly
                    , _cairoBypassAdjust = True
                    }
                   )


-- | Render a diagram onto a 'DrawingArea' using the given CairoOptions.
--
--   This uses cairo double-buffering.
renderDoubleBuffered ::
  (Monoid' m) =>
  GI.Cairo.Context -- ^ DrawingArea's Window to render onto
  -> (Int32 -> Int32 -> Options Cairo V2 Double) -- ^ options, depending on drawable width and height
  -> QDiagram Cairo V2 Double m -- ^ Diagram
  -> IO ()
renderDoubleBuffered ctx renderOpts diagram =
    renderWithContext ctx (do
        (x1, x2, y1, y2) <- Cairo.clipExtents
        let w = round $ x2 - x1
            h = round $ y2 - y1
            opts = renderOpts w h
        delete w h
        snd (renderDia Cairo opts diagram))

--
--   Used to clear canvas when using double buffering.
delete :: Int32 -> Int32 -> Cairo.Render ()
delete w h = do
  Cairo.setSourceRGB 1 1 1
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.fill


-- | Wrap the given render action in double buffering.
doubleBuffer :: Cairo.Render () -> Cairo.Render ()
doubleBuffer renderAction = do
  Cairo.pushGroup
  renderAction
  Cairo.popGroupToSource
  Cairo.paint


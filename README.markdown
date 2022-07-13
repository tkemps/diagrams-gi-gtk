# diagrams-gi-gtk

## What's this?

This package provides a rendering backend for drawing
[diagrams](http://projects.haskell.org/diagrams) directly to GTK
windows, built on top of the diagrams-cairo backend and the gi-gtk package.

# Installation

## Prerequisites

You need a GTK4 development installation since gi-gtk and diagrams-cairo are required packages and these need them. See there for further explanations.

## Compilation from the repository

To build and install the core library from the source repository, simply type:
```bash
cd diagrams-gi-gtk
cabal build
```

To build the examples, type:
```bash
cabal build -fbuildExamples
```

To run the examples, type:
```bash
cabal run ex1 -fbuildExamples
```

## How to use

Given that `diagram :: Diagram Cairo` and `drawingArea :: DrawingArea` are in scope, the following snippet
shows how to render the diagram within the drawing area:
```haskell
Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_ context width height ->
    defaultRender True diagram context width height
```
# License

The source code is distributed under a MIT license. See the `LICENSE` file.


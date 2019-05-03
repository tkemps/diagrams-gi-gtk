# diagrams-gi-gtk

## What's this?

This package provides a rendering backend for drawing
[diagrams](http://projects.haskell.org/diagrams) directly to GTK
windows, built on top of the diagrams-cairo backend and the gi-gtk package.

# Installation

## Prerequisites

You need a GTK3 development installation since gi-gtk and diagrams-cairo are required packages and these need them. See there for further explanations.

## Compilation from the repository

To build and install the core library from the source repository, simply type

    cd diagrams-gi-gtk && cabal install && cd ..

To build the examples, type

    cd diagrams-gi-gtk
    cabal configure -fbuildExamples && cabal build
    cd ..

If you like stack then

    stack build

should do the trick too.

# License

The source code is distributed under a MIT license. See the `LICENSE` file.


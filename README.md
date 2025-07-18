# Introduction

In Code Style Sheets: CSS For Code, we develop the notion of style
sheets for code. Our presentation has two primary components: a language
and semantics for describing where (i.e. to which syntactic elements)
styles should be applied within a program, and layout system which
renders styled programs as HTML documents in the web browser. Our
implementation of these two components for Haskell is called Hass.

This artifact demonstrates both of these components by implementing
several examples. These examples reproduce similar visualizations to
those seen in some research and production programming systems (as
mentioned in the paper).

With that in mind, the purpose of this artifact is twofold. It serves as
(1) a way for the reader of our paper to decide for themselves if the
style sheets we present succeed in describing an expressive interface
for multivarious program visualizations, as we claim they do in the
paper. And (2), it serves as a reference implementation that a reader
could study if they wish to have a deeper understanding of how the
components described in the paper are implemented.

Readers interested in (1) should direct themselves to `examples/`, where
they can read the style sheets which were used to generate the figures
in the paper.

Readers interested in (2) should direct themselves to
`stylish-text/src/StylishText/Style.hs`, for the implementation of style
computation, or `stylish-text/src/Layout/Algorithm.hs` for the
implementation of layout.

# Hardware Dependencies

To reproduce the examples in this repository, you\'ll need a computer
with an installation of `ghc` (\>= 9.2.1), and `cabal` (\>= 2.4), as
well as `node` (\>= 14.17) and `npm`.

# Getting Started Guide

The code style sheets artifact is organized as a client-server model.
All of the style computation and layout code is implemented on the
server--the client exists only to present the rendered output as an HTML
page, and to respond to requests from the server about the size of the
elements to be laid out.

In order to view examples, we need to build both the client and the
server. The below instructions describe how to do this, and how to run
an example.

## Building the Client

The `client` directory contains all of the front-end code for rendering
code in the browser. To build, we first need to install the dependencies
of the front-end using npm. Run the following command from the root of
the repository:

``` shell
$ npm -C client install
```

Once that\'s finished, we can build the front-end. Run the following to
do so:

``` shell
$ npm -C client run build
```

Building the front end should create and populate a directory called
`dist_client` in the root of the repository.

## Building the Server

The code style sheets server depends on the tree-sitter library to
parse Haskell code. The Haskell bindings for this library are included
at at `ts-parser/haskell-tree-sitter/`. For the purposes of this
artifact, this library, and its dependencies, has been "inlined"
(rather than using a git submodule, which are not supported by
Zenodo).

To build the server, run the following:

``` shell
$ cabal build
```

This will build all of the executables (one for each example in the
paper). To test that the build worked, you can run:

``` shell
$ cabal run Blocks -- ./examples/Blocks.hs
```

This will run the `Blocks` style sheet on the `examples/Blocks.hs`
program (Figure 1 of the paper). If everything works correctly, you
should see the message `listening on port 1234!` print in your terminal.
Then, navigate to `http://localhost:1234` in your browser to see the
rendered output.

# Evaluation Guide

The below table summarizes the main examples presented in the paper (see
also Fig. 27 in Appendix D, for reference). The rightmost column shows
the command that may be run in order to reproduce the given example.
Note that there is a separate executable for each example, and each
executable expects one command-line argument which a path to the input
source file. The only exception is Projection Boxes, which in addition
to a source file, requires some input via `stdin`.

| Example Name          |  Figure(s)        | Command To Run
| --------------------- | ----------------- | -------------------------------------------------------------------------------------------------------------
| Blocks                | Figure 1 (c, d)   | `cabal run Blocks -- ./examples/Blocks.hs`
| Skeleton Code         | Figure 16 (a)     | `cabal run SkeletonCode -- ./examples/SkeletonCode.hs`
| Point-Free Pipeline   | Figure 3 (f)      | `cabal run PointFreePipeline -- ./examples/PointFreePipeline.hs`
| Syntax Highlighting   | Figure 1 (a)      | `cabal run SyntaxHighlighting -- ./examples/SyntaxHighlighting.hs`
| Semantic Highlighting | Figure 16 (b)     | `cabal run SemanticHighlighting -- ./examples/SemanticHighlighting.hs`
| Type Error            | Figure 17         | `cabal run TypeError -- ./examples/TypeError.hs`
| Projection Boxes      | Figure 2 (e)      | `cat ./examples/ProjectionBoxesInput.hs \| cabal run ProjectionBoxes -- ./examples/ProjectionBoxes.hs`
| Test Runner           | Figure 18 (a)     | `cabal run TestRunner -- ./examples/TestRunner.hs`
| Heat Map              | Figure 18 (b)     | `cabal run HeatMap -- ./examples/HeatMap.hs`

# Reusability Guide

The below is a summary of where one can find the critical algorithms and
definitions as described in the paper. It also serves as a guide for
navigating this repository.

## `app/`

This directory contains the source code for all of the executables in
the project. There is an executable for every example presented in the
paper. Each executable has a corresponding file in `examples/`, which is
the *input* file that was used to generate the example.

Note that since code style sheets supports a parser which covers most of
Haskell\'s surface syntax (`chi`), and an interpreter which supports a
tiny subset of Haskell (`tiny-chi`), not all executables will work with
all examples. Generally speaking, the first four examples (Blocks,
Skeleton Code, Point-Free Pipeline, and Syntax Highlighting) are meant
to work with `chi`, while the rest of the examples are designed to work
with `tiny-chi`.

The interpreter was designed with the examples in mind, so if you want
to try your own examples, keep in mind that if you\'re trying to use a
Haskell feature that isn\'t already shown in one of the `tiny-chi`
examples, it may well be unsupported.

## `chi/`

`chi` is the subproject which implements a parser (but not interpreter,
or any static analysis) for Haskell. It supports a large subset of
Haskell\'s surface syntax. There are two interesting files:

-   `chi/src/CHI/Parser.hs` is the parser for `chi`, which converts
    tree-sitter output into our AST type.
-   `chi/src/CHI/Types.hs` contains both the AST definition and its
    `Stylish` instance, which has been written in such a way as to make
    it clear that implementing a `Stylish` instance could be made a
    mechanical process.

## `client/`

This directory contains the client implementation. Most of the code in
this directory is not important for the purposes of the paper. It is a
straightforward React application. There is one file of interest:

-   `client/measureElement.ts` defines a function which, given the
    description of a fragment (`Types.MeasureSpec`), instantiates the
    element in the DOM with its computed styles, and responds with the
    size of the element.

The client also contains the beginnings of a \"DOM inspector for Hass,\"
as mentioned in section 8.4 of the paper. (It can be invoked by clicking
and shift-clicking on elements of the rendered displays in the browser.)

## `dist-client/`

`dist-client/` contains the compiled client code. You shouldn\'t need to
look in here.

## `examples/`

This directory contains each of the example *inputs* which were used to
generate the figures in the paper. Each file in this directory has a
corresponding executable in `app/`.

Note that these Haskell files are not meant to be compiled with the rest
of the code in the project. They are meant to be either parsed by the
parser defined in `chi/`, or interpreted by the interpreter defined in
`tiny-chi/`. As such, some of the files would not compile if run through
`ghc`, for example.

## `scripts/`

`scripts/` contains:

-   `scripts/countLOC.rb`, which counts the lines of code.

## `src/`

The `src/` directory contains two Haskell modules which define the
client-server communication protocol and webserver for viewing the
examples. They aren\'t important for understanding the main ideas of the
paper.

## `stylish-text/`

This directory contains our implementation of both the style computation
and layout algorithms as presented in the paper.

-   `stylish-text/src/StylishText/Style.hs` is the style computation
    algorithm.
-   `stylish-text/src/Layout/Algorithm.hs` is the s-blocks layout
    algorithm.

These are the two most important files for understanding (and re-using)
the implementation. `stylish-text/` also contains the definition of the
surface syntax for our style sheets.

-   `stylish-text/src/Stylesheet/Parser.hs` is the style sheet parser,
    while
-   `stylish-text/src/Stylesheet/Types.hs` is the type definition of a
    style sheet, and the quasi-quoter which allows us to embed style
    sheets using our custom syntax inside Haskell source files.

## `tiny-chi/`

`tiny-chi/` contains a parser and tracing interpreter for a tiny subset
of Haskell, which is used for the program analyses demonstrated in the
latter examples. The interpreter is standard, and isn\'t important for
understanding the ideas in the paper.

The Stylish instances for the AST type of `tiny-chi` are important
because they demonstrate our encoding of \"projections\" or \"views\" on
the program AST. All of the `Stylish` instances used for the examples in
the paper are factored into three files:

-   `tiny-chi/src/Tiny/CHI/DefaultStylish.hs`, which defines the
    `DefaultStylish` wrapper type, and
-   `tiny-chi/src/Tiny/CHI/ProjectionBoxesStylish.hs`, which defines the
    `ProjectionBoxesStylish` wrapper type, used to add projection-boxes
    like displays inline with code,
-   and finally `tiny-chi/src/Tiny/CHI/StylishCode.hs`, which defines
    the mostly mechanical plumbing required to implement provenance.
    Note the similarity between this file and `chi/src/CHI/Types.hs`,
    which contains the `Stylish` instance for `chi`.

## `ts-parser/`

This subproject is a simple wrapper over the tree-sitter bindings which
deals with the details of low-level memory management. It isn\'t
important for understanding the core ideas of the paper.

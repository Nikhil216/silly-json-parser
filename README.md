# sily-json-parser

This is attempt at writing a JSON parser in hopes of learning about lexers and parsers. As far as I know, this follows the json specification. The style followed here is the one used by [Elm](https://elm-lang.org/docs/style-guide).

## Overview

The source contains three files

- Magic.hs

  Running this with a command line argument for the file path would return true if the file starts with the magic code `P K 0x3 0x4` for a zip file.

- Scanner.hs

  This file contains all the function to describe the JSON syntax in a grammer and be able to parse it into tokens. This has its own rendition of regular expression with functions like `matchPipe`, `matchOr`, `matchOneOrMoreChar`, etc. The tests for these function are written in `tests/Spec.hs`.

- Parser.hs

  This contains the code to parse a JSON file into the data structure `JSData`.

## Build

You would need `stack` to build this project. Stack can be installed by following these [instructions](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md). Once installed you can run this command to see the tree generated.

```sh
stack run -- file.json
```

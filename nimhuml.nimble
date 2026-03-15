# Package
version       = "0.2.0"
author        = "Abhishek Verma"
description   = "A Nim implementation of HUML (Human-Oriented Markup Language) parser and serializer"
license       = "MIT"
srcDir        = "src"

# Dependencies
requires "nim >= 2.0.0"

# Tasks
task test, "Run tests":
  exec "nim c -r tests/test_nimhuml.nim"

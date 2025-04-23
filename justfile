run:
  rlwrap cabal run

test:
  cabal test

build:
  cabal build

format:
  nix fmt

check:
  nix flake check

spell:
  nix build .\#spell-check

format-check:
  nix build .\#format-check

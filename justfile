run:
  just repl

repl:
  rlwrap cabal run octizys -- repl --logLevel=info

compile *paths:
  cabal run octizys -- compile --logLevel=trace {{paths}}


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


# haddock complains about post-qualified imports
qualify:
  find test -type f -name "*.hs" -exec sed -i -E 's/^import[[:space:]]+([A-Z][A-Za-z0-9_.]*)[[:space:]]+qualified[[:space:]]+as[[:space:]]+([A-Z][A-Za-z0-9_]*)/import qualified \1 as \2/' {} +
  find src -type f -name "*.hs" -exec sed -i -E 's/^import[[:space:]]+([A-Z][A-Za-z0-9_.]*)[[:space:]]+qualified[[:space:]]+as[[:space:]]+([A-Z][A-Za-z0-9_]*)/import qualified \1 as \2/' {} +

add MODULE:
    REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null) && \
    if [ -z "$REPO_ROOT" ]; then \
        echo "No se encontró la raíz del repositorio Git."; \
        exit 1; \
    fi && \
    FILE_PATH="$REPO_ROOT/src/$(echo {{MODULE}} | tr '.' '/')".hs && \
    mkdir -p "$(dirname "$FILE_PATH")" && \
    if [ ! -f "$FILE_PATH" ]; then \
        echo "module {{MODULE}} where" > "$FILE_PATH"; \
        echo "Archivo creado: $FILE_PATH"; \
    else \
        echo "Archivo ya existe: $FILE_PATH"; \
    fi

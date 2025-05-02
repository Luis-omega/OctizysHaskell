A functional programming language

# Example

This is a factorial written in the current language syntax, it can be found
[here](test/Examples/factorial.oct), and
[here it is colorized](https://luis-omega.github.io/OctizysHaskell/index.html)
by Treesitter

```
factorial_standard : n :Int , Int =
    if lt n 2
    then
      1
    else
      mul n (factorial_standard (minus n 1));


factorial_acc : n:Int , acc:Int, Int =
    if lt n 2
    then
      acc
    else
      factorial_acc (minus n 1) (mul n acc);

factorial_acc_sass : n:Int, acc:Int, Int =
    if lt n 2
    then
      acc
    else
      let new_n = minus n 1;
          new_acc = mul n acc;
      in
        factorial_acc_sass new_n new_acc
    ;
```

# Editor support

## Treesitter

We maintain a Treesitter grammar
[here](https://github.com/Luis-omega/tree-sitter-octizys)

## Neovim

There is also a neovim plugin to register the language and set some properties
[here](https://github.com/Luis-omega/vim-octizys).

Note that you still need to install the Treesitter grammar by yourself.

# Roadmap

- Build a simple typed lambda calculus compiler with let and definitions
  - Support for build-in booleans and ints
  - Type checking
  - A REPL
  - Minimal standard library
  - naive code generation (python)
  - cps optimization
  - sass optimization
  - code generation (python, wasm)
  - optional : module system
- Addition of monomorphic records
- Addition of sum types
- Addition of parametric polymorphism
- Optional:
  - Typeclasses
  - A fixed set of operators with fixed precedence
- Checked exceptions or effects
- Code formatter
- Treesitter grammar
- Package manager

# Examples

```
factorial_aux : int -> int -> int
= {
    \ n acc ->
        if lt n 2
        then
            1
        else
            let new_n = minus n 1;
                new_acc = mul n acc;
            in
                factorial_aux new_n new_acc
}

factorial : int -> int = { \ n -> factorial_aux n 1 }
```

# Build and Development

## Build

The simpler way to build is using nix

```
nix build
```

## Develop environment

To access a development environment

```
nix develop
```

## Formatting

To format all files (Haskell + Markdown + Shell + Cabal + YAML + TOML) run

```
nix fmt
```

## Pre-push hooks

Be sure to add the following pre-push hook under `.git/hooks/pre-push`

```bash
root=$(git rev-parse --show-toplevel)
cd $root

nix flake check
flake_check_status=$?
nix develop --command bash -c "cabal test"
cabal_test_status=$?
if [ $flake_check_status -ne 0 ]; then
  echo "Failed at flake check"
  exit $flake_check_status
fi
if [ $cabal_test_status -ne 0 ]; then
  echo "Failed at haskell tests"
  exit $cabal_test_status
fi
exit 0
```

## Pre-commit hooks

Be sure to add the following pre-commit hook under `.git/hooks/pre-commit`

```bash
root=$(git rev-parse --show-toplevel)
cd $root

nix flake check
flake_check_status=$?

if [ $flake_check_status -ne 0 ]; then
  echo "Failed at flake check"
  exit $flake_check_status
fi
```

## Checks

We have two checks active right now:

- format check
- spell check

Both of them are run as part of the nix checks:

```
nix flake check
```

But can be run individually

## Spell check

The spell checker we use is `typos` and can be run individually by

```
nix build .\#spell-check
```

## Format check

We use various tools for format, one per language.

All of this tools are supported by `treefmt-nix`, and we can run the format
check like

```
nix build .\#format-check
```

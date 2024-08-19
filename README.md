`interval-index`
=====

Efficient interval querying over extensible data in Haskell.

## Why

Interval scheduling problems are easier if you have a nice way to look up intervals covering points and time ranges.
This library provides a way to represent data that can be viewed as intervals in a format where it's relatively cheap
to find data matching some query over the interval type `k`, e.g.:

```haskell
import Data.Interval (IntervalLit(..))
import Data.IntervalIndex (at, fromList)

idx = fromList [IntervalLit 'a' 'f', IntervalLit 'c' 'k']

idx `at` 'e'
-- [IntervalLit 'a' 'f', IntervalLit 'c' 'k']

idx `at` 'm'
-- Nothing
```

## Development

This project was initialized from a Nix flake template.

To get started, replace `project-name` with the name of your project in

* the file name `project-name.cabal` (e.g. rename to `my-executable.cabal`)
* module names in the file contents of `project-name.cabal` (e.g. with `rg -l 'project-name' | xargs sed -i 's/project-name/my-executable/g'`)

Then run a `nix build` to compile everything and run tests. You can verify
success with a `nix run .`, which will call the program definedin `app/Main.hs`.

### Project Features

* `hspec-discover` for automatic test discover (see `LibSpec` for an example)
* some example dev/test/main dependencies in the `cabal` file
* required plugins for ergonomic use of `hls`, including rename support
* `cabal-install` so you can run tests manually as well for nicer output

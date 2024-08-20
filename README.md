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

The easiest development environment is to use the development shell provided
in the [`flake.nix`](./flake.nix). It provides Haskell development tools including:

* `cabal`
* `haskell-language-server`
* `hlint`
* `ormolu`
* test discovery with `hspec-discover`

module Main where

import Criterion.Main (bench, bgroup, defaultMain)

-- example blog post:
-- http://www.serpentine.com/criterion/tutorial.html
--
-- probably want nf for all of these benchmarks
-- http://www.serpentine.com/criterion/tutorial.html#benchmarking-pure-functions
-- given that I have a complex object returned at the end and I want more than just the outermost constructor
main :: IO ()
main = defaultMain [bgroup "fromList" [], bgroup "insert" [], bgroup "merge" []]

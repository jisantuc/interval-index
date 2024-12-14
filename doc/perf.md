# Performance benchmarking

My reference point for an interval index library is [`intervaltree`]. I don't know anything about its implementation
(though for obvious reasons I assume some kind of tree is involved), but it's the best comparison I can think of for
comparing interval-y throughput.

## Design differences

There are a few noteworthy differences that I think are likely to affect results.

The first is that adding values to an interval tree in the python library mutates in place rather than returns a new
tree. This difference should affect resource utilization (maybe take less memory?) but shouldn't affect what you get
when you query one of the objects at a point.

The second difference likely affects what you get when you query the tree. [`intervaltree`] treats intervals as closed
on both ends, while this library treats intervals as open on the right, meaning that an interval from `a` to `e`
includes `a` through `d` in this library but all of `a` through `e` in [`intervaltree`].[^1]

Nonetheless, the libraries are similar enough in principal that I think `[`intervaltree`] is a good point of
comparison.

## Benchmarks

In all cases, I'll ensure that the two libraries are operating with identical data. I'll vary benchmarks over two axes
for both -- _dense interval_ benchmarks vs. _sparse interval_ benchmarks and _literal intervals_ vs. _data-carrying
intervals_. Dense interval benchmarks will run against data where, for a given key, on average the index will hold
several values. Sparse interval benchmarks will run against data where, for a given key, mostly there isn't any data in
the index. Literal intervals are data that hold their start and end values and nothing else, e.g., in `interval-index`,
`IntervalLit { beginning = 'a', end = 'e' }`. Data-carrying intervals carry their start and end data in some form _and_
some additional data, e.g., in `interval-index`,

```haskell
data DataCarryingInterval a = DataCarryingInterval { start :: Int, end :: Int, somethingElse :: a }

instance Interval Int (DataCarryingInterval b) where
  intervalStart dci = start dci
  intervalEnd dci = end dci
```

I'll generate both data sets ahead of time and make sure the time to read in the data isn't part of the
throughput. All benchmarks will run on my personal laptop with an Intel i9 and 64gb of RAM [^2].

### Construction

Construction benchmarks matter if you persist the interval data rather than persisting the index itself.
If the index is JSON serializable, and you write/read the JSON rather than construct the index from the underlying data
when you need it, you don't really need to care about construction costs, because you'll probably pay construction
costs exactly once. If instead you mostly work with data outside of its interval representation and sometimes need to
work with it as intervals, you'll construct the index ad hoc, and construction speed is important.

I randomly generated 10,000[^3] interval values and measured construction times using `criterion` for this library and
SOMETHING ELSE[^4] to measure construction time in [`intervaltree`] for 100 / 1000 / all 10000 randomly chosen entries.[^5]

| Entries | `interval-index` | `intervaltree` |
| :------ | ------ | ------ |
| 100 literal sparse | a | 597 us |
| 1,000 literal sparse | a | 7.72 ms |
| 10,000 literal sparse | a | 82.7 ms |
| 100 literal dense | a | 566 us |
| 1,000 literal dense | a | 6.64 ms |
| 10,000 literal dense | a | 67.2 ms |
| 100 data carrying sparse | a | 599 us |
| 1,000 data carrying sparse | a | 7.60 ms |
| 10,000 data carrying sparse | a | 84.8 ms |
| 100 data carrying dense | a | 567 us |
| 1,000 data carrying dense | a | 7.08 ms |
| 10,000 data carrying dense | a | 68.7 ms |


### Point query

### Range query

### Insertion

Insertion benchmarks are important if it's common for you to incrementally add data to your index. If you persist the
underlying data instead of the interval index and only query the index without writing new values to it, insertion
throughput isn't important to you. However, if you're doing something like using the interval index as a representation
of a schedule and adding new scheduled objects to it (and you have a lot of objects to add/learn the new objects to
add one at a time), insertion throughput is really important to you.[^5]

| Entries | `interval-index` | `intervaltree` |
| :------ | ------ | ----- |
| 100 literal | a | a |
| 1,000 literal | a | a |
| 10,000 literal | a | a |
| 100 data carrying | a | a |
| 1,000 data carrying | a | a |
| 1,0000 data carrying | a | a |

### Merge

Merge benchmarks are important if it's common for you to combine indices. tktktk

[`intervaltree`]: todo
[^1]: verify this!
[^2]: verify this/be more specific
[^3]: actually do this
[^4]: what?
[^5]: I apparently don't remember markdown table syntax

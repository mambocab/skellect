# Skellect

This is a simple Haskell implementation of [Gary Bernhardt's `selecta`](https://github.com/garybernhardt/selecta). I wrote it primarily as an exercise in Haskell, and the `selecta` use case has some properties that make it interesting as such:

- It does some [pure text manipulation and mathematical calculation](https://github.com/mambocab/skellect/blob/master/library/Skellect/Score.hs), exercising core Haskell language features.
- It [does I/O](https://github.com/mambocab/skellect/blob/master/executable/Main.hs), and more than simply reading `stdin` or files. It [accepts input interactively](https://github.com/mambocab/skellect/blob/master/executable/Main.hs#L43) and [dynamically updates output](https://github.com/mambocab/skellect/blob/master/executable/Main.hs#L56) in one position in a terminal.
- As a result of the above, it [requires good data structures](https://github.com/mambocab/skellect/blob/master/library/Skellect/ListZipper.hs) for maintaining an internal view of, e.g., current inputs and scroll state so they can be manipulated and displayed correctly.

In addition, it was a good exercise for learning Haskell tooling, such as that for testing and building.

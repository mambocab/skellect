# Skellect

This is a simple Haskell implementation of [Gary Bernhardt's `selecta`](https://github.com/garybernhardt/selecta). I wrote it primarily as an exercise in Haskell, and the `selecta` use case has some properties that make it interesting as such:

- It does some pure text manipulation and mathematical calculation, exercising core Haskell language features.
- It does I/O, and more than simply reading `stdin` or files. It accepts input interactively and dynamically updates output in one position in a terminal.
- As a result of the above, it requires good data structures for maintaining an internal view of, e.g., current inputs and scroll state so they can be manipulated and displayed correctly.

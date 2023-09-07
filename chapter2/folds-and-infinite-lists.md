# Folds and Infinite lists

> What happens?

The call will result in an infinite loop, but will not cause a stack overflow.

> What does this tell you about why you can't use `foldl` on an infinite list?

That `foldl` is eager and tries to evaluate the entire list before applying the function.

> Are there any other benefits to `foldr` when dealing with large but finite lists?

Since `foldr` processes lists from right to left, it can produce results incrementally and therefore be more memory-efficient than `foldl`.

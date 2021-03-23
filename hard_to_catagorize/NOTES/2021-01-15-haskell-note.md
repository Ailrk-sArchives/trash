### Some Haskell techniques 2021-01-15.

- If you need to count something
  - say you have `foo :: [Int] -> String`  that will be called recursively, and now you want to know how many times it's called, if it's larger than a number reutnr the times, you can write a wrapper function `foo' :: [Int] -> Int -> String` and increment the count from the parameter.
  - Whenever you need to keep track of several things, passing a tuple or return a tuple is almost always correct. `foldl' (\(p, q) z -> (z, p : q)) (x, []) r` for instance allows you to have access of the last element while consing.
  - List comprehension is nice when you need to iterate. It's more ergnomic then map for the most of time you only need a simple loop like mechanism.
  - To get an index of elements in a sequence, you can zip the sequence with `[1..]`, wich gives you a tuple `(r, Int)` .
  - Traverse is almost always the answer.

- When thinking of recursion:
  - If a type hole want a type, just throw some expression with that type. If youcan make an expression with requirement type, most of the time it's correct, you already have the scaffoler.
  - Don't worry to much about non tail recursion like how you do in lisp, lazyniess avoid most of the problem it causes.

- When thinking of immutable data structures.
  - Immutable data structures are like math objects, you don't modify a set, instead you make a new set.
  - Immutable data structures are static, and they are easier to reason about. It's like you are looking at a map, you can investigate where things go.
  - Immutable data structure can be implemented with acceptable efficiency. To avoid uncessary copy one can use copy on write, and there are several amortized constant time data structure.
  - Array plays poorly with immutable data structure, especially when an array is being used as a buffer.
  - Pointer has properties immutable data structures can't have (or hard to have). To fully simulate what a pointer can do you need zipper and lens combined. (To access an element (even nested element), and to look at the promixity of the element).
  - Finger tree is goooood in theory. But you don't have good cache locality. This means if you try to sell it to c++ people you will have problem.
  - Anything you want to keep track of can be preresented as a tuple or a record, to access an individule item just pattern match or use `RecordWildcard` extension.

-  When thinking of state
  - You can have acutal, destructive state with ST or IO ref, or you can have State monad mimic state.
  - State monad is still immutable. It simply pass a implicit argument, and allows you to "modify" the state within the monad. Under the hood a modification is just replace the old state with a new one.
  - ST and IO are real mutable state, they are actual reference to a piece of memory. If you call modify, that same piece of memory will be changed.
  - Besides that, if you have two processes sharing the same memory, the memory can be in a race condition, which is not really possible with State Monad.
  - For most of the time you only need state monad and st monad. Only use IORef when necessary.

- When thinking of types.
  - Wrapping types is the essence of Haskell,  you wrap a type in some other type to add new logic on top.
  - You can even say `HoldLock a`  indicates a is protected by a `TVar`, I think it's a good example of showing how type is used to decorate funtionality.
  - Use type hold wisely. Try to deduce the implementation from the type required.
  - Normally you will only have handful amount of materials to use within a scope. There are only that many combinations you can make to create the expected type.
  - Type holes help you to shink the range until you can find the answer yourself.
  - Don't worry about creating too many new types. Types are very low cost objects in Haskell
  - Sum types play a role in control flow, each case can lead to an implemnetation. (The mechanism is really similar to a overload)

- When thinking of folds:
  - `traverse :: (a -> m b) -> f a -> m f b` is probably the most powerful combinator to control a foldable.
  - `traverse = sequence . fmap`
  - compare witha normal map `fmap :: (a -> b) -> f a -> f b` , `traverse` traverse the foldable while applying the side effect.
  - This resemble what a for loop can do.
  - Good thing about haskell is now you can annotate what side effect you are using. If you see `traverse putStrLn [1..10]` , for example you know you are doing IO.

- When thinking about performance
  - Haskell is like any other compiled languages, compiler is your best friend. You want to be able to understand what it compiles to (core or stg) to understand it's performance implications.
  - Use rewrite rules and compiler pragmas to precisely control the compilation process. C++ programmers should know this better than anybody else.
  - Like Java or other GC languges, you can also tweak the GC setting.
  - How to tweak gc really depends on the nature of the application. If you want gc to be less frequent, you can tone down the size of young generation, for example.

- When thinking of ecosystem and tooling.
  - It sucks.
  - The dependency management and build tool is not better then CMake. At least you have modules.
  - Lot's of commonly used modules are not in prelude.
  - People like to lock the version which make things acutally harder to access.
  - Bad decision of what goes to prelude really increase the learning curve a lot.

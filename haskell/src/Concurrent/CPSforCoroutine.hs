module Concurrent.CPSforCoroutine where

{-@ Use continuation to implement coroutine.
@-}



{-@ Coroutine is:
    1. a generalization of subroutine.

    2. yield instead of return. resume to go back to last
       exit point.
    3. yield not return also means coroutine holds states.

    4. the interaction between two coroutines can be similar
       to mutual recursive, but it's more efficient as the call
       doesn't need to be at the tail position, coroutine holds
       states so no need to pass state as parameters etc.

    5. k

    6. Coroutine can be implemented with goto or continuation.
       We are in haskell so we takes the cps approach, but goto
       is essentially the same.
@-}


-- The history of coroutine actually way back to lat 1950s. The first
-- coroutine was done in assembly implemented with jmp. The idea was
-- simply to have a subroutine that can exit and re-enter with it's state
-- preserved.

{-@ Use coroutine as the fundation to implementes many things:

    1. Easy to read state machine...
    2. actor model
    3. coinductive types (generators, conduit)
    4. communicating sequential processes. (csp)
    5. exception.
    6. events.

@-}

{-@ Cooperative multitasking.

    1. First we have preemtive multitasking, where programs
       run on separate threads, and the scheduler descide which
       thread gain the control and for how long.

    2. In cooperative multitaksing there is no scheduler, each
       subprogram voluntarily gives up the control to some other
       subprogram.

    3. non preemptive multitasking means you don't need to worry about
       thread safty as you do with mutexes and threads.

@-}

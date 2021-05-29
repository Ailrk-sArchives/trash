# How to run asynchrounsly?

Several models.

- threads
  - preemtive multitasking
  - straight forward control
  - needs synchronization
  - sync can have large overhead
  - typical model from pthread (c++)

- event driven programming
  - inverse of control
  - register callback to an event. Call the callback when event is triggered.
  - loosely coupled comminication
  - can be abstracted with monads to avoid nested callbacks so no big deal.
  - javascript event loop builds on this

- coroutines
  - cooperative multitasking
  - straight forward control
  - it's a higher level abstraction, which itself can have overhead (I mean coroutine needs to be implemented somehwere as it doesn't come for free).
  - green threads are essentially coroutine.
  - the model is really similar to csp tbh.

- actor models
  - mail box model (actor1 -> [maxibox] -> actor 2)
  - point to point communication
  - separate computations into actors, communicate three message passing.
  - retry when error happens.
  - fire and forget
  - what you see in erlang's model (true oop!)

- communicating sequential processes.
  - actually very similar with actor model.
  - channel model (process1 -> [(write end), (read end)] -> process 2)
  - processes are anonymous (like a spsc queue)
  - Chan in haskell and rust are examples.
  - based on process calculus. A formalization of


Haskell uses green threads, which essentially the compiler converts all functions run on thread a coroutine by inserting yield points, and the runtime system take over the control of these yield points.

Think about what's necessary to implement green thread:
1. being able to spawn system threads
2. manage a thread pools, which is the container of green threads
3. convert functions into coroutines by inserting yield points
4. bound coroutines to system threads
5. embed an even loop into the runtime system to schedule.

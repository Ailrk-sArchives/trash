# Pointers, smart pointers.

Smart pointers boil down to the idea of having a small handle on the stack that control a much larger memory in the heap. Because the handle is on the stack, it's part of the automatic storage. The life time is managed the same way as any other objects in the stack.

At the very beginning there is no distinguishment between stack and heap what so ever. On one hand, you have instructions for control flow, some instructions to load, modify values in the register, move values from and to the memory (as a big array), and some operations on pointers to achieve indirect access.


__Why do we need memory any way?__
What does memory do anyway? We know we can build half adder, chain them into a full adder. Given two set of input bits, it return their sum (an example of combinational logic). The problem is we can only have one output at a time, there is no way if say, we want to save the output somewhere unless we write it down. Now think about vending machine, it's a typical finite state automaton, which is equivalent with a regular language. At each given moment, we are at one state, and what state we go next depends on the input. The implication is: given we at some state, say A, we don't really know how we get there.

-- TODO the computional power from combinational logic -> finite automaton -> pushdown machine -> turing machine.


The most restricted form of computational model as combinational logic, which the output is a pure function of the input. Then we have finite automaton that has the concept of state transition. To make it more powerful, we have push down automaton, in which we can not only have state transition, but a stack to remember what happend before; the operation is also determined by the value in the stack.

A computer should be turning complete, means it definitely needs to be more powerful than finite state automaton. Conceptually turning machine is conceived as a head operate on a infinite long tape. The tape is separated into cells, and each cell can be read from or write into a symbol at a time, you can move the tape one cell left or one cell right. Operations like read, write, move, change state are instructions that are provided before the machine runs, they are essentailly the program. One important thing to note here is that all features of the turning machine can do is deterministic, yet because of the decision problem, given a program, you don't know if it halts.

Looking at turning machine, the tape seems to capture the notion of memory. It has infinite storage, and symbol once written in a cell can be retrieved later. So what does it enable us to do? What's the difference that if we can't read from the tape?

-- TODO Turning machine
-- TODO multitape turing machine and how it can be simulated by turing machine.
-- TODO how adding a stack make FA to PDM
-- TODO how adding a second stack improve the power of PDM into stack automaton.
-- TODO how remove all the restriction makes turing machine.
-- TODO Recap Basic set of things can do with turing machine
-- TODO The role of the tape as memory.
-- TODO The role of the head being able to read write.
-- TODO Simulating random accecss without considering the cost.

-- TODO How this model reflects to pointers on array like memory.

Turning machine is formulated as a 7-tupke M = ⟨Q `set eof state`, Γ `state of symbols on tape`, b `blank symbol`, ∑ `input symbols`, δ `transition function`, q₀ `initial state`, F `final states`⟩. Fact: there can be multiple tapes for a turning machine, and no matter how many tapes there are, you can always simulate it with a single tape turing machine.

An exampe of turning machine can be written like this: Q = A, B, C, HALT }, Γ = {0, 1}, b = 0, ∑ = {1}, q₀ = A, F = {Halt}. For the transition function we can make a table:

```
δ :: Map (Q, Γ) Operation
δ = {
  (A, 0): { write: 1 move: R next_state: B },
  (A, 1): { write: 1 move: L next_state: C },
  (B, 0): { write: 1 move: L next_state: A },
  (B, 1): { write: 1 move: R next_state: B },
  (C, 0): { write: 1 move: L next_state: B },
  (C, 1): { write: 1 move: R next_state: Halt },
}
```

Given the current state and the symbol in the tape, the transition function tells us what operation to perfrom next. We don't perform read actively, instead we just read the cell once we finished a step. This is a three state busy beaver.



__Some canonical examples of ancient computer:__
1. 1950s:

Today's computer read input output from some desginated device like keyboard and screen, but at that age the only format that machine reads was punch cards. Although the format looks weird and a bit funny for people live in 21st century, the way they work is actually not too much different from what wehave. Punch cards encode data and programs. For program, it stores the encoding of the text of the source code (Some stripped version of ascii). A typical punch card can encode 80 characters, the card reader will scan the card from left to right to read the content and load it into the memory.

How do you make these punch cards? There are dedicated machines helps you to do that. It's card punching machine with a keyboard. You sit in front of the machine, program in assembly with instructions spelled out in English (Or something like Algol in later days). The machine is mechanical, it will punch different holes in a vertical line. How holes are punched depends on the character you typed. If you made a mistake the machine will discard this card and replace with another one. It's a little mechanical programming environment.

After you get a pile of punch cards that records your program, you hand them over to the card reader, which loads the program into the memory. If you write in a high level langauge, the computer needs to load the code for the compiler first to be able to compile your code. If you compare this process with the workflow we have today, essentially you as a programmer needs to be part of the IO, to mannually feed the program to the computer.

So what does memory look like in those ancient computers?

-- TODO delay line memory, mercuray, nickle, cheat but sequential.
Well around 1950s was a chaning era for memory devices. In the earily half most memory were sequential acess. The memory act like a doubly linked list, if you want to read anything in the middle, you need to move your read head all the way to that address. Some notable examples were "delay line memory" or "tape memory" for SSEC (not really a memory). The idea of having a memory is to

-- TODO How SSEC uses punch card tape to load progarms, how this is not a ture memory.
An example of earily days machine is IBM SSEC, a mixture of vacuum tubes and electromagnetical relays, designed in 1944, and operated from 1948 to 1952. It's one of early machine that stores both program and data in the same memory.


2. 1960s: Fortran IV runs on IMB 7030, a machine around 1964.
-- TODO Random access memory, magnetic core memory.
-- TODO Assembly runs on top of it. Storing address in a memory location that points to somewhere else, (intermediate addressing)

4. 1970s: pdp11, rleased at 1970, and it's where unix and C first released on. In __pdp11/40 processor handbook__ h
-- TODO Still using core memory, the abstract model actually carries on til today.
-- TODO Program with stack.
-- TODO Heap

Memory is just an big big flat array that supports random access. Supproting random access means you can lookup the value at any address in O(1) time. You can store

Objects live in the stack has the property that they get destroyed automatically when they are out of the scope.

Objects live in the heap has the advantage that they can be much bigger in size.

We can take the advantage of this property, let

To track life time

In imperative languages, executions happen in sequential.

The scope of computation is get marked as a interval of computation, start from some lines of code and finish at some offset of it.

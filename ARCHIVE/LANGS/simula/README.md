# Simula

- Came out at 1962, more known version is the 1967 one: Simula 67.
- Made by Kristen Nygaard & Ole-Johan Dahl for simulations.
- It's garbage collected (oh.) StoupStrop mentioned how the gc was incredibly slow which made him want to make the memory management fully mannual to be performant.

- According to Stroustup Simula greatly influenced C++ and it's class design.

- Simula is a class augmented Algol60. Semantics of class are almost the same. There is no access control.

- Simula 67 cmes with lots of modern OOP elements.
  - class and objects
  - virtual procedure with dynamic dispatching
  - coroutines
  - inheritence + subtyping.
  - discrete-event simulation (DES) thingy. Apparently it's a simulation thingy of stochastic processes with discrete state. R seems to have it too.

- Simula has a wide influence on object oriented langauges. Started from C++, and to now many otheres that adopts the same model. Both StroupStrup and James Gosling admitted they are influenced by it.

- The concept of record class was introduced by Hoare at 1966. Simula adopts the concepts and made some modification. Simula 67 comes only one year after this, must say they are pretty efficient.

- So many modern languages unify the conecpt of type and class, it's a thing at this time already. There were a proposal to unify type and class but got rejected.

- Simula has this weird DES thing, essentially a stochastic lib lets you to write simulation program easier. E.g you want to simulate Brownian motion, but large part of specifying brownian motion is to describe the statistic model, like how one particle moves etc. DES abstract out this part out. Today in R you can find similar system. There is a package literaly called DES.

- Class system provides a easier way to describe subtype relationship.

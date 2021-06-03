# Smalltalk

- famous Alan Kay bashing c++ talk: https://www.youtube.com/watch?v=oKg1hTOQXoY
- little subtyping comparison http://staff.ustc.edu.cn/~yuzhang/fopl/2018s/notes/subtyping.pdf

- According to Alan Kay: Computer as a meta media to reporgram human brain, the expressive power helps us to understand things better.

- I was trying to understand what Alan Kay really mean by his OOP. He likes to make a lot of metaphor, but it's hard to find a concrete example of what oop really is.

- For instance he always make the cell analogy -- think each cell as a small computer and talk to each other. The rationale is biological structures are decentralized and can scale easily.

- Also he talks a lot about the success of Internet is based on it's decentralized nature. He describes "You internet will never break".

- From these two evidences one can imagine oop is something about homogenous objects talk to each other (homogenous in terms of they way they communicate, each object can do it's own thing).

- He said when he coined the word oop, he didn't have C++ in mind. What c++ has is a Simula like class system, (inheritance + dynamic dispatching). If this is not what he want, what gives?

- He emphasis a lot on the meta circular nature. In a talk he recommanded "Art of meta object protocol", and described it as the best book on object orientation. The problem is how Mop relates to oop if we think oop as objects talk to each other?

- In smalltalk literally everything are objects. Class are also objects, numbers are objects. If you want to calculate abs of an object you send a message to it. It's in the smallest scale of the langauage.

- If you imagine you build program with this model, no matter how large the program becomes, it will still be an object. The structure is kinda recursive. For instance imagine there is a compielr written in smalltalke: The compiler is an object -> lexer of the compiler is an objet / parser is an object / codegen is an object -> function is an object / number is an object -> ... There needs to be a base case of this chain. I think this is why meta object protocol is so important for Alan Kay.

- Follow this argument it kinda make sense why Java model is not Alan Kay's model. It failes to convey the meta circular nature of objects.

- Object talk with each other in messages, details of objects are irrelevant in larger scale. Does it looks like... category theory?? (hmm maybe not.)

- Side: Smalltalk's oop model doesn't conflict with functional programming, as common lisp today also get equipped with CLOS. However, class system introduces subtyping into the language, so it's hard to move the same model into language with HM based type system, as parametric polymorphism has some tricky interaction with variances. There is object haskell, but I didn't look into that yet.

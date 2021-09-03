# Glossary (Logic)
PL really is formal logic in disguise. It's good to clearly distinguish words ppl use in literatures, especially those are almost the same in ordinary meaning but have vastly different meanings in logics.

## Intro
- `Judgement`: A logic statement in metalangauge.
  - sym: Assertion.

- `proposition`: A declarative sentence that maybe true, false, or unknown.
  - sym: Statement
  - NOTE1: difference between proposition and statement is some metaphysics, you can treate them the as the same in actual formal system.

- `Argument`: A group of propositions which one follows another.
  - NOTE1: Not to mix with inference, which is a process of reasoning, forming new propositions from known one.
  - NOTE2: Argument is just a collection of propositions, some propositions can support others but it's not necessary.
  - NOTE3: Argumemt can have different meaning in different context.

- `Conclusion`: The proposition that is supported by other propositions in an argument.

- `Premises`: Propositions that support a conclusion in an argument.

- `deduction`: Deductive argument provides constructive ground for it's conclusion.
  - NOTE1: if we know all premises are true leads to conclusion is true, then a deductive argument is valid.
  - NOTE2: deductive logics aim to distiguish arguments that are valid and those that are not.
  - NOTE3: from general to specfic

- `induction`: Inductive argument claims its premises give only some degree of probability, but not certainty to its conclusion.
  - NOTE1: from specific to general
  - NOTE2: Conclusions for inductive argument are never certain. (So conclusion is weaker than deductive arguments)
  - e.g: scientific discoveries are inductive arguments. First we get a specific proposition, then generalize.

- `validity`: Property for all deductive argument that: if all premises are true, it will be enought to say the conclusion is true.
  - NOTE1: validity is a propety only relavent to deductive arguments.

- `Extension`

- `Intension`

- `Rule of inference`: A logical form consisting of a function takes premises and return conclusions
  - sym: inference rule; transformation rule
  - NOTE1: Inference rules consits of permises and a conclusion
  - NOTE2: premises and conclusion are themselves Judgements.
  - NOTE3: inference rules usually preserves some truth (semantic propety)
  - NOTE4: inference rules can be in form of function from any formulae to any formulae.
    - This makes inference rules very general. One can either think it is a deduction from premise to conclusion, or as a functioin that maps certain parameters to result.
  - e.g:
    - Modus ponens is a inference rule.
    - Typing rules for lambda calculus are inference rules.
    - Context free grammar's production rules are inference rules.
    - Axioms for abstract machine are inference rule.

- `Natural deduction`: process of expressing logical reasoning by inference rules.
  - NOTE1: inference rules defines rules, natural deduction evaluates them.
  - NOTE2: work from Gentzen (also sequent calculus which is iso with natural deduction)
  - NOTE3: choose the most natural way of reasoning, instead of using axioms as much as possible (as Hilbert style system)
  - NOTE4: we say meta variable A is a proposition first, then it's true or false (or other things)
  - NOTE5: One important aspect of natural deduction is the introduction and elimination rule.
  - NOTE6: Introduction rule introduces a logical connectives from premises, elimination rule remove them from premises.

```
A prop  B prop   A true  B true
--------------------------------∧I
    (A ∧ B) true
```
  - NOTE7: one connective can have multiple introduction rules.

```
   A true                   B true
  -----------             ------------
   A ∧ B true              A ∨ B true
```
  - NOTE8: Elimination rules are dual of introduction rules.
```
   A ∧ B true                A ∧ B true
  ------------ ∧E₁         ------------- ∧E₂
    A true                    B true
```

- `Proposition`: The meaing of a declarative sentence.
  - NOTE1: A proposition means more then just a sentence that can either be true or false.
  - NOTE2: It means the meaning of a sentence, which is separated from any linguistic structures (different languages can have the same proposition)
  - NOTE3: Because of Curry Howard correspondence, type really shouldbe the meaning of some program. (Thus the static semantics).

- `metalanguage`: A language used to describe other languages ()
  - NOTE1: Metalangauge describes an object langauge.
  - NOTE2: Structural of metalangauge is described by metasyntax.
  - NOTE3: All specifications (BNF, Big step semantics, etc.) are some kinds of metalanguage.
  - e.g: BNF is a metalanguage that we can use to describe syntax of a programming langauge.
  - e.g: Lisp macros are meta language, we use them to describe lisp programs.

- `formal system`: An abstract structure for inferring theorems from axioms according to a set of rules.
  - sym: Deductive system.
  - NOTE1: The point is to be able to infer something from some rules. (entailment or inference).
  - e.g Set theory has axioms, mathemataics based on set theory infer theorems from set theory axioms.
  - e.g Small step smantics for Lambda calculus. It has three axioms, the process of reduction will infer a lambda expression to it's normal form according to axioms available.

- `entailment`: When conclusion is the consequence of the premises, we say premises entrails conclusion.
  - sym: Logical consequence
  - NOTE0: entailment implies true premises leads to some conclusion. Inference only need some premises to lead to some conclusion.
  - NOTE1: ⊢ is called turnstile...
  - NOTE2: ⊢ A, I know A is true.
  - NOTE3: P ⊢ Q, from P, I know Q

## Categorical Proposition

## Categorical Syllogisms

## Deduction

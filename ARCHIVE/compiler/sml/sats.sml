(*
 * boolean satisfiability problem
 *  search for values for terms to make a booleam expression true.
 *
 * - Modern SAT solvers are based on resolution. only applies to formulas in
 *   CNF form (conjunctive normal form)
 *   - a clause is a disjunction of literals. e.g
 *      a1 or a2 or ... or an
 *      where a1, ... an are literals.
 *   - CNF has form (and... (clauses...))
 *
 * An example of CNF form:
 *    (p ∨ ~q ∨ r) ∧ (q ∨ r) ∧ (`p ∨ ~q) ∧ r
 * Note implication can be transformed into CNF form only.
 *
 *
 * *)

(* A simple sat solver *)



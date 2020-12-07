type SomeType1 = { a: string, b: string, c: number };
type SomeType2 = { a: string, b: string, c: string };

// linkedlist type.
// type LinkedList<T> = T & { next: LinkedList<T> };


// filer types
type Filter<T, U> = { [K in keyof T]: T[K] extends U ? T[K] : never }[keyof T];

type Number1 = Filter<SomeType1, number>;
type Number2 = Filter<SomeType2, number>;
let n1: Number1 = 1;

// equal type
type Equal<T, U> = T extends U ? (U extends T ? true : false) : false;

// extract array type. pattern matching in type level.
type FlattenIfArray<T> = T extends Array<infer R> ? R : T;
type Unpromisify<T> = T extends Promise<infer R> ? R : T;

// natrual number in type
type Zero = void;
type Succ<N> = { pred: N };
type Nat<N = Zero> = Zero | Succ<N>;
type One = Succ<Zero>;
type Two = Succ<One>;

type isNat<T> = T extends Nat<unknown> ? true : false;

// conditional types.

declare function f<T extends boolean>(x: T): T extends true ? string : number;

let x = f(Math.random() < 0.5);

type TypeName<T> =
  T extends string? "string" :
  T extends number? "number" :
  T extends boolean? "boolean" :
  T extends undefined? "undefined" :
  T extends Function? "function" :
  "object";


type T0 = TypeName<string>;
type T1 = TypeName<"a">;
type T2 = TypeName<true>;


interface Foo {
  propA: boolean,
  propB: boolean,
}

declare function f1<T>(x: T): T extends Foo ? string : number;

function foo<U>(x: U) {
  let a = f1(x);

  let b: string | number = a;
}

// disctributive
type T10 = TypeName<string | (() => void)>;
type T11 = TypeName<string | Array<string> | undefined>;
type T12 = TypeName<Array<string> | Array<number>>;





declare function f<T extends boolean>(x: T): T extends true ? string : number;

{
  // depends on the value of Math.random() < 0.5.
  const x = f(Math.random() < 0.5);
}

namespace TN {
  type TypeName<T> =
    T extends string ? "string" :
    T extends number ? "number" :
    T extends boolean ? "boolean" :
    T extends undefined ? "undefined" :
    T extends Function ? "function" :
    "object";

  type t0 = TypeName<string>;
  type t1 = TypeName<"a">;

  type Mode<T extends "static" | "runtime"> =
    T extends "static" ? Map<string, number> :
    T extends "runtime" ? {url: string, map: Map<number, string>} :
    never;
}

// use distributive conditionals to select types.
// T exntends U ? X : never where T = A | B | C
// will get distributed into
//  | A extends U ? X : never
//  | B extends U ? X : never
//  | C extends U ? X : never
{
  type Diff<T, U> = T extends U ? never : T;
  type Filter<T, U> = T extends U ? T : never;

  type t1 = Diff<'a' | 'b' | 'c' | 'd', 'a' | 'b' | 'c'>;
  type t2 = Filter<'a' | 'b' | 'c', 'a' | 'b'>;

  type NonNullable<T> = Diff<T, null | undefined>;
  function f1<T>(x: T, y: NonNullable<T>) {
    x = y;
    // y = x is not allowed.
  }
}


class Just<T> {
  _kind: "Just";
  value: T;
  constructor(value: T) {
    this.value = value;
  }
}

class Nothing<T> {
  _kind: "Just";
  value: null = null;
  constructor() {

  }
}

type Maybe_<T> = Just<T> | Nothing<T>;

class Maybe<T> {
  data: Maybe_<T>;
  constructor(value: Maybe_<T>) {
    this.data = value;
  }

  // THis is one applcation on Maybe Monad.
  // this logic is insert into every "statement" you are calling,
  // so you never need to check if a value is a Nothing or not.
  // If they are nothing, the whole term reduce to Nothing.
  bind(f: (a: T) => Maybe<T>) {
    if (this.data._kind === "Just") {
      return f(this.data.value);
    } else {
      return new Maybe(new Nothing<T>());
    }
  }
  // You can insert arbitary logic here, so you can use this
  // to control aribitrary processes.
  //
  // It's like statement, but in a controlled manner.

}

type M = Map<number, string>;

// you can mmake many many functions like this
const lookup = (key: number) => {
  return (a: M): Maybe<M> => {
    const val = a.get(key);
    return val ? new Maybe(new Just(a)) : new Maybe(new Nothing);
  }
}

// your logic will be here
function foo() {
  return new Maybe(new Just(new Map([[1, '1'], [2, '2'], [3, '3']])))
    // I write it longer to make it bit clearer
    .bind(s => lookup(1)(s))  // this ok, M will be pipe to next bind
    .bind(s => lookup(2)(s))  // also ok
    .bind(s => lookup(99)(s))  // not ok, the whole chain colapse to Nothing
}

// Same logic as above as oppose to
function bar() {
  const a = new Map([[1, '1'], [2, '2'], [3, '3']]);
  const b = lookup(1)(a);
  if (b.data._kind !== "Just") return new Maybe(new Nothing());
  const c = lookup(2)(b.data.value);
  if (b.data._kind !== "Just") return new Maybe(new Nothing());
  const d = lookup(2)(c.data.value);
  if (d.data._kind === "Just") return new Maybe(new Nothing());
  return new Just(d.data.value);
}

interface Yeet<T> {
  hasval: (a: T) => boolean,
  val: T
}

declare function lookup1(a: Yeet<number>): Yeet<number>;

function good(a: Yeet<number>) {
  if (!a.hasval(1)) return null;
  const b = lookup1(a);
  if (!b.hasval(2)) return null;
  const c = lookup1(b);
  if (!c.hasval(99)) return null;
  const d = lookup1(b);
  return d;

}

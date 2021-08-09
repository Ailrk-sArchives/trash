// this is a generic identity function
const identity = <T>(arg: T) => arg;

// type signature of identity
const genericIdentity: <T>(arg: T) => T = identity;

// turn g func into object
interface GenericIdentity<T> {
  (arg: T) : T;
};

// now numIdentity only instaintiated for one type.
// A workaround for c++ template.
const numIdentity: GenericIdentity<number> = identity;

interface GenericIdentityFn {
  <T>(arg: T): T;
}

const myIdentity: GenericIdentityFn = identity;

class GenericNumber<T> {
  zeroValue: T;
  add: (x: T, y: T) =>T;
}


const myGenericNumber = new GenericNumber<number>();


// Generic constraints.

interface Lengthwise {
  length: number,
}

function loggingIdentity<T extends Lengthwise>(arg: T): T {
  console.log(arg.length);
  return arg;
}

// use type parameters in Generic constraints.

const getProperty = <T, K extends keyof T>(obj: T, key: K) => {
  return obj[key];
}

const x = {a: 1, b: 2, c: 3, d: 4};
getProperty(x, "a");


class Animal {
  numLegs: number;
}

class BeeKeeper {
  hasMask: boolean
}

class Bee extends Animal {
  keeper: BeeKeeper;
}

const createInstance = <A extends Animal>(c: new() => A): A => {
  return new c();
}

createInstance(Bee).keeper.hasMask = true;



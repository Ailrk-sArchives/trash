function pluck<T, K extends keyof T>(o: T, propertyNames: K[]): T[K][] {
  return propertyNames.map(n => o[n]);
}

interface Car {
  manufacturer: string,
  model: string,
  year: number,
}

{
  let taxi: Car = {
    manufacturer: "Toyota",
    model: "Camry",
    year: 2014,
  };

  let makeAndModel: string[] = pluck(taxi, ["manufacturer", "model"]);
}

// index signatures (index can have types too.)
{
  interface Dictionary<T> {
    [key: string]: T,
  }

  let keys: keyof Dictionary<number>;
  let value: Dictionary<number>["foo"];

  interface Dictionary1<T> {
    [key: number]: T,
  }

  let value2: Dictionary1<number>[2];

}

// map types
type MyReadonly<T> = {
  readonly [P in keyof T]: T[P]
}

type MyPartial<T> = {
  [P in keyof T]?: T[P]
}

type PartialWithOneTwoThree<T> = {
  [P in keyof T]?: T[P]
} & {one: number, two: string, three: boolean}

type MyProxy<T> = {
  get(): T,
  set(value: T): void,
}

type Proxify<T> = {
  [P in keyof T]: MyProxy<T[P]>
}

function proxify<T>(o: T): Proxify<T> {
  let result = {} as Proxify<T>;
  for (const k in o) {
    result[k] = {
      get: () => o[k],
      set: value => o[k] = value,
    }
  }
  return result;
}

interface FishHouse {
  fish: string,
  shrimp: string,
}

const proxiedFish = proxify({fish: "cod", shrimp: "arctic scale shrimp"});

type MyPick<T, K extends keyof T> = {
  [P in K]: T[K]
}

// this map is not homomorphic.
type MyRecord<K extends keyof any, T> = {
  [P in K]: T
}




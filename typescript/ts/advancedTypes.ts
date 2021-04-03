// Intersection types
const extendw = <F, S>(first: F, second: S): F & S => {
  const result: Partial<F & S> = {};
  for (const prop in first) {
    if (first.hasOwnProperty(prop)) {
      (result as F)[prop] = first[prop];
    }
  }

  for (const prop in second) {

    if (second.hasOwnProperty(prop)) {
      (result as S)[prop] = second[prop];
    }
  }

  return result as F & S;
}


// union
interface Layable {
  layEggs: () => string,
}
interface Bird extends Layable {
  fly: () => void,
}

interface Fish extends Layable {
  swim: () => void,
}

const getSmallPet = (which: string): Fish | Bird => {

  if (which === "bird")
    return {
      fly: () => console.log("flying"),
      layEggs: () => "1 egg"
    } as Bird;
  else return {
    swim: () => console.log("swimming"),
    layEggs: () => "100eggs"
  } as Fish;

};


// typeguard
let pet = getSmallPet("fish");

function isFish(pet: Fish | Bird): pet is Fish {
  return (pet as Fish).swim !== undefined;
}

if (isFish(pet)) pet.swim(); else pet.fly();


function move(pet: Fish | Bird) {
  if ("swim" in pet) {
    return pet.swim();
  }
  return pet.fly();
}


// type aliases

type Name = string;
type NameResolver = () => string;

type NameOrNameResolve = Name | NameResolver;
const getName = (n: NameOrNameResolve): Name => {
  if (typeof n === "string") {
    return n;
  }
  else {
    return n();
  }
}

type Container<T> = { value: T};
type Tree<T> = {
  value: T;
  left: Tree<T>;
  right: Tree<T>
};
type LinkedList<T> = T & {next: LinkedList<T>};

interface Person {
  name: string;
}

let people: LinkedList<Person>;

let s = people.name;
s = people.next.name;
s = people.next.next.next.name;


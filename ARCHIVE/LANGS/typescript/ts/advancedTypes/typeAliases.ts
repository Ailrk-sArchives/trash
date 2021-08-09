type Name = string;
type NameResolver = () => string;
type NameOrNameResolver = Name | NameResolver;

function getName(n: NameOrNameResolver): Name {
  if (typeof n === "string") return n;
  else return n();
}

type Container<T> = { value: T};

type Tree<T> = {
  value: T,
  left: Tree<T>,
  right: Tree<T>,
}


type LinkedList<T> = T & { next: LinkedList<T> };

interface Person {
  name: string,
}

let people: LinkedList<Person>;


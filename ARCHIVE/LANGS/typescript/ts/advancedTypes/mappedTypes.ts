interface Person {
  name: string,
  age: number,
}

type ReadOnly<T> = {
  readonly [P in keyof T]: T[P];
}

type PartialType<T> = {
  [P in keyof T]: T[P];
}

type PartialWithNewMember<T> = {
  [P in keyof T]?: T[P];
} & { newMember: boolean }

type Keys  = 'option1' | 'option2';
type Flags = { [K in Keys]: boolean };

type NullablePerson = { [P in keyof Person]: Person[P] | null }
type PartialPerson = { [P in keyof Person]?: Person[P] }



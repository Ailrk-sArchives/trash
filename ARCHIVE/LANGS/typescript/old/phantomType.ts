
type FormData1<A> = string;

interface User {
  name: string,
  id: number,
}


interface Company {
  name: string,
  size: number,
  id: number,
}

function foo(u: User, form: FormData1<User>): FormData1<User> {

  return "goo";
}

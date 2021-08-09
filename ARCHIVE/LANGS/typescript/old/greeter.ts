enum Color {
  Red,
  Green,
  Blue,
}

interface Person {
  firstName: string;
  lastName: string;
}

class Student {
  fullName: string;
  constructor(public firstName: string, public middleInitial: string, public lastName: string) {
    this.fullName = firstName + " " + middleInitial + " " + lastName;
  }
}


function greeter(person: Person) {
  let list: Array<number> = [1, 2, 3];
  let tuple: [string, number] = ['hello', 10];
  let color: Color = Color.Blue;
  return "Hello" + person.firstName + " " + person.lastName;
}

function printSomething(): void {

  console.log("hello there");
}

let user = {firstName: "Jane", lastName: "User"};

document.body.textContent = greeter(user);

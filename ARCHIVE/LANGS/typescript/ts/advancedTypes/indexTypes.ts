function pluck<T, K extends keyof T>(o: T, propertyNames: K[]): Array<T[K]> {
  return propertyNames.map(n => o[n]);
}

interface Car {
  manufacturer: string,
  model: string,
  year: number,
}

let taxi: Car = {
  manufacturer: 'Toyota',
  model: 'Camry',
  year: 2014
}

let makeAndModel: Array<string> = pluck(taxi, ['manufacturer', 'model']);
let modelYear: Array<string | number> = pluck(taxi, ['model', 'year']);

let carProps: keyof Car;

carProps = "model";



interface Dictionary<T> {
  [key: string]: T,
}

let d: Dictionary<number> = { 2: 10 };
let keys: keyof Dictionary<number>;

let value: Dictionary<number>[2] = d[2];
console.log(d);
console.log(value);



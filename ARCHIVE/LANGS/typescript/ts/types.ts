// basic types

const isDone: boolean = false;
const decimal: number = 23;
const color: string = "blue";

const age: number = 45;
const sentence: string = `I am ${age} years old`;


const list: Array<number> = [1, 3, 4];

const tuple: [string, string] = ["one", "two"];

const o: string = tuple[1][2].toString();


enum Color {Red = 1, Green, Blue};
const c: Color = Color.Red;

const notSure: any = 5;

const warnYou = (): void => { console.log('Warn you!'); }

const error = (message: string): never => { throw new Error("e");}

declare function create(o: object | null): void;

const someVal: any = " string";
const strLen: number = (someVal as string).length;









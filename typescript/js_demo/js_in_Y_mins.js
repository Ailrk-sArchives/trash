console.log(Infinity);
console.log("hello world".charAt(2));
console.log("hello world".substring(0, 5));
console.log("Hello".length);

// null -> no value
// undefined -> value is not currently present.
console.log(null == undefined);

let val = 5;
val += 10;

let array = [val, "hello", true];

// only direction different.
array.push(23);
array.pop();
array.unshift(32);
array.shift();

console.log(array.join(';'));
console.log(array.slice(3, 4));

let obj = {k1: "Hello", k2: "World"};
console.log(obj["k1"]);

Math.abs(10);

// oop
let constructor = function() {
  this.num = 5;
}

constructor.prototype.do = function() {
    return this.num + 2;
}

let o = new constructor();

console.log(o.do());


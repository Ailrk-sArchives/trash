// 'use strict';
const length = 20;
function fn() {
  this.a = 10;  // add {a: 10} to this no matter what this is.
  // here is actually global (module scope).
  console.log("== this is in fn in obj3 >");
  console.log(this.length);
  console.log(this);
  console.log("== this is in fn in obj3 <");
}

const obj3 = {
  length: 5,
  method: function (fn) {
    console.log("== in obj3 method: ");
    console.log(this);
    console.log(arguments);
    // binds to
    fn();
    // call as argument, this binds to argument.this
    arguments[0]();

    fn.apply(Object.apply);
    console.log("== out of obj3 method <");
  }
}
obj3.method(fn, 2, 3);


// // empty module global, {}
// console.log("all: ", this);

// function ff() {
//   console.log("!!");
//   // this.pp = "pp";
//   console.log(this);
// }
// // just this
// ff();

// // this binds to array.
// const array = [ff, 10, 20];
// array[0]();

// //  this binds to obj
// const obj = {
//   n: 10,
//   ff: ff,
// };
// obj.ff();

// // dynamic scope
// const objobjobj = {
//   flag: 77,
//   objobj: {
//     flag: 99, obj: {flag: 111, ff: ff, }
//   },
// };
// objobjobj.objobj.obj.ff();


// {
//   // this for global nodejs scope.
//   function addToGlobalThis() {
//     this.a = 88;
//   }
//   function addToGlobalThis1() {
//     this.b = 22;
//   }
//   addToGlobalThis();
//   addToGlobalThis1();
// }

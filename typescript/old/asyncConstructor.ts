/* async function */

// An async function is a function declared with the async keyword.
// Async functions are instances of AsyncFunction constructor, and
// await keyword is premitted within them. THe async and await keywords
// asynchronous, promise-based behavior to be written in a cleaner
// style, avoiding the need to explicitly configure promise chains.


/* AsyncFunction */

// async Syntax create  a new AsyncFunction
// AsyncFunction is not a global object. It can be obtained with
// Object.getPrototypeOf(async function () {})
//
// async function objects created with the AsyncFunction constructor are
// paresd when the function is created. This is less effiecient than
// declaring an async function with an async function expression and
// calling it within your code, because such functions are parsed
// with the rest of the code.

function resolveAfter2Seconds(x: string) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve(x);
    }, 2000);
  });
}

async function asyncCall(a: string) {
  console.log("calling");
  let result = await resolveAfter2Seconds(a);
  result = await resolveAfter2Seconds(a);
  console.log(result);
  console.log(result);
}

asyncCall("async happens here");
// this will run first. no blocking.
console.log("sync happens here");

// Note, async functions created with the AsyncFunction constructor do not
// create closures to their creation contexts; they are always created in
// the global scope.

// When running them, they will only be able to axess their own local
// variables and global ones, not the ones
// This is different from using eval with code for an async function
// expression.

// promise creater.
// let AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;

// let a = new AsyncFunction('a',
//   'b',
//   'return await resolveAfter2Seconds(a) + await resolveAfter2Seconds(b);');

// // print 30 after 4 seconds.
// a(10, 20).then((v: string) => {
//   console.log(v);
// });

export {}

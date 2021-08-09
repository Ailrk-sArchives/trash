// The Promise object represents the eventual completion (or failure)
// of an asynchornous operation, and its resulting value.

// A Promise is a proxy for a value not necessarily known when the
// promise is created. It allows you to associate handler with an
// asynchronous actions eventual success value or failure reason. This
// lets asynchronous methods return values like synchronous methods:
// instead of immediately returning the final value, the asynchronous
// method returns a promise to supply the value at some point in the
// future.

// A Promise is in one of these states:
//  - Pending initial state, neither fulfilled nor rejected.
//  - fulfilled: meaning that the operation completed successfully.
//  - rejected: meanig that the operation failed.

// A pending promise can either be fullfilled with value, or rejected
// with a reason (error). When either of theses options happens, the associated
// handlers queued up by a promise's then method are called. (If the promise has
// already been fulfilled or rejected when a corresponding handler is attached,
// the handler will be called, so there is no race condition between an
// asynchronous operation completing and its handler being attached.)

// As the Promise.prototype.then() and Promise.prototype.catch() methods return
// promise promises, they can be chained.

//                    |-- async actions
//          fulfilled |
//          |--- .then() -------------|
// Promise --                         |-- Promise -> next.
//          |--- .then().catch() -----|
//          rejected  |
//                    |-- error handling

// Chained Promises
// The methods promise.then(), promise.catch(), and promise.finally() are
// used to associate further action with a promise that becomes settled. These
// methods also return a mewly generated promise object, which can optionally be used for chaining;

// Handling a rejected promist too early has consequences further down the
// promise chain. Sometimes there is no chocie, because an error must be handled
// immediately. (See throw -999) in the example, below, for a technique to handle
// the consequences.) On the other hand, in the absence of an immediate need,
// it is simpler to leave out error handling until a final .catch() statement.

// The signatures of these two functions are simple, the accept a sigle
// parameter of any type. These functions are written by you, the programmer.
// The termination condition of these function determines the "settled" state
// of the next promise in the chain. Any termination other than a throw creates
// a "resolved" state, while terminating with a throw create a "rejected" state.
// handleFullfilled(value) { ... return nextValue; }
// handleRejection(reason) { ...; throw nextReason; }
// handleRejection(reason) { ...; return nextReason; }
// The returned nextValue can be another promise object, in which case the
// promise gets dynamically inserted into the chain.
//
// When a .then() lacks the appropriate function, processing simply continues to
// the next link of the chain. THerefore, a chain can safely omit every
// handleRejectin until the final .catch. Similarly, .catch is really just
// .then() without a slot for handleFullfilled.

// The promise of a chain are nested like Russian dolls, but get popped like the
// top of a stack. The first promise in the chain is most deeply nested and
// first to pop.
//
// (promise D, (promise C, (promise B, (promise A))))
//
// When a nextValue is a promise, the effect is a dynamic replacement. The
// return causes a promise to be poped, but the nextValue promise is pushed into
// its plaxe. For the nesting shown above, suppose the .then() assiciated with
// promise B return a nextValue of promise ...

function promise_chain() {
  const promiseA = new Promise(() => 1);
  const promiseB = promiseA
    .then((e: number) => e + 1, () => {console.log("error")});

  promiseB.then(() => {
    console.log("hi");
  });
}

// Promise all
// wait for all promise to be resolved, or for any to be rejected.
// it the returned promise resolves, it is resolved with a array of
function promise_all() {
  const p1 = Promise.resolve(3);
  const p2 = 42;
  const p3 = new Promise((resolve, reject) => {
    if (resolve == undefined) {
      reject();
    }
    setTimeout(resolve, 100, 'foo');
  });
  Promise.all([p1, p2, p3]).then((values: Array<any>) => {
    for (let v of values) console.log(v);
  });
}

// Promise race
// fulfill or reject as long as one of the promise fulfill or reject.
async function promise_race(): Promise<string> {
  return Promise.race<Promise<string>>([
    new Promise((resolve) => {
      setTimeout(resolve, 500, 'one');
    }),
    new Promise((resolve) => {
      setTimeout(resolve, 100, 'two');
    }),
  ]).then(value => {
    console.log(value);
    return value;
  });
}

export {}

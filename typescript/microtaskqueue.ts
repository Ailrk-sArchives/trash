
setImmediate(() => console.log("immediate1"));
setImmediate(() => {
  console.log("immediate2");
  // in node version higher than 11 promise queue is visited right after every setImmediate call.
  Promise.resolve().then(() => console.log("promise resolve"));
});
setImmediate(() => console.log("Immediate3"));
setImmediate(() => console.log("Immediate4"));


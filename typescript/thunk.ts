// sync thunk.
function add(x: number, y: number) {
  return x + y;
}
const thunksync = () => add(10, 20);
console.log(thunksync());

// async thunk
function addAsync(x: number, y: number, cb: (arg: number) => number) {
  setTimeout(() => {
    cb(x + y);
  }, 200);
}
const thunkasync = (cb: (arg: number) => number) => {
  addAsync(10, 15, cb);
}
console.log(thunkasync(val => val * 100));

export {};

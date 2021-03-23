type AsyncIf = (predicate: () => boolean, callback: Function) => void;

const timeout = async (ms: number) => new Promise(res => setTimeout(res, ms));
const asyncIf = async (predicate: () => boolean, callback: Function) => {
  while (predicate() === false) await timeout(50);
  callback();
}

((callback: AsyncIf) => {
  callback(() => a,
    () => console.log("do something"));
})(asyncIf);

let a = false;
setTimeout(() => {
  a = true;
  console.log('outside: ' + a);
}, 1000);

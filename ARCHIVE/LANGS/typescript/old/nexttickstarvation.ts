import fs from 'fs';


function addNextTick(count: number) {
  let self: {id: undefined | number} = this;

  // issue here. No way to change type at runtime.
  if (self?.id === undefined) {
    self.id = 0;
  }

  if (self.id === count) return;
  process.nextTick(() => {
    console.log(`process.nextTick call ${++self.id}`);
    addNextTick.call(self, count);
  })
}

addNextTick(Infinity);
setTimeout(console.log.bind(console, 'setTimeout'), 2);
setImmediate(console.log.bind(console, 'setImmediate'));
fs.readFile(__filename, () => {
  console.log('file read');
});


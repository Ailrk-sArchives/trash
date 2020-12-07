import EventEmitter from 'events';
import process from 'process';
class MyEmitter extends EventEmitter.EventEmitter {};

const syncEmmiter= new MyEmitter();
const asyncEmmiter= new MyEmitter();

asyncEmmiter.on('asyc', (a: string) => {
  process.nextTick(() => {  // async invocation.
    console.log(`an event occured, handled asycrounously ${a}`);
    console.log("asd");
  });
});

asyncEmmiter.emit('async', 'gozila');

syncEmmiter.on('sync', (a: string) => {
  console.log(`an event occured, handled syncrounously ${a}`);
});

syncEmmiter.emit('sync', 'mozila');

import {Worker, isMainThread, parentPort, workerData} from 'worker_threads';
import * as http from 'http';

function runNaive() {
  if (isMainThread) {

    const worker = new Worker(__filename, {workerData: {num: 6}});
    worker.once('message', result => {
      console.log('square of 5 is:', result);
    });
  } else {
    const num: number = workerData.num;
    http.get('http://www.google.com', res => {
      parentPort.postMessage(`${res.read(100)}${num * num}`);
    })
  }
}

runNaive();

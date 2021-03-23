"use strict";
exports.__esModule = true;
var worker_threads_1 = require("worker_threads");
var http = require("http");
function runNaive() {
    if (worker_threads_1.isMainThread) {
        var worker = new worker_threads_1.Worker(__filename, { workerData: { num: 5 } });
        worker.once('message', function (result) {
            console.log('square of 5 is:', result);
        });
    }
    else {
        var num_1 = worker_threads_1.workerData.num;
        http.get('http://www.google.com', function (res) {
            worker_threads_1.parentPort.postMessage("" + res.read(100) + num_1 * num_1);
        });
    }
}
runNaive();

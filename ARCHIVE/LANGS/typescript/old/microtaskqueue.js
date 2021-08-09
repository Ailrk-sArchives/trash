setImmediate(function () { return console.log("immediate1"); });
setImmediate(function () {
    console.log("immediate2");
    Promise.resolve().then(function () { return console.log("promise resolve"); });
});
setImmediate(function () { return console.log("Immediate3"); });
setImmediate(function () { return console.log("Immediate4"); });

"use strict";
exports.__esModule = true;
var fs_1 = require("fs");
function addNextTick(count) {
    var self = this;
    if (self.id === undefined) {
        self.id = 0;
    }
    if (self.id === count)
        return;
    process.nextTick(function () {
        console.log("process.nextTick call " + ++self.id);
        addNextTick.call(self, count);
    });
}
addNextTick(Infinity);
setTimeout(console.log.bind(console, 'setTimeout'), 2);
setImmediate(console.log.bind(console, 'setImmediate'));
fs_1["default"].readFile(__filename, function () {
    console.log('file read');
});

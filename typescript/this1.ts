// 'use strict';
const length = 10;
function fn() {
  // here is actually global (module scope).
  console.log("in fn this.lenth:   |  ", this.length);
  console.log("in fn this:         |  ", this);
}

const obj3 = {
  length: 5,
  method: function (fn) {
    console.log("this:               |  " + this);
    console.log("arguments:          |  " + arguments);
    console.log("arguments.length:   |  " + arguments.length);
    console.log("arguments[0]:       |  " + arguments[0]);
    console.log("arguments[0].length:|  " + arguments[0].length);
    console.log("arguments[0].this:  |  " + arguments[0].this);
    // binds to
    fn();
    console.log("========");
    // call as argument, this binds to argument.this
    arguments[0]();

    fn.apply(Object.apply);
  }
}
obj3.method(fn, 2);

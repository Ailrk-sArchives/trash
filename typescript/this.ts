// global this
this.pp = 10;
this.data = "global";

const obj1 = {
  data: "obj1",
  f1: () => {
    console.log(this);  // global this
  }
}

const obj2 = {
  data: "obj2",
  f2: function () {
    console.log(this);
  }
}

// f2 will rebind itself, nw this is global
const f3 = obj1.f1;

const f4 = obj2.f2;
const f5 = obj2.f2;
const f6 = obj2.f2;
f5.bind({data: "rebinded data"});
f5.bind(obj1);

// arrow
obj1.f1();

// regular function
obj2.f2();

// arrow method get assigned.
f3();

// regular function get assigned and rebind with different obj.
f5();
f6();

// // regular function get assigned.
// f4();

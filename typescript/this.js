var _this = this;
var obj1 = {
    f1: function () {
        console.log(_this.NaN); // global
    }
};
var obj2 = {
    data: "in obj2",
    f2: function () {
        console.log(this.data);
    }
};
var data = "peepee";
// f2 will rebind itself, nw this is global
var f3 = obj2.f2;
obj1.f1();
obj2.f2();
f3();

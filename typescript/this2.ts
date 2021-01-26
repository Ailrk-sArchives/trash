function ggg() {
  this.a = 10;
}
ggg();

function fff() {
  this.a--;
  if (typeof this === "function") {
    console.log(this.a);
    if (this.a !== 0) {
      this.apply(this);
    }
  } else {
    console.log(this);
  }
  let v = new Function();
}

fff();
// fff.bind();
fff.apply(fff());

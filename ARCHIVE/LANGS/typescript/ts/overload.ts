interface Show {
  show(d: number): string,
  show(): string
}

class Point implements Show {
  constructor(public x: number, public y: number) {}

  show(d?: number) {
    if (d && typeof d === "number") {
      return `Point ${d}`;
    } else {
      return "Point";
    }
  }
}

let p = new Point(1, 2);
console.log(p.show(1));
console.log(p.show());

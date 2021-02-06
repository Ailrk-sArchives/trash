interface Square {
  kind: "square",
  size: number,
}

interface Rectangle {
  kind: "rectangle",
  width: number,
  height: number,
}

interface Circle {
  kind: "circle",
  radius: number,
}

function asserNever(x: never): never {
  throw new Error("Unexpected object:" + x);
}

type S = Square | Rectangle | Circle;

function sarea(s: S): number {
  switch (s.kind) {
    case "square":
      return s.size * s.size;
    case "rectangle":
      return s.width * s.height;
    case "circle":
      return Math.PI * s.radius ** 2;
    default:
      // exthaustiveness check.
      assertNever(s);
  }
}

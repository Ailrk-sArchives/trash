function padLeft(value: string, padding: string | number) {
  if (typeof padding === "number") {
    return Array(padding + 1).join(" ") + value;
  }

  if (typeof padding === "string") {
    return padding + value;
  }

  throw new Error(`Expected string or number, got ${padding}.`);
}


console.log(padLeft("Hello", "World"));
console.log(padLeft("Hello", 4));

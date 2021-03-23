
function* numberGen(start: number, end: number) {
  for (let i = start; i < end; i++)
    yield i;
}

const generator = numberGen(10, 20);


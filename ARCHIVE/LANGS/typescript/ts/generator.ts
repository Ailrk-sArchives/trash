function* idGen() {
  let i = 0;
  while (true)
    yield ++i;
}


const gen: Generator<number> = idGen();

for (let i = 0; i < 10; i++) {
  console.log(gen.next().value);
}

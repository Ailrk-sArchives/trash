const join = async <T>(p: Promise<Promise<T>>) => {
  return await p;
};

const unify = async<T>(p: Promise<T>) => {
  const v = await p;
  if (v instanceof Promise) {
    unify(join(v));
  }
  return v;
}

const nest = async <T>(t: T): Promise<T> => {
  return new Promise((resolve, _) => {
    setTimeout(() => {
      resolve(t);
    }, 300);
  });
}

const foo = async (): Promise<number> => {
  return new Promise((resolve, _) => {
    setTimeout(() => {
      resolve(1);
    }, 300);
  });
}

const main = async () => {

  const nested = nest(nest(nest(foo())));
  const a = await unify(nested);
  console.log(a);
};


(async () => {
  try {
    console.log("good");
    await main();

  } catch (e) {

  }
})();

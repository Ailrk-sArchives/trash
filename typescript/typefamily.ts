interface Parser<T> {
  runParser: (stream: string) => Array<[T, string]>
}


type Fmap<T, A, B> = (a: T<A>) => T<B>;

interface Functor<T> {
  fmap: (f: ())
}

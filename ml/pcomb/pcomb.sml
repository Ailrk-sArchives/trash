signature PCOMB =
sig
  datatype parserError = EOF
                       | Unexpected of string;

  datatype 'a presult = PResult of 'a * string
                      | PError of (parserError * string);

  datatype 'a parser = Parser of (string -> 'a presult);

  val runParser : 'a parser -> (string -> 'a presult)
  val <$> : ('a -> 'b) * 'a parser -> 'b parser
  val <*> : ('a -> 'b) parser * 'a parser -> 'b parser
  val pure : 'a -> 'a parser
  val >>= : 'a parser * ('a -> 'b parser) -> 'b parser
  val >> : 'a parser * 'b parser -> 'b parser
  val <|> : 'a parser * 'a parser -> 'a parser
  val many : 'a parser -> 'a list parser
  val some : 'a parser -> 'a list parser
  val item : char parser;
  val satisfy : (char -> bool) -> char parser
  val ch : char -> char parser
  val digit : int parser
  val space : char parser
  val str : string -> string parser
end

structure Parser :> PCOMB =
struct
  datatype parserError = EOF
                       | Unexpected of string;

  datatype 'a presult = PResult of 'a * string
                      | PError of (parserError * string);

  datatype 'a parser = Parser of (string -> 'a presult);

  fun runParser (Parser p) = p;

  infix 2 <$>;
  fun f <$> (Parser p) = Parser (fn s =>
    case p s of
         PError e => PError e
       | PResult (a, s') => PResult (f a, s'));

  fun pure a = Parser (fn s => PResult (a, s));

  infix 2 <*>;
  fun (Parser fm) <*> (Parser m) = Parser (fn s =>
    case fm s of
         PError e => PError e
       | PResult (f, s') =>
           case m s' of
                PError e => PError e
              | PResult (v, s'') => PResult (f v, s''));

  infixr 1 >>=;
  fun (Parser ma) >>= f = Parser (fn s =>
    case ma s of
         PError e => PError e
       | PResult (a, s') =>
           case runParser (f a) s' of
                PError e => PError e
              | PResult (b, s'') => PResult (b, s''));

  infixr 1 >>;
  fun ma >> mb = ma >>= (fn _ => mb);

  infix 1 <|>;
  fun (Parser a) <|> (Parser b) = Parser (fn s =>
    case a s of
         PError _ => b s
       | PResult (v, s') => PResult (v, s'))

  fun cons a b = a :: b;

  fun many v = (cons <$> v <*> many v) <|> pure [];
  fun some v = cons <$> v <*> many v;

  val item = Parser (fn s =>
    if String.size s = 0
    then PError (EOF, s)
    else
      let
        val head = String.sub (s, 0);
        val tail = String.extract (s, 1, NONE);
      in PResult (head, tail) end);

  fun satisfy pred = Parser (fn s =>
    case runParser item s of
         PError e => PError e
       | PResult (v, s') =>
           if pred v
           then PResult (v, s')
           else PError (Unexpected ("unexpted character " ^ Char.toString v) ,s'))

  fun ch c = satisfy (fn c' => c = c');
  val digit = (fn n => Char.ord n - Char.ord #"0") <$> satisfy Char.isDigit;
  val space = satisfy Char.isSpace;

  fun str s =
    let
      val slist = String.explode s;
      fun str' (s' : char list) =
        case s' of
             [] => pure ""
           | (x::xs) => ch x >> str' xs >> pure s
    in str' slist end

 end

structure Test =
struct
  open Parser;
  fun test_ch () =
    let
      val p1 = ch #"a";
      val result = runParser p1 "ab"
    in result end

  fun test_str () =
    let
      val p1 = str "string"
      val result = runParser p1 "string to parse"
    in result end

  fun test_digit () = runParser digit "1"
end

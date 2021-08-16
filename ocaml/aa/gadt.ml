type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n -> n
  | Add -> (fun x y -> x + y)
  | App (f, x) -> (eval f) (eval x)

let two = eval (App (App (Add, Int 1), Int 1))

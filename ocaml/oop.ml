(* ocaml is object oriented imperative language :)*)

(* polymorphic class *)
class ['a] stack =
  object (self)
    val mutable the_list = ([] : 'a list)

    method push x =
      the_list <- x :: the_list

    method pop =
      let result = List.hd the_list in
      the_list <- List.tl the_list;
      result

    method peek =
      List.hd the_list

    method size =
      List.length the_list
  end;;

(* empty a stack *)
let drain_stack (s : 'a stack) =
  while s#size > 0 do
    ignore (s#pop)
  done;;

(* inheritance, virtual classes, initializer *)

(* virtual class, or abstract class that defines the virtual function *)

(* superclass for all widgets *)
class virtual widget (name : string) =
  object (self)
    method get_name =
      name

    method virtual repaint : unit
  end;;

(* any widget that can contain other widgets
 * Though it doesn't containt any virtual methods, we mark it
 * virtual to prevent people from creating an instance for it.
 * *)
class virtual container name =
  object(self)
    inherit widget name
    val mutable widgets = ([] : widget list)
    method add w =
      widgets <- w :: widgets
    method get_widgetes =
      widgets
    method repaint =
      List.iter (fun w -> w#repaint) widgets
  end;;

type button_state = Released | Pressed;;

(* callback is an optional argument
 * we inherit container as super so we can refer it as super in the
 * class definition.
 * *)
class button ?callback name =
  object (self)
    inherit container name as super
    val mutable state = Released
    method press =
      state <- Pressed;
      match callback with
      | None -> ()
      | Some f -> f ()
    method release =
      state <- Released
    method repaint =
      super#repaint;
      print_endline("Button being repainted, state is " ^
                    (match state with
                    | Pressed -> "Pressed"
                    | Released -> "Released"))
  end;;

(* create a new object *)
let b = new button ~callback:(fun () -> print_endline "Ouch!!") "button";;
b#repaint;;
b#press;;
b#repaint;;

(* objects without class (structual subtyping) *)
let o =
  object
    val mutable n = 0
    method incr = n <- n + 1
    method get = n
  end;;

(* equivalent record representation *)
type counter_r = {
  get : unit -> int;
  incr : unit -> unit;
};;

(* full equivalency.
 * functions close on the same state *)
let r =
  let n = ref 0 in
  { get = (fun () -> !n);
    incr = (fun () -> incr n);
  }

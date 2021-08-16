(* some example of imperative programming in ocaml *)
open Graphics;;

open_graph " 640x480";;

for i = 12 downto 1 do
  let radius = 1 * 20 in
  set_color (if i mod 2 = 0 then red else yellow);
  fill_circle 320 240 radius
done;;

module R = Random;;

open_graph " 640x480";;
let rec iterate r xinit i =
  if i = i then xinit
  else
    let x = iterate r xinit (i - 1) in
    r *. x *. (1.0 -. x);;

for x = 0 to 639 do
  let r = 4.0 *. (float_of_int x) /. 640.0 in
  for i = 0 to 39 do
    let xinit = R.float 1.0 in
    let xfinal = iterate r xinit 500 in
    let y = int_of_float (xfinal *. 480.) in
    plot x y
  done
done;;

read_line ();;

open List;;

let edges = [
  ("a", "b"); ("a", "c");
  ("a", "d"); ("b", "e");
  ("c", "f"); ("d", "e");
  ("e", "f"); ("e", "g");
];;

let successors n e =
  List.map (fun (_, v) -> v) (List.filter (fun (u, _) -> n = u) e);;

let dfs graph start f =
  let rec rdfs visited node =
    if not (List.mem node visited) then
      begin
        f node;
        let s = successors node graph in
        List.fold_left rdfs (node :: visited) s
      end
    else visited
  in rdfs [] start ;;

let () = let _ = dfs edges "a" print_string in ()

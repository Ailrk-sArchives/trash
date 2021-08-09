let main = { pkgs ? import <nixpkgs> { } }:
  let
    xs = [ 1 2 3 4 5 ];
    t1 = builtins.typeOf [ ];
    t2 = builtins.foldl' (a: b: a + b) 0 xs;
  in
  [ t1 t2 ];
in
main { pkgs = (import <nixpkgs>); }

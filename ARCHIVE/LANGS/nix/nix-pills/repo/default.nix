let
  nixpkgs = import <nixpkgs> {};
  allpkgs = nixpkgs // pkgs;
  # 2020-04-29
  # callPackage pattern
  # to match matchable attributes from set to parameter.

  # path refers to the path of the nix expression function.
  callPackage = path: overrides:
    with builtins;
      let f = import path;
      in
        # builtins.intersectAttrs
        # builtins.functionArgs: return attributes of function arguments.
        f (intersectAttrs ((functionArgs f) allpkgs) // overrides);
  pkgs = with nixpkgs; {
    mkDerivation = import ./autotools.nix nixpkgs;
    hello = callPackage ./hello.nix {};
    graphviz = callPackage ./graphviz.nix {};
    graphvizCore = callPackage ./graphviz.nix {gdSupport = false;};
  };
in
  pkgs

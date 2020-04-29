let
  nixpgs = import <nixpkgs> {};
  allpkgs = nixpkgs // pkgs;
  callPackage = path: overrides:
    with builtins;
      let f = import path;
      in
        f (intersectAttrs ((functionArgs f) allpkgs) // overrides);
  pkgs = with nixpkgs; {
    mkDerivation = import ./autotools.nix nixpkgs;
    hello = callPackage ./hello.nix {};
    graphviz = callPackage ./graphviz.nix {};
    graphvizCore = callPackage ./graphviz.nix {gdSupport = false;};
  };
in
  pkgs


let
  nixpkgs = import <nixpkgs> {};
  allpkgs = nixpkgs // pkgs;
  overridelib = import ./overridelib.nix;
  callPackage = path: overrides:
    with builtins // overridelib;
    let
      f = import path;
      ogArgs = (intersectAttrs (functionArgs f) allpkgs) // overrides;
    in
      makeOverridable f ogArgs;
  pkgs = with nixpkgs; {
    mkDerivation = import ./autotools.nix nixpkgs;
    hello = callPackage ./hello.nix {};
    graphviz = callPackage ./graphviz.nix {};
    graphvizCore = callPackage ./graphviz.nix {gdSupport = false;};
  };
in
  pkgs

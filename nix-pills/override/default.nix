let
  nixpkgs = import <nixpkgs> {};
  allpkgs = nixpkgs // pkgs;
  overridelib = import ./overridelib.nix;
  callPackage = path: overrides:
    with builtins // overridelib;
    let
      f = import path;
      # 2020-04-29
      # patch override into the output package.
      # so we can easily override some fields when we only have
      # package alone.
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

# 2020-04-29
# Note we define allpkgs with allpkgs itself
# this works because of lazy evaluation.


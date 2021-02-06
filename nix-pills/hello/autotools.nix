# a generic build nix expression for all autotools.
# it is based on generic_builder.sh,
# and the main purpose is to describe essential
# dependecies for generic_builder.sh

# builder and args can be seem as the IO of this expression
pkgs: attrs:
  with pkgs;    # with bring the name into the scope. like using in cpp
  let defaultAttrs = {
    builder = "${bash}/bin/bash"; # IO, for nix-build
    args = [ ./builder.sh ];
    setup = ./setup.sh;           # setup environment. for nix-shell
    baseInputs = [  # you can also put everything into buildInputs
      gnutar
      gzip
      gnumake
      gcc
      binutils-unwrapped
      coreutils
      gawk
      gnused
      gnugrep
    ];
    buildInputs = [];
    system = builtins.currentSystem; # linux
  };
  in
  derivation (defaultAttrs // attrs)

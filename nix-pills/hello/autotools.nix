# a generic build nix expression for all autotools.
# it is based on generic_builder.sh,
# and the main purpose is to describe essential
# dependecies for generic_builder.sh

pkgs: attrs:
  with pkgs;
  let defaultAttrs = {
    builder = "${bash}/bin/bash";
    args = [ ./builder.sh ];
    setup = ./setup.sh;
    baseInputs = [
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
    system = builtins.currentSystem;
  };
  in
  derivation (defaultAttrs // attrs)

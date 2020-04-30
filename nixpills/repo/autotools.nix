# 2020-04-29
# a generic build nix expression for all autotools.
# based on builder.sh
#

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

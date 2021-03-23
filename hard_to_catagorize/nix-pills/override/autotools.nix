# add dependecies of autotools into the final derivation.
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

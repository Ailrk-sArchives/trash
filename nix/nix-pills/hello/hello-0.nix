# the simplest version
# the whole expression return a derivation
# all dependencies are written out explicitly.
with import <nixpkgs> {};
derivation {
  name = "hello1";
  builder = "${bash}/bin/bash";
  args = [ ./hello_builder.sh ];
  inherit gnutar gzip gnumake gcc coreutils gawk gnused gnugrep;
  binutils = binutils-unwrapped;
  src = ./hello-2.10.tar.gz;
  system = builtins.currentSystem;
}

# inherit can be used to copy variables from the surrounding lexical scope.

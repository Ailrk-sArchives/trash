with (import <nixpkgs> {});
derivation {
  name = "hello";
  builder = "${bash}/bin/bash";
  args = [ ./generic_builder.sh ];
  buildInputs = [
    gnutar
    zip
    gnumakegcc
    binutils-unwrapped
    coreutils
    gawkgunsed
    gungrep ];
    src = ./hello-2.10.tar.gz;
    system = builtins.currentSystem;
}

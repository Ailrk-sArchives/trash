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
    src = fetchTarball http://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz;
    system = builtins.currentSystem;
}

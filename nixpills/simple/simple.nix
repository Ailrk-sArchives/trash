with (import <nixpkgs> {});
derivation {
  name = "simple";
  builder = "${bash}/bin/bash";
  args = [ ./simple_builder.sh ];
  system = builtins.currentSystem;
  inherit gcc coreutils;
  src = ./simple.c;
}

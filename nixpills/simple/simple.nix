
with (import <nixpkgs> {});
derivation {
  name = "simple";
  builder = "${bash}/bin/bash";
  args = [ ./simple_builder.sh ];
  system = builtins.currentSystem;
  inherit gcc coreutils;
  src = ./simple.c;
}

# 2020-04-29
# derivation is the basic data nix program deal with.
# it is a set used to describe the dependencies of a build.
# a derivation has three basic attributes { name, builder, system },
# and there are some other meta attributes works for a derivation.
# all extra attributes you defined will be in the PATH when build.
#

# 2020-04-29
# nix doesn't build by itself, but it rather build up the final derivation
# based on inputs.
# the build only take place when you run nix-build or equivalence.
# It is kinda like haskell, all you program is pure functions, and IO
# happend outside of your program.
#

# 2020-04-29
# with bring attributes in a set into the scope.
# similar to type in Haskell, using in c++
#

# 2020-04-29
# one cool thing about nix is it has a build in path type.
# when you make a path from some randome string, it will not be
# a path type if it is not a valid path.
# An example of DSL design.
#

# 2020-04-29
# note ${bash}/bin/bash in builder attributes.
# bash here refer to bash folder in nix store.
# the exeutable is inside.
#



# 2020-04-29
# easy description of the package dependecies
# using input pattern, somewhat like render props in react.
# your parameter mkDerivation is truely 'polymorphic' in some sense.
# the functionality of it depends on the caller.
#
{ mkDerivation }:
mkDerivation {
  name = "hello";
  src = ./hello-2.10.tar.gz;
}

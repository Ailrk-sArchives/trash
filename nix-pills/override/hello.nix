# easy description of the package dependecies
# based on generic autotools.
{ mkDerivation }:
mkDerivation {
  name = "hello";
  src = ./hello-2.10.tar.gz;
}

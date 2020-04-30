# 2020-04-29
# more interesting than hello.nix
# specifies some input patterns and buildInputs.
#
{ mkDerivation, gdSupport ? true, gd, fontconfig, libjpeg, bzip2 }:
mkDerivation {
  name = "graphviz";
  src = ./graphviz-2.38.0.tar.gz;
  buildInputs = if gdSupport then [ gd fontconfig libjpeg bzip2 ] else [];
}

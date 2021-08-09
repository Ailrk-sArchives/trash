# dependecies for graphviz only.
# in nix it is called inpupt pattern.
# (really it's a reader function)
{ mkDerivation, gdSupport ? true, gd, fontconfig, libjpeg, bzip2 }:
mkDerivation {
  name = "graphviz";
  src = ./graphviz-2.38.0.tar.gz;
  buildInputs = if gdSupport then [ gd fontconfig libjpeg bzip2 ] else [ ];
}

# 2020-04-29
# $out $src are attributes from the derivation.
# when build they will be put in PATH
# nix create sandbox by playing with PATH a lot.
#
export PATH="$coreutils/bin:$gcc/bin"
mkdir $out
gcc -o $out/simple $src

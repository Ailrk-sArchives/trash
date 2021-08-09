# 2020-04-29
# builder only build.
# all environment setup relies on setup.sh
# This make it easier to setup a development
# environment in nix-shell
set -e          # stop when error occur.
source $setup
genericBuild

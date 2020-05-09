

unset PATH              # clean path

# 2020-04-29
# here we concat  baseInputs and buildInputs into one
# it should be paths separated by spaces.
# then convert it to array with ()
# shell is insane.
#
Inputs=${baseInputs}${buildInputs}
Inputs=(`echo ${Inputs}`)

for p in $Inputs; do    # aquire dependencies
    export PATH=$p/bin${PATH:+:}$PATH
done

# 2020-04-29
# Good pattern here
# no matter how trivial the thing is you still
# put it into a function. Just simply name each
# process make things a lot easier.
function unpackPhase() {
    tar -xf $src

    # cd into decompressed directory.
    for d in *; do
        if [ -d "$d" ]; then
            cd "$d"
            break
        fi
    done
}

function configurePhase() {
    ./configure --prefix=$out
}

function buildPhase() {
    make
}

function installPhase() {
    make install
}

function genericBuild() {
    unpackPhase
    configurePhase
    buildPhase
    installPhase
}

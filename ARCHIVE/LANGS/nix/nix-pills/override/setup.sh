unset PATH      # clean path
Inputs=${baseInputs}${baseInputs}
Inputs=(`echo ${Inputs}`)

for p in $Inputs; do   # aquire dependencies
  export PATH=$p/bin${PATH:+:}$PATH
done

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

compile_ () {
  g++ ./specialization"$1".cc
}

dump_ () {
  compile_ $1 && objdump -C -d ./a.out > out."$1".asm
}

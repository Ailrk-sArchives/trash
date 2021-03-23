int k(int a, int b) { return a; }

int abs(int a) {
  if (a >= 0) {
    return k(a, 0);
  }
  return -a;
}

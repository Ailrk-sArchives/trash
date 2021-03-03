
int foo(int a, int b){
  int x = 10 + a;
  return a * b + a;
}

int main(void)
{
  int x = foo(1, 2);
  return 0;
}

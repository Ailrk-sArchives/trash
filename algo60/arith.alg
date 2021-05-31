begin
  comment begin
    Algol 60:
      1. evaluation strategies: call by value, call by name.
  end;

  integer procedure Factorial(i);
  value i;
  integer i;
  begin
    if i <= 1 then
      Factorial := 1
    else
      Factorial := i * Factorial (i - 1)
  end;

  integer procedure FibonacciNaive(i);
  value i;
  integer i;
  begin
    if i <= 1 then
      FibonacciNaive := i
    else
      FibonacciNaive := FibonacciNaive (i - 2) + FibonacciNaive (i - 1)
  end;

  real procedure PiGregoryLeibniz(n);
  value n;
  integer n;
  begin
    integer i, s;
    real q, r;

    s := 1;
    q := 4;
    r := 0;
    for i := 1 step 2 until n do
    begin
      r := r + s * q / i;
      s := -s;
    end;

    PiGregoryLeibniz := r
  end;

  integer i;
  real r;


  begin
    outstring(1, "\n====\n");
    i := 7;
    outstring(1, "Factorial(7) = ");
    i := Factorial(7);
    outinteger(1, i);
  end;


  begin
    outstring(1, "\n====\n");
    i := 16;
    outstring(i, "FibonacciNaive(16) = ");
    i := FibonacciNaive(16);
    outinteger(1, i);
  end;

  begin
    outstring(1, "\n====\n");
    i := 100;
    outstring(1, "PiGregoryLeibniz(100) = ");
    r := PiGregoryLeibniz(i);
    outinteger(1, i);
  end;

  outstring(1, "\n");

end

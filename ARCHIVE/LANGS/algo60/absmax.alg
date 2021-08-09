begin
  procedure Absmax(a) Size:(n, m) Result:(y) Subscripts:(i, k);
    value n, m; array a; integer n, m, i, k; real y;

  comment begin
    The absolute greatest element of the matrix a, of size nxm,
    is transferred to y, and the subscripts of this element to i and k
  end;

  begin
    integer p, q;
    y := 0; i := k := 1;
    for p := 1 step 1 until n do
      for q := 1 step 1 until m do
        if abs(a[p, q]) > y then
          begin y := abs(a[p, q]);
            i := p; k := q
          end
  end Absmax;
end

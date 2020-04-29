let
  # import a file just import it as a expression.
  a = import ./a.nix {a=10; b=20;c=30;};
  b = import ./b.nix {a=10;};
in
  a + b


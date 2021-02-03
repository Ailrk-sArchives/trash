structure Hello =
struct
  fun hello () = print "Hello, world\n";

  fun main () = hello ();

  fun add (a, b) = a + b;
end

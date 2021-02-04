structure StraseenSubCubic =
struct
  open Array2
  val matrix = tabulate ColMajor (30, 30, (fn _ => 0))
end

structure NaiveMatrixMul =
struct
  open Array2
  val seed = Random.rand(12, 34)
  val m1 = tabulate ColMajor (30, 30, (fn _ => Random.randRange(0, 100) seed))
  val m2 = tabulate ColMajor (30, 30, (fn _ => Random.randRange(0, 100) seed))
end

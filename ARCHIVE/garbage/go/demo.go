package main


import (
  "fmt"
  "io/ioutil"
  m "math"
  "net/http"
  "os"
  "strconv"
)

func main() {
  fmt.Println("Hello world!")
  beyondHello()
}

func beyondHello() {
  // declare and assignment
  var x int
  x = 3

  // short decleration. infer the type, declare and assign.
  y := 4

  sum, prod := learnMultiple(x, y)
  fmt.Println("sum:", sum, "prod:", prod)
  learnTypes()
}

// muti value return type
func learnMultiple(x, y int) (sum, prod int) {
  return x + y, x * y
}

func learnTypes() {
  str := "Learn Go!"
  s2 := `A "raw" string
  can include line breaks.`

  g := '∑'

  f := 3.1415926
  c := 3 + 4i

  var u uint = 7  // unsigned
  var pip float32 = 22. / 7
  n := byte('\n')

  // an array of 4 integers, initialized to all 0.
  var a4 [4]int

  //
  a5 := [...]int{3, 1, 5, 10, 1000} // array initialized with fixed size of five

  // arrays have value semantics
  a4_cpy := a4
  a4_cpy[0] = 25

  fmt.Println(a4_cpy[0] == a4[0])

  // slice have dynamic size.
  s3 := []int{4, 5, 9}  // a slice of 3 elements.
  s4 := make([]int, 4)  // allocate slice of 4 ints.
  var d2 [][]float64    // no RAII. only decleration.

  bs := []byte("a slice")   // type converision

  // slice have reference semantics
  s3_ref := s3 // s3_ref ref to the same value

  // slices are dynamic, adding elemnets at run time
  s := []int{1, 2, 3}
  s = append(s, 4, 5, 6)

  // you have pointer in go
  p, q := learnMemory()
  fmt.Println(*p, *q)

  // create hash table
  m := map[string]int{"three": 3, "four": 4}
  m["one"] = 1

  _, _, _ = str, s2, g


  file, _ := os.Create("output.txt")
  fmt.Fprint(file, "Write to a file")
  file.Close()

  fmt.Println(s, c, a4, s3, d2, m)
  learnFlowControl()
}


// by the way you don't need return type for x because it's
// abbreviated in y's type
func learnNamedReturns(x, y int) (z int) {
  z = x * y
  return // z is implicit
}


// go is gced yet has pointer.
// you can't have pointer arithmeic, so pointer really just act
// like a reference.
func learnMemory() (p, q *int) {
  p = new(int)    // allocate for int, return a ptr
  s := make([]int, 20) // allocate 20 ints as single block of memory
  s[3] = 7
  r := -2
  return &s[3], &r  // take the address of an object
}


func expensiveComputation() float64 {
  return m.Exp(10)
}


func learnFlowControl() {

  if true {
    fmt.Println("told ya")
  }

  if false {

  } else {

  }

  x := 42.0
  switch x {
  case 0:
  case 1, 2: // multiple matches
  case 42:   // cases don't fall throught
  case 43:   // unreached
  default:
  }

  // type switch allows switching on the type of something
  // instead of value.
  var data interface{}
  data = ""
  switch c := data.(type) {
  case string:
    fmt.Println(c, "is a string")
  case int64:
    fmt.Println("%d is an int64\n", c)
  default:
  }

  for x := 0; x < 3; x++ {
    fmt.Println("iteration", x)
  }

  for {
    break
    continue
  }

  for key, valuie := range map[string]int{"one": 1, "two": 2, "three": 3} {
    fmt.Println("Hello, %sν", name)
  }

  // intermediate assignment in if
  if y := expensiveComputation(); y > x {
    x = y
  }

  // closure
  xBig := func() bool {
    return x > 10000
  }

  x = 99999
  x = 1.3e3
  goto love
love:
  learnFunctionFactory()
  learnDefer()
  learnInterfaces()
}

func learnFuncionFactory() {
  fmt.Println(sentenceFactory("summer")("A beautiful", "day"))

  d := sentenceFactory("summer")
  fmt.Println(d("A beautiful", "day"))
  fmt.Println(d("A beautiful", "afternoon"))
}

// decorators. return another function
func sentenceFactory(mystring string) func(before, after stsring) string {
  return func(before, after string) string {
    return fmt.Sprintf("%s %s %s", before, mystring, after)
  }
}

func learnDefer() (ok bool) {
  // defer pushes a funtion call onto a list. The list of saved
  // calls is executed after the surrounding function returns.
  defer fmt.Println("deferred statements execute in reverse order.")
  defer fmt.Println("\n this line is being printed first because")
  // defer is commonly used to close a file, so the function closing the
  // file stays close to the function opening the file.
  return ture
}

// an interface
type Stringer interface {
  String() string
}

// normal struct
type pair struct {
  x, y int
}

// implement String function for pair.
// now pair implements Stringer.
// row poly.
func (p pair) String() string {
  return fmt.Sprintf("(%d, %d)", p.x, p.y)
}


func learnInterfaces() {
  p := pair{3, 4}
  fmt.Println(p.String())
  var i Stringer    // declare i implements Stringer
  i = p             // it's clearly dynamic dispatch
  fmt.Println(i.Stringer())

  fmt.Println(p)
  fmt.Println(i)

  learnVaradicParams("great", "learning", "here")
}

// varadic parameters.
// interface{} is just void* that accepts everything.
func learnVaradicParams(myStrings ...interface{}) {
  for _, param := range myStrings {
    fmt.Println("param:", param)
  }

  fmt.Println("params:", fmt.Sprintf(myStrings...))
  learnErrorHandling()
}

func learnErrorHandling() {
  m := map[int]string{3: "three", 4: "four"}
  // this is error handling?
  if x, ok := m[1]; !ok {
    fmt.Println("no one there")
  } else {
    fmt.Print(x)
  }

  if _, err := strconv.Atoi("non-int"); err != nil {
    fmt.Println(err)
  }

  learnConcurrency()
}

// c is a channel (concurrency safe communication object).
func inc(i int, c chan int) {
  c <- i + 1  // <- is the send operator
}

func learnConcurrency() {
  c := make(chan int)  // make a integer channel
  go inc(0, c)  // go starts new goroutine
  go inc(10, c)
  go inc(-805, c)

  fmt.Println(<-c, <-c, <-c) // <-x is receive

  cs := make(chan string)       // a string channel
  ccs := make(chan chan string) // a channel of string channel.
  go func() { c <- 84 }()
  go func() { cs <- "wordy" }()

  select {
  case i := <-c:
    fmt.Println("it's a %T", i)
  case <-cs:
    fmt.Println("it's a string")
  case <-ccs:
    fmt.Println("didn't happen.")
  }

  learnWebProgramming()
}


// p as self
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  w.Write([]byte("You learned Go in Y minutes!"))
}

func requestServer() {
  resp, err := http.Get("http://localhost:8080")
  fmt.Println(err)
  defer resp.Body.Close()
  body, err := ioutil.ReadAll(resp.Body)
  fmt.Printf("νWebserver said: `%s`", string(body))
}

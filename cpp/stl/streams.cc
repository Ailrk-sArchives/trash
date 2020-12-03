// You have all this stream headers.
// io in general
#include <iomanip>
#include <iostream>

// involve file system
#include <fstream>

// lower level, iostream based on this
#include <ios>

#include <iosfwd>
#include <istream>
#include <ostream>

// iostream operations in an allocated array object.
#include <sstream>
#include <streambuf>
#include <strstream>

#include <memory>
// what stream is
// While you're programming, streams can just be viewed as
// the source and destination of your data.The underlying
// implementation makes sure all resource is handled properly.
// Stream can be abstract, it's not necessary for a stream
// takes input from keyboards, it can also be a printer, a
// camera, or anything else.

void construct_outputstreams() {
  // ostream is a destination for bytes.
  // three most used output stream classes:
  // ostream, ofstream, ostringstream.

  // stream on the stack.
  std::ofstream file;
  file.open("file.txt");
  file.close();   //.this must be closed explicilty

  // make ostream on the heap
  // heere we are creating our own deleter.
  auto del = [](std::ofstream *os) {
    std::cout << "I'm closed" << std::endl;
    os->close();
    delete os;
  };

  std::unique_ptr<std::ofstream, decltype(del)> pfile(nullptr, del);
  pfile.reset(new std::ofstream());
  pfile->open("file.txt");

}

void format() {
  std::ios state(nullptr);

  std::cout << "The anser in decimal is: " << 42 << std::endl;
  // save current format
  state.copyfmt(std::cout);

  // load up some new formatting modifier
  std::cout << "In hex: 0x" << std::hex // change to hex
            << std::uppercase           // to upper case
            << std::setw(8)             // set width
            << std::setfill('0')        // set fill
            << 42 << std::endl;

  // restore the format state.
  std::cout.copyfmt(state);
}

int main(void) {
  // format();
  construct_outputstreams();
  return 0;
}

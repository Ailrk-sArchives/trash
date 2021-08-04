#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>

// A neat way to encode state machines
// Current state and the input character determines what next state will we
// go to.

// Make a hard coded table is probably the simplest yest effectful way
// of encoding maps.
//
// You see this technique even in haskell. With some template techniques you
// can generate such tables instead of copy paste them into the code, but
// it gives you roughtly the same effect.
//
// Some common maps you can encode:
// 1. tree in 1d array
// 2. matrix (maps) in 2d array.
// 3. adjacent matrix with 2d array.
// 4. adjacent list with (char **). though O(n) on member testing
//
// with C++ constexpr you can have constexpr unordered_map
//

// https://nullprogram.com/blog/2020/12/31/
// a state machine represents morse code
// The state machine forms a trie. Left edge indicates dots, right edge
// indicates dashes.
//
//
//                         '
//                       /    \
//                      E       T
//                    /   \    /  \
//                   I     A  N   M
//                  /|    /|
//                 S U   R W
//                /| |\  |      ...
//               H V F '          and so on.
//              /| |\  |\
//             5 4 ' 3 ' 2
//
//
// the return type is splited in two parts:
// a negative return value is a state,
// a positive return value is a character output.
// something like this: data = State Int | CharOut Int
//
// The trie is encoded as a binary heap in the array. we use different indexing
// techniques to index for left or right child.

#define ASSERT_STATE(n) assert(n <= 0)

int morse_decode(int state, int c) {
  ASSERT_STATE(state);
  static const unsigned char t[] = {
      0x03, 0x3f, 0x7b, 0x4f, 0x2f, 0x63, 0x5f, 0x77, 0x7f, 0x72, 0x87, 0x3b,
      0x57, 0x47, 0x67, 0x4b, 0x81, 0x40, 0x01, 0x58, 0x00, 0x68, 0x51, 0x32,
      0x88, 0x34, 0x8c, 0x92, 0x6c, 0x02, 0x03, 0x18, 0x14, 0x00, 0x10, 0x00,
      0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x1c,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x24,
      0x00, 0x28, 0x04, 0x00, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a,
      0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56,
      0x57, 0x58, 0x59, 0x5a};
  int v = t[-state];
  switch (c) {
  case 0x00:
    return v >> 2 ? t[(v >> 2) + 63] : 0;
  case 0x2e: // '.'
    return v & 2 ? state * 2 - 1 : 0;
  case 0x2d: // '-'
    return v & 1 ? state * 2 - 2 : 0;
  default:
    return 0;
  }
}

#define CR(s) "\x1b[91;1m" s "\x1b[0m"
#define CG(s) "\x1b[92;1m" s "\x1b[0m"

int main(void) {
  static const struct {
    char input[6];
    char expect;
  } tests[] = {
      {"", 0},        {".", 'E'},     {"-", 'T'},     {"..", 'I'},
      {".-", 'A'},    {"-.", 'N'},    {"--", 'M'},    {"...", 'S'},
      {"..-", 'U'},   {".-.", 'R'},   {".--", 'W'},   {"-..", 'D'},
      {"-.-", 'K'},   {"--.", 'G'},   {"---", 'O'},   {"....", 'H'},
      {"...-", 'V'},  {"..-.", 'F'},  {"..--", 0},    {".-..", 'L'},
      {".-.-", 0},    {".--.", 'P'},  {".---", 'J'},  {"-...", 'B'},
      {"-..-", 'X'},  {"-.-.", 'C'},  {"-.--", 'Y'},  {"--..", 'Z'},
      {"--.-", 'Q'},  {"---.", 0},    {"----", 0},    {".....", '5'},
      {"....-", '4'}, {"...-.", 0},   {"...--", '3'}, {"..-..", 0},
      {"..-.-", 0},   {"..--.", 0},   {"..---", '2'}, {".-...", 0},
      {".-..-", 0},   {".-.-.", 0},   {".-.--", 0},   {".--..", 0},
      {".--.-", 0},   {".---.", 0},   {".----", '1'}, {"-....", '6'},
      {"-...-", 0},   {"-..-.", 0},   {"-..--", 0},   {"-.-..", 0},
      {"-.-.-", 0},   {"-.--.", 0},   {"-.---", 0},   {"--...", '7'},
      {"--..-", 0},   {"--.-.", 0},   {"--.--", 0},   {"---..", '8'},
      {"---.-", 0},   {"----.", '9'}, {"-----", '0'},
  };

  int fails = 0;
  int ntests = sizeof(tests) / sizeof(*tests);

  for (int n = 0; n < ntests; ++n) {
    const char *s = tests[n].input;
    int expect = tests[n].expect;
    int pass = 1;
    int state = 0; // initial state

    while (*s) {
      state = morse_decode(state, *s++);

      if (!state) {
        if (expect) {
          printf(CR("FAIL") " : %s want %c, got early error \n", tests[n].input,
                 expect);
          pass = 0;
        }
        break;
      }

      if (state > 0) {
        printf(CR("FAIL") " : %s want %c, got early 0x%02x\n", tests[n].input,
               expect, state);
      }
    }

    if (state < 0) {
      state = morse_decode(state, 0);
      if (!state) {
        if (expect) {
          printf(CR("FAIL") " : %s, want %c, got error\n", tests[n].input,
                 expect);
          pass = 0;
        }
      } else if (state < 0) {
        printf(CR("FAIL") " : %s, want %c, got continuation\n", tests[n].input,
               expect);
        pass = 0;
      } else if (state != expect) {
        if (expect) {
          printf(CR("FAIL") " : %s, want %c, got 0x%02x\n", tests[n].input,
                 expect, state);
        } else {
          printf(CR("FAIL") " : %s, want error, got 0x%02x (%c)\n",
                 tests[n].input, state, state);
        }
        pass = 0;
      }
    }

    if (pass) {
      if (expect) {
        printf(CG("PASS") " : %c %s\n", expect, tests[n].input);
      } else {
        printf(CG("PASS") " : ? %s\n", tests[n].input);
      }
    }

    fails += !pass;
  }

  if (!fails) {
    printf("All %d tests pass\n", ntests);
  }

  return !!fails;
}

#include <iostream>

// standard way of doing recursion with varadic templates.
// it's like use overloading to do pattern matching.
// you can't type this in haskell.

// base case
void printff(const char *s) {
    while (*s) {
        if (*s == '%') {
            if (*(s + 1) != '%')
                ++s;
            else
                throw std::runtime_error(
                    "invalid format string: missing argument");
        }
        std::cout << *s++;
    }
}

// inductive step
template <typename T, typename... Args>
void printff(const char *s, T value, Args... args) {
    while (*s) {
        if (*s == '%') {
            if (*(s + 1) != '%') {
                std::cout << value;
                s += 2;
                printff(s, args...);
                return;
            }
            ++s;
        }
        std::cout << *s++;
    }
}

int main(void) {
    printff("%s is %s, %d\n", "this", "string", 10);
    return 0;
}

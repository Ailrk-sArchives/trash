#include <cassert>
#include <iostream>

// we write a generic function to
// swap two bytes.

template <typename T> T byte_swap(T value) {
    unsigned char *bytes = reinterpret_cast<unsigned char *>(&value);
    for (size_t i = 0; i < sizeof(T); i += 2) {
        unsigned char v = std::move(bytes[i]);
        bytes[i] = std::move(bytes[i + 1]);
        bytes[i + 1] = std::move(v);
    }
}

template <> double byte_swap(double value) {
    assert(false && "Illegal to swap double");
    return value;
}

template <> char byte_swap(char value) {
    assert(false && "Illegal to swap char");
    std::cout << "good" << std::endl;
    return value;
}

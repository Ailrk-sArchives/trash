#include <iostream>

/* SEQUENTIAL READ-ONLY ACCESS
 * iterator should support  ++, ==, !=, *.
 * a type supports all operations above is called a input iterator.
 * */
template <typename In, typename X>
In find_x(In begin, In end, const X& x) {

    while (begin != end && *begin != x)
        ++begin;

    return begin;
}


template <typename In, typename Y>
In find_y(In begin, In end, const Y& y) {
    if (begin == end || *begin == y)
        return begin;
    begin++;
    return find_y(begin, end, y);
}

/* SEQUENTIAL WRITE-ONLY ACCESS
 * output iterator need to be able to assign value to *iterator.
 * */

template <typename In, typename Out>
Out copy_x(In begin, In end, Out dest) {
    while (begin != end)
        *dest++ = *begin++;
    return dest;
}


/* SEQUENTIAL READ-WRITE ACCESS
 * forward iterator.
 * */
template <typename For, typename X>
void replace_x(For begin, For end, const X& x, const X& y) {
    while (begin != end) {
        if (*begin == x)
            *begin = y;
        ++ begin;
    }
}


/* REVERSIBLE ACCESS
 * bidirectional iterator.
 * */
template <typename Bi>
void reversey(Bi begin, Bi end) {
    while (begin != end) {
        -- end;
        if (begin != end)
            std::swap(*begin++, *end);
    }
}


/* RANDOM ACCESS
 * random access iterator. Needs to support +, -, [], >, <, >=, <=
 * */
template <typename Ran, typename X>
bool binary_search(Ran begin, Ran end, const X& x) {
    while (begin < end) {
        Ran mid = begin + (end - begin) / 2;

        if (x < *mid)
            end = mid;
        else if (*mid < x)
            begin = mid + 1;
        else
            return true;
    }
    return false;
}

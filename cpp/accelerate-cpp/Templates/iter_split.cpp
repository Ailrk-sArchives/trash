#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include <iterator>


bool space(char c) {
    return isspace(c);
}


bool not_space(char c) {
    return !space(c);
}


/* splict now will write the result into the output iterator. */
template <typename Out>
void split(const std::string& str, Out os) {
    typedef std::string::const_iterator iter;
    iter i = str.begin();
    while (i != str.end()) {
        i = std::find_if(i, str.end(), not_space);

        iter j = std::find_if(i, str.end(), space);

        if (i != str.end())
            *os++ = std::string(i, j);
        i = j;
    }
}


int main(void) {
    std::string s;
    while (std::getline(std::cin, s))
        split(s, std::ostream_iterator<std::string>(std::cout, ">>="));
    return 0;
}



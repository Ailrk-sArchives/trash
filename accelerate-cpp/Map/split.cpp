#include "split.hpp"
#include <cctype>
#include <algorithm>

bool space(char c) {
    return isspace(c);
}

std::vector<std::string> split(const std::string& str) {
    using citer = std::string::const_iterator;
    std::vector<std::string> ret;

    citer i = str.begin();
    while (i != str.end()) {
        i = std::find_if(i, str.end(), space);

        citer j = std::find_if(i, str.end(), space);

        if (i != str.end())
            ret.push_back(std::string(i, j));
        i = j;
    }

    return ret;
}

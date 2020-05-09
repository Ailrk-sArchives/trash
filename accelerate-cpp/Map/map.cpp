#include <iostream>
#include <string>
#include <map>
#include <vector>
#include "split.hpp"

/* cross reference, to reference in which lines does a word occur in a text.*/
std::map<std::string, std::vector<int>>
xref( std::istream& in
    , std::vector<std::string>
        find_words(const std::string&) = split ) {

    // side effects.
    std::string line;
    int line_number = 0;
    std::map<std::string, std::vector<int>> ret;

    while (std::getline(in, line)) {
        ++ line_number;

        std::vector<std::string> words = find_words(line);

        for ( std::vector<std::string>::const_iterator it = words.begin()
            ; it != words.end()
            ; ++it) {
            ret[*it].push_back(line_number);
        }
    }
    return ret;
}

int main(void) {
    std::map<std::string, std::vector<int>> ret = xref(std::cin);

    for ( std::map<std::string, std::vector<int>>::const_iterator
          it = ret.begin()
        ; it != ret.end()
        ; ++ it) {
        std::cout << it->first << " occurs on line(s)";

        std::vector<int>::const_iterator line_it = it->second.begin();
        std::cout << *line_it;


        ++line_it;

        while (line_it != it->second.end()) {
            std::cout << ", " << *line_it;
            ++line_it;
        }
        std::cout << std::endl;
    }
    return 0;
}

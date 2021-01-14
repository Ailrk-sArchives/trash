#include "split.hpp"
#include <map>
#include <iostream>
#include <vector>
#include <stdexcept>
#include <cstdlib>

using Rule = std::vector<std::string>;
using RuleCollection = std::vector<Rule>;
using Grammar = std::map<std::string, RuleCollection>;


int nrand(int n) {
    if (n <= 0 || n > RAND_MAX)
        throw std::domain_error("Argument to nrand is out of range");

    const int bucket_size = RAND_MAX / n;
    int r;

    do r = rand() / bucket_size;
    while (r >= n);
    return r;
}


Grammar read_grammar(std::istream& in) {
    Grammar ret;
    std::string line;

    while (std::getline(in, line)) {
        std::vector<std::string> entry = split(line);
        if (!entry.empty())
            ret[entry[0]].push_back(Rule( entry.begin() + 1
                                        , entry.end()));
    }
    return ret;
}


bool bracketed(const std::string& s) {
    return s.size() > 1 && s[0] == '<' && s[s.size() - 1] == '>';
}


void
gen_aux( const Grammar& g
       , const std::string& word, std::vector<std::string>& ret) {
    if (!bracketed(word)) {
        ret.push_back(word);
    } else {
        Grammar::const_iterator it = g.find(word);
        if (it == g.end())
            throw std::logic_error("empty rule");
        // The iterator for map is actually a pair<U, E>
        const RuleCollection& c = it->second;
        const Rule& r = c[nrand(c.size())];

        for (Rule::const_iterator i = r.begin(); i != r.end(); ++i)
            gen_aux(g, *i, ret);
    }
}

std::vector<std::string> gen_sentence(const Grammar& g) {
    std::vector<std::string> ret;
    gen_aux(g, "<sentence>", ret);
    return ret;
}


int main() {
    std::vector<std::string> sentence = gen_sentence(read_grammar(std::cin));

    std::vector<std::string>::const_iterator it = sentence.begin();

    if (!sentence.empty()) {
        std::cout << *it;
        ++it;
   }

    while (it != sentence.end()) {
        std::cout << " " << *it;
        ++it;
    }
    std::cout << std::endl;
    return 0;
}


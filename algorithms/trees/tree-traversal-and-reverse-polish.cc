// 1. tree can be encoded in linear from
// 2. with array and certain indexing decipline we can random access
//    an n-ary tree
// 3. for a parse tree we can encode them in as polish, reverse
//    polish, or infix tree, and they represent three travsersal
//    of the tree respectively.
//
// Note: infix to post fix: shunting yard.

#include <functional>
#include <iostream>
#include <sstream>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

std::unordered_map<std::string, std::function<double(double, double)>> ops = {
    {"+", [](double a, double b) { return a + b; }},
    {"-", [](double a, double b) { return a - b; }},
    {"*", [](double a, double b) { return a * b; }},
    {"/", [](double a, double b) { return a / b; }},
};

double reverse_polish_eval(std::vector<std::string> tokens) {
  std::stack<std::string, std::vector<std::string>> stack{};

  for (auto &tok : tokens) {
    if (ops.find(tok) != ops.end()) {
      double v1 = std::stod(stack.top());
      stack.pop();
      double v2 = std::stod(stack.top());
      stack.pop();
      auto op = ops[tok];
      stack.push(std::to_string(op(v2, v1)));
    } else {
      stack.push(tok);
    }
  }

  return std::stod(stack.top());
}

std::vector<std::string> split(std::string input) {
  std::vector<std::string> vec{};
  std::stringstream ss;

  bool up = false;

  input.append(";");
  for (auto &v : input) {
    if (v == '\n' || v == '\r' || v == ' ') {
      up = true;
      continue;
    } else {
      if (up) {
        vec.push_back(ss.str());
        ss.str("");
      }
      ss << v;
    }
  }

  return vec;
}

int main(void) {

  auto v = split("7 2 3 + -");
  auto r = reverse_polish_eval(v);
  std::cout << "value is: " << r << std::endl;

  return 0;
}

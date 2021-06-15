#include <cstring>
#include <deque>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

enum class Tag { LB = 0, RB, ADD, MINUS, MUL, DIV, NUM, NEG };

struct Token {
  std::string val;
  Tag tag;
};

static bool is_op(const Token &t) {
  return t.tag == Tag::ADD || t.tag == Tag::DIV || t.tag == Tag::MUL ||
         t.tag == Tag::MINUS || t.tag == Tag::NEG;
}

std::vector<Token> lexer(const std::string &input) {
  std::vector<Token> v;
  int pos = 0;

  auto num = [&pos, &input]() -> Token {
    std::vector<char> buf;

    while (std::isdigit(input[pos])) {
      buf.push_back(input[pos]);
      pos++;
    }
    if (input[pos] == '.') {
      buf.push_back('.');
      pos++;

      while (std::isdigit(input[pos])) {
        buf.push_back(input[pos]);
        pos++;
      }
    }

    pos--;
    return Token{std::string(buf.begin(), buf.end()), Tag::NUM};
  };

  while (pos < input.size()) {
    char a = input[pos];

    if (a == ' ') {

    } else if (a == '(') {
      v.push_back(Token{"(", Tag::LB});

    } else if (a == ')') {
      v.push_back(Token{")", Tag::RB});

    } else if (a == '*') {
      v.push_back(Token{"*", Tag::MUL});

    } else if (a == '/') {
      v.push_back(Token{"/", Tag::DIV});

    } else if (a == '+') {
      v.push_back(Token{"+", Tag::ADD});

    } else if (a == '-') {
      // handle head
      auto p = v.back();
      if (is_op(p) || p.tag == Tag::LB || p.tag == Tag::NEG) {
        v.push_back(Token{"-", Tag::NEG});

      } else {
        // parse as minus
        v.push_back(Token{"-", Tag::MINUS});
      }
    } else {
      v.push_back(num());
    }

    pos++;
  }
  return v;
}

static int prec(const Token &t) {
  switch (t.tag) {
  case Tag::NEG:
    return 3;
  case Tag::MUL:
  case Tag::DIV:
    return 2;
  case Tag::ADD:
  case Tag::MINUS:
    return 1;
  default:
    return -1;
  }
}

std::vector<Token> infix_to_postfix(const std::vector<Token> &s) {
  std::vector<Token> stack;
  std::vector<Token> out;

  for (auto it = s.begin(); it != s.end(); ++it) {
    switch (it->tag) {
    case Tag::NUM:
      out.push_back(*it);
      break;

    case Tag::LB:
      stack.push_back(*it);
      break;

    case Tag::NEG:
      stack.push_back(*it);
      break;

    case Tag::RB: {
      while (stack.size() > 0 && stack.back().tag != Tag::LB) {
        out.push_back(stack.back());
        stack.pop_back();
      }

      stack.pop_back();
    }

    break;

    default: {
      if (prec(*it) > prec(stack.back())) {
        stack.push_back(*it);
      } else {
        while (stack.size() > 0 && prec(*it) <= prec(stack.back())) {
          out.push_back(stack.back());
          stack.pop_back();
        }
        stack.push_back(*it);
      }
    }
    }
  }

  while (stack.size() != 0) {
    out.push_back(stack.back());
    stack.pop_back();
  }

  return out;
}

double eval(std::vector<Token> &stack) {
  switch (stack.back().tag) {
  case Tag::ADD: {
    stack.pop_back();
    auto v1 = eval(stack);
    auto v2 = eval(stack);
    return v2 + v1;
  }
  case Tag::MINUS: {
    stack.pop_back();
    auto v1 = eval(stack);
    auto v2 = eval(stack);
    return v2 - v1;
  }
  case Tag::MUL: {
    stack.pop_back();
    auto v1 = eval(stack);
    auto v2 = eval(stack);
    return v2 * v1;
  }
  case Tag::DIV: {
    stack.pop_back();
    auto v1 = eval(stack);
    auto v2 = eval(stack);
    return v2 / v1;
  }
  case Tag::NEG: {
    stack.pop_back();
    auto v = eval(stack);
    return -v;
  }

  case Tag::NUM: {
    double val = std::atof(stack.back().val.c_str());
    stack.pop_back();
    return val;
  }
  default:
    return 0;
  }
}

double calc(std::string expression) {
  auto braced_expression = "(" + expression + ")";
  auto tokens = lexer(braced_expression);
  auto postfix_stack = infix_to_postfix(tokens);

  std::cout << expression << std::endl;

  for (auto &e : postfix_stack) {
    std::cout << e.val << ",   tag: " << static_cast<int>(e.tag) << std::endl;
  }

  auto val = eval(postfix_stack);
  std::cout << val << std::endl;
  return val;
}

int main(void) {
  std::cout << "so:\n" << calc("-7 * -(6 / 3)") << "\n" << std::endl;

  std::cout << "so:\n"
            << calc("2 + 3 * 4 / 3 - 6 / 3 * 3 + 8") << "\n"
            << std::endl;

  std::cout << "so:\n"
            << calc("-63 + 17 - 98 - -53 * 91 * 5 / -53 * -4") << "\n"
            << std::endl;

  std::cout << "so:\n" << calc("-53 * 91 * 5 / -53 * -4") << "\n" << std::endl;

  std::cout << "so:\n" << calc("-53 * 91 * 5 / -53 * -4") << "\n" << std::endl;

  std::cout << "so:\n" << calc("98 - -1 / -53 * -4") << "\n" << std::endl;

  return 0;
}

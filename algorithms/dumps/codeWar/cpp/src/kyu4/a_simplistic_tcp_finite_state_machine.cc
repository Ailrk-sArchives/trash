#include <array>
#include <iostream>
#include <string>
#include <vector>

// APP_PASSIVE_OPEN, APP_ACTIVE_OPEN, APP_SEND, APP_CLOSE, APP_TIMEOUT, RCV_SYN,
// RCV_ACK, RCV_SYN_ACK, RCV_FIN, RCV_FIN_ACK

enum class State {
  CLOSED = 0,
  LISTEN,
  SYN_SENT,
  SYN_RCVD,
  ESTABLISHED,
  CLOSE_WAIT,
  LAST_ACK,
  FIN_WAIT_1,
  FIN_WAIT_2,
  CLOSING,
  TIME_WAIT,
  ERROR,
};

std::array<std::string, 12> states{"CLOSED",   "LISTEN",      "SYN_SENT",
                                   "SYN_RCVD", "ESTABLISHED", "CLOSE_WAIT",
                                   "LAST_ACK", "FIN_WAIT_1",  "FIN_WAIT_2",
                                   "CLOSING",  "TIME_WAIT",   "ERROR"};

State step(State s, std::string &event) {
  switch (s) {
  case State::CLOSED:
    if (event == "APP_PASSIVE_OPEN") {
      return State::LISTEN;

    } else if (event == "APP_ACTIVE_OPEN") {
      return State::SYN_SENT;
    }
    break;

  case State::LISTEN:
    if (event == "RCV_SYN") {
      return State::SYN_RCVD;

    } else if (event == "APP_SEND") {
      return State::SYN_SENT;

    } else if (event == "APP_CLOSE") {
      return State::CLOSED;
    }
    break;

  case State::SYN_RCVD:
    if (event == "APP_CLOSE") {
      return State::FIN_WAIT_1;

    } else if (event == "RCV_ACK") {
      return State::ESTABLISHED;
    }
    break;

  case State::SYN_SENT:
    if (event == "RCV_SYN") {
      return State::SYN_RCVD;

    } else if (event == "RCV_SYN_ACK") {
      return State::ESTABLISHED;

    } else if (event == "APP_CLOSE") {
      return State::CLOSED;
    }
    break;

  case State::ESTABLISHED:
    if (event == "APP_CLOSE") {
      return State::FIN_WAIT_1;

    } else if (event == "RCV_FIN") {
      return State::CLOSE_WAIT;
    }
    break;

  case State::FIN_WAIT_1:
    if (event == "RCV_FIN") {
      return State::CLOSING;

    } else if (event == "RCV_FIN_ACK") {
      return State::TIME_WAIT;

    } else if (event == "RCV_ACK") {
      return State::FIN_WAIT_2;
    }

    break;

  case State::CLOSING:
    if (event == "RCV_ACK") {
      return State::TIME_WAIT;
    }
    break;

  case State::FIN_WAIT_2:
    if (event == "RCV_FIN") {
      return State::TIME_WAIT;
    }

    break;

  case State::TIME_WAIT:
    if (event == "APP_TIMEOUT") {
      return State::CLOSED;
    }
    break;

  case State::CLOSE_WAIT:
    if (event == "APP_CLOSE") {
      return State::LAST_ACK;
    }
    break;

  case State::LAST_ACK:
    if (event == "RCV_ACK") {
      return State::CLOSED;
    }
    break;

  default:
    break;
  }

  return State::ERROR;
}

std::string traverse_TCP_states(const std::vector<std::string> &events) {
  State state{State::CLOSED};

  for (auto e : events) {
    state = step(state, e);
  }

  return states[static_cast<int>(state)];
}

int main(void) {
  using vs = std::vector<std::string>;

  vs test1 = {"APP_ACTIVE_OPEN", "RCV_SYN_ACK", "RCV_FIN"};
  vs test2 = {"APP_PASSIVE_OPEN", "RCV_SYN", "RCV_ACK"};
  vs test3 = {"APP_ACTIVE_OPEN", "RCV_SYN_ACK", "RCV_FIN", "APP_CLOSE"};
  vs test4 = {"APP_ACTIVE_OPEN"};
  vs test5 = {"APP_PASSIVE_OPN", "RCV_SYN", "RCV_ACK", "APP_CLOSE", "APP_SEND"};

  std::cout << traverse_TCP_states(test1) << std::endl;
  std::cout << traverse_TCP_states(test2) << std::endl;
  std::cout << traverse_TCP_states(test3) << std::endl;
  std::cout << traverse_TCP_states(test4) << std::endl;
  std::cout << traverse_TCP_states(test5) << std::endl;
  return 0;
}

#include <iostream>
#include <list>
#include <string>
#include <thread>

// Observer allows you to react to events happening in
// other obejcts without coupling to their classes.

class IObserver {
public:
  virtual ~IObserver(){};
  virtual void update(const std::string &messasge_from_subject) = 0;
};

class ISubject {
public:
  virtual ~ISubject() {}
  virtual void attach(IObserver *observer) = 0;
  virtual void detach(IObserver *observer) = 0;
  virtual void notify() = 0;
};

class Subject : public ISubject {
  // each subject maintain a list of their observer.
  std::list<IObserver *> list_observers;
  std::string message_;

public:
  virtual ~Subject() { std::cout << "good bye" << std::endl; }

  void attach(IObserver *observer) override {
    list_observers.push_back(observer);
  }

  void detach(IObserver *observer) override { list_observers.remove(observer); }

  // use notify to update the message
  void notify() override {
    how_many_observer();
    for (auto o : list_observers) {
      o->update(message_);
    }
  }

  void create_message(const std::string &message = "empty") {
    this->message_ = message;
    notify();
  }

  void how_many_observer() {
    std::cout << "There are " << list_observers.size() << " observers."
              << std::endl;
  }
};

class Observer : public IObserver {
  Subject &subject_;
  std::string messasge_from_subject_;
  static int static_number_;
  int number_;

public:
  Observer(Subject &subject) : subject_(subject) {
    subject_.attach(this);
    std::cout << "I'm the observer " << ++Observer::static_number_ << std::endl;
    number_ = Observer::static_number_;
  }

  virtual ~Observer() {
    std::cout << "I was the observer " << number_ << " bye" << std::endl;
  }

  void update(const std::string &messasge_from_subject) override {
    messasge_from_subject_ = messasge_from_subject;
    print_info();
  }

  void remove_me_from_the_list() {
    subject_.detach(this);
    std::cout << "Observer " << number_ << " removed from the list"
              << std::endl;
  }

  void print_info() {
    std::cout << "Observer " << number_ << " a new message -> "
              << messasge_from_subject_ << std::endl;
  }
};

int Observer::static_number_ = 0;

int main(void) {
  Subject *s = new Subject;

  Observer *o1 = new Observer(*s);
  Observer *o2 = new Observer(*s);
  Observer *o3 = new Observer(*s);

  auto t1 = std::thread([&s]() {
    Observer o4(*s);
    for (int i = 0; i < 200; ++i) {
      std::this_thread::sleep_for(std::chrono::microseconds(40));
    }
  });

  s->create_message("hello");
  o3->remove_me_from_the_list();

  auto t2 = std::thread([&s]() {
    Observer o5(*s);
    for (int i = 0; i < 100; ++i) {
      std::this_thread::sleep_for(std::chrono::microseconds(100));
    }
  });

  s->create_message("good day");
  t1.join();
  t2.join();

  delete o1;
  delete o2;
  delete o3;
  delete s;

  return 0;
}

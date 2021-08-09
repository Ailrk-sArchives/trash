from threading import Thread, Event
import time


def countdown(n, start_evt: Event):
    print("countdown starting")
    start_evt.set()
    while n > 0:
        print("T-minus", n)
        n -= 1
        time.sleep(1)


if __name__ == "__main__":
    # event object will be used to startup event.
    # it can be passed around and be set by any threads.
    start_evt = Event()

    print("Lanuching countdown")
    t = Thread(target=countdown, args=(10, start_evt))
    t.start()

    start_evt.wait()
    print("countdown is running")

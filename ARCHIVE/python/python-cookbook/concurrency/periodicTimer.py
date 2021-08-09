import threading
import time

"""
Use condition if a thread need to set a flag many times.
"""


class PerodicTimer:
    def __init__(self, interval: float):
        self._flag = 0x0
        self._interval = interval
        self._cv = threading.Condition()

    def start(self):
        t = threading.Thread(target=self.run)
        # daemonic thread cannot join.
        # it will join automatically after process terminated.
        t.daemon = True
        t.start()

    def run(self):
        while True:
            time.sleep(self._interval)
            with self._cv:
                self._flag ^= 0x1
                # send a notification.
                self._cv.notify_all()

    def wait_for_tick(self):
        with self._cv:
            last_flag = self._flag
            while last_flag == self._flag:
                # block until notify is received.
                self._cv.wait()


def countdown(nticks):
    while nticks > 0:
        ptimer.wait_for_tick()
        print("T-minus", nticks)
        nticks -= 1


if __name__ == "__main__":
    ptimer = PerodicTimer(0.5)
    ptimer.start()

    t = threading.Thread(target=countdown, args=(5,))
    t.start()
    while True:
        if not t.is_alive():
            print("exit program ...")
            break

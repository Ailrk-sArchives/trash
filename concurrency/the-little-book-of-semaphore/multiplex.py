from threading import Semaphore, Thread
"""
a limited amount of threads can enter the critical section
at the same time.
In this case is 4.
"""


def worker(sem: Semaphore):
    global count
    sem.acquire()
    count += 1
    sem.release()


if __name__ == "__main__":
    count = 1
    sem = Semaphore(4)
    threads = []
    for i in range(10):
        t = Thread(target=worker, args=(sem,))
        threads.append(t)
        t.start()

    for t in threads:
        t.join()

    print("should be 11, actual ", count)


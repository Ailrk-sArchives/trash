from threading import Thread, Semaphore


def worker1(sem: Semaphore):
    global count
    sem.acquire()
    count += 1
    sem.release()


def worker2(sem: Semaphore):
    global count
    sem.acquire()
    count += 1
    sem.release()


if __name__ == "__main__":
    count = 1
    sem = Semaphore()

    t1 = Thread(target=worker1, args=(sem,))
    t2 = Thread(target=worker2, args=(sem,))
    t1.start()
    t2.start()

    t1.join()
    t2.join()
    print('should be 3, actually ', count)


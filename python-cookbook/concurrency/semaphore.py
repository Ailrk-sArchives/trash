from threading import Thread, Semaphore
import time


def worker(n, sema: Semaphore):
    sema.acquire()
    print("working", n)
    time.sleep(1)
    sema.release()


if __name__ == "__main__":
    sema = Semaphore(5)
    nworkers = 10

    threads = [Thread(target=worker, args=(i, sema,)) for i in range(10)]
    for t in threads:
        t.start()

    for t in threads:
        t.join()

import time
from typing import TextIO
import subprocess
from multiprocessing import Process
import os


def follow(thefile: TextIO):
    """ even file is infinite it still works """
    thefile.seek(0, os.SEEK_END)
    while True:
        line = thefile.readline()
        if not line:
            time.sleep(0.1)
            continue
        yield line


if __name__ == "__main__":
    def ping():
        subprocess.call(['ping', 'google.com'], stdout=open('ping-data', 'w'))

    def run():
        with open('ping-data', 'r') as lines:
            for n in follow(lines):
                print(n)

    p1 = Process(target=run)
    p2 = Process(target=ping)

    p1.start()
    p2.start()
    p1.join()
    p2.join()


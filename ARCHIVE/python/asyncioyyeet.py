"""
Cooperative multitasking vs Preemptive multitasking:
    coroutines hands out control voluntarily, while threads
    can preempt other threads at the middle of the execution.

    coroutines need a even loop to schedule their execution, while
    threads are scheduled by operating system scheduler.

    Thus:
        - if one coroutine runs for too long it will block the
          entire program.
        - as a concequence one can control the access of shared
          resource and avoid using lock.
        -

"""

import asyncio
import time
import random
from typing import Tuple

"""
asyncio systems relied on even loop. in javascript
event loop is in the runtime.
"""


"""
use lock with async
"""


async def worker_lock(lock):
    print("get lock")
    async with lock:
        print('locked')
        time.sleep(2)
    print('unlocked')


async def run_lock():
    lock = asyncio.Lock()
    # this is seriously wierd.
    # is it some metaprogramming?
    await asyncio.wait([worker_lock(lock), worker_lock(lock)])


def main_lock():
    loop = asyncio.get_event_loop()
    loop.run_until_complete(run_lock())
    loop.close()


"""
streams
high level async primitive to work with network connections.
"""


async def tcp_echo_client(message):
    reader: asyncio.StreamReader
    writer: asyncio.StreamWriter

    reader, writer = await asyncio.open_connection('127.0.0.1', 8888)
    print(f'Send: {message!r}')
    writer.writer(message.encode())
    await writer.drain()

    data = await reader.read(100)
    print(f'Received: {data.decode() !r}')

    print("Close connection")
    writer.close()
    await writer.wait_closed()


def main_tcp_echo_client():
    asyncio.run(tcp_echo_client('Hello world!'))


if __name__ == "__main__":
    main_lock()

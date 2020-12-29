"""
Storing thread specific state.
"""
from socket import socket, AF_INET, SOCK_STREAM
import threading
from functools import partial


class LazyConnection:
    def __init__(self, address, familiy=AF_INET, type=SOCK_STREAM):
        self.address = address
        self.familiy = familiy
        self.type = type
        self.local = threading.local()

    def __enter__(self):
        """ with thread local each thread create their own socket """
        if hasattr(self.local, 'sock'):
            raise RuntimeError("Already connected")
        self.local.sock = socket(self.familiy, self.type)
        self.local.sock.connect(self.address)
        return self.local.sock

    def __exit__(self, exc_ty, exc_val, tb):
        self.local.sock.close()
        del self.local.sock


def test(conn):
    """
    Each thread has its own local connection, less chaotic.
    """
    with conn as s:
        s.send(b"GET /index.html HTTP/1.0\r\n")
        s.send(b"Host: www.python.org\r\n")
        s.send(b"\r\n")
        resp = b''.join(iter(partial(s.recv, 8192), b''))

    print("Got {} bytes".format(len(resp)))


if __name__ == "__main__":
    conn = LazyConnection(("www.python.org", 80))
    t1 = threading.Thread(target=test, args=(conn,))
    t2 = threading.Thread(target=test, args=(conn,))
    t1.start()
    t2.start()
    t2.join()
    t2.join()

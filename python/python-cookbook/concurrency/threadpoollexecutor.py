from socket import socket, AF_INET, SOCK_STREAM
from concurrent.futures import ThreadPoolExecutor
from queue import Queue
from threading import Thread


def echo_client1(sock: socket, client_addr):
    print("Got connection from ", client_addr)
    while True:
        msg = sock.recv(65536)
        if not msg:
            break
        sock.sendall(msg)
    print("Client closed connection")
    sock.close()


def echo_server1(addr):
    """ with thread pool """
    pool = ThreadPoolExecutor(128)
    sock = socket(AF_INET, SOCK_STREAM)
    sock.bind(addr)
    sock.listen(5)
    while True:
        client_sock, client_addr = sock.accept()
        pool.submit(echo_client1, client_sock, client_addr)


def echo_client2(q: Queue):
    client_sock, client_addr = q.get()
    print("Got connection from ", client_addr)
    while True:
        msg = client_sock.recv(65536)
        if not msg:
            break
        client_sock.sendall(msg)
    print("Client closed connection")
    client_sock.close()


def echo_server2(addr, nworker):
    """ with queue """
    q = Queue()
    for n in range(nworker):
        t = Thread(target=echo_client2, args=(q,))
        t.daemon = True
        t.start()

    sock = socket(AF_INET, SOCK_STREAM)
    sock.bind(addr)
    sock.listen(5)
    while True:
        client_sock, client_addr = sock.accept()
        q.put((client_sock, client_addr))


if __name__ == "__main__":
    thread_server1 = Thread(target=echo_server1, args=(('', 15000),))
    thread_server2 = Thread(target=echo_server2, args=(('', 15001), 128,))
    thread_server1.start()
    thread_server2.start()


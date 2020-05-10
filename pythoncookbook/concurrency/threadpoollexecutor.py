from socket import socket, AF_INET, SOCK_STREAM
from concurrent.futures import ThreadPoolExecutor


def echo_client(sock: socket, client_addr):
    print("Got connection from ", client_addr)
    while True:
        msg = sock.recv(65536)
        if not msg:
            break
        sock.sendall(msg)
    print("Client closed connection")
    sock.close()


def echo_server(addr):
    pool = ThreadPoolExecutor(128)
    sock = socket(AF_INET, SOCK_STREAM)
    sock.bind(addr)
    sock.listen(5)


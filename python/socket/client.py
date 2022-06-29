import socket
import sys

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# Connect the socket to the port where the server is listening

ip = '192.168.10.127'
port = 8080
server_address = (ip, port)
print(sys.stderr, 'connecting to %s port %s' % server_address)
sock.connect(server_address)

try:
    # Send data
    message = b'This is the message.  It will be repeated.'
    # print('sending "%s"' % message)
    # sock.sendall(message)
    # print(s.send(b'test message'))

    # Look for the response

    while True:
        data = sock.recv(512)
        # amount_received += len(data)
        print('received "%s"' % data)

finally:
    print(sys.stderr, 'closing socket')
    sock.close()



import socket
import datetime

# create an INET, STREAMing socket
serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
# bind the socket to a public host, and a well-known port
host = ""
serversocket.bind((host, 6789))

serversocket.listen(1)
print("listening connection from " + host)

def handle_client(client_sock):
    data = client_sock.recv(1024)
    # do sth with the data
    print(data)

def runloop():
    # accept connections from outside
    (clientsocket, address) = serversocket.accept()
    # now do something with the clientsocket
    # in this case, we'll pretend this is a threaded server
    handle_client(clientsocket)
    # ct = client_thread(clientsocket)
    # ct.run()
    while 1:
        data = clientsocket.recv(1024)
        if not data: break
        data = data.decode("utf-8")
        print(datetime.datetime.today().strftime("%Y-%m-%d %H:%M:%S"), address, data)
        clientsocket.close()
    runloop()

def main():
    try:
        runloop()
    except KeyboardInterrupt:
        serversocket.close()
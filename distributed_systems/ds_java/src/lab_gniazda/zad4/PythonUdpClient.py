# -*- coding: utf-8 -*-
import socket

serverIP = "127.0.0.1"
serverPort = 9008
msg = "Ping Python Udp"

print('PYTHON UDP CLIENT')
client = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
client.sendto(bytes(msg, 'utf-8'), (serverIP, serverPort))

response, server = client.recvfrom(1024)
print("received response:", response.decode('utf-8'))
client.close()





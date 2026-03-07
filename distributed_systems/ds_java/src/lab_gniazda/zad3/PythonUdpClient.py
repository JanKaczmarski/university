# -*- coding: utf-8 -*-
import socket

serverIP = "127.0.0.1"
serverPort = 9008
msg_bytes = (300).to_bytes(4, byteorder='little')

print('PYTHON UDP CLIENT')
client = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
# zamiana kodowania
client.sendto(msg_bytes, (serverIP, serverPort))

response, server = client.recvfrom(1024)
# zamiana bajty -> int (little-endian)
nb = int.from_bytes(response, byteorder='little')
print("received response:", nb)
client.close()





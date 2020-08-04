#!/usr/bin/env python3

"""
A python script to get battery level from Bluetooth headsets

Shamelessly stolen from https://github.com/TheWeirdDev/Bluetooth_Headset_Battery_Level
"""

# License: GPL-3.0
# Author: @TheWeirdDev
# 29 Sept 2019

import errno
import bluetooth
import sys


def send(sock, message):
    sock.send(b"\r\n" + message + b"\r\n")


def getATCommand(sock, line, device):
    blevel = -1

    if b"BRSF" in line:
        send(sock, b"+BRSF: 1024")
        send(sock, b"OK")
    elif b"CIND=" in line:
        send(sock, b"+CIND: (\"battchg\",(0-5))")
        send(sock, b"OK")
    elif b"CIND?" in line:
        send(sock, b"+CIND: 5")
        send(sock, b"OK")
    elif b"BIND=?" in line:
        # Announce that we support the battery level HF indicator
        # https://www.bluetooth.com/specifications/assigned-numbers/hands-free-profile/
        send(sock, b"+BIND: (2)")
        send(sock, b"OK")
    elif b"BIND?" in line:
        # Enable battery level HF indicator
        send(sock, b"+BIND: 2,1")
        send(sock, b"OK")
    elif b"XAPL=" in line:
        send(sock, b"+XAPL: iPhone,7")
        send(sock, b"OK")
    elif b"IPHONEACCEV" in line:
        parts = line.strip().split(b',')[1:]
        if len(parts) > 1 and (len(parts) % 2) == 0:
            parts = iter(parts)
            params = dict(zip(parts, parts))
            if b'1' in params:
                blevel = (int(params[b'1']) + 1) * 10
    elif b"BIEV=" in line:
        params = line.strip().split(b"=")[1].split(b",")
        if params[0] == b"2":
            blevel = int(params[1])
    else:
        send(sock, b"OK")

    if blevel != -1:
        print(blevel)
        return False

    return True


def main():
    if (len(sys.argv) < 2):
        print("Usage: bl_battery.py <BT_MAC_ADDRESS_1>[.PORT] ...")
        print("         Port number is optional (default = 4)")
        exit()
    else:
        for device in sys.argv[1:]:
            i = device.find('.')
            if i == -1:
                port = 10
            else:
                port = int(device[i+1:])
                device = device[:i]
            try:
                s = bluetooth.BluetoothSocket(bluetooth.RFCOMM)
                s.connect((device, port))
                while getATCommand(s, s.recv(128), device):
                    pass
                s.close()
            except OSError as e:
                print(f"{device} is offline", e)


if __name__ == "__main__":
    main()

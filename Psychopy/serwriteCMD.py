import serial
ser = serial.Serial(port = "",
                    baudrate=112500,
                    bytesize=8,
                    parity="N",
                    stopbits=1,
                    timeout=1)
ser.write(b '01')
line = ser.readline()
ser.close

#OR

import serial
ser = serial.Serial(port = "",
                    baudrate=112500,
                    bytesize=8,
                    parity="N",
                    stopbits=1,
                    timeout=1)
ser.write('01'.encode())
line = ser.readline()
ser.close

# based off forum https://discourse.psychopy.org/t/using-variable-parameter-from-conditions-file-in-code-component-for-value-to-be-sent-via-serial-port/5745/7
ser.write(bytes(thisTrial["stimuluscode"]))

#or
ser.write()(thistrial["stimuluscode"]).encode())

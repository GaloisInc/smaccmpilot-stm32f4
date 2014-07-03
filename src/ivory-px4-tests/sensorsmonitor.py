#!/usr/bin/env python
# sensors monitor
# Pat Hickey, 2 July 2014


import serial, sys, optparse, time, threading, struct
import hxframing, util

defaults = {
    'debug': False,
    'baudrate': 115200,
    'dsrdtr': False,
    'rtscts': False,
    'xonxoff': False }

class Barometer(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (ifail, sfail, pres, temp, t) = struct.unpack("<BBffQ", binary)
            self.ifail = ifail
            self.sfail = sfail
            self.pres  = pres
            self.temp  = temp
            self.t     = t
            self.errormsg = None
        except Exception:
            self.errormsg = ("Barometer: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Baro ifail %d sfail %d mmhg %4.4f degc %2.2f micros %d" %
            (self.ifail, self.sfail, self.pres, self.temp, self.t))

class Compass(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (ifail, sfail, x, y, z, t) = struct.unpack("<BBhhhQ", binary)
            self.ifail = ifail
            self.sfail = sfail
            self.x     = x
            self.y     = y
            self.z     = z
            self.t     = t
            self.errormsg = None
        except Exception:
            self.errormsg = ("Compass: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Compass ifail %d sfail %d x %4d y %4d z %4d micros %d" %
            (self.ifail, self.sfail, self.x, self.y, self.z, self.t))

class Gyro(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (valid, gx, gy, gz, ax, ay, az, temp, t) = struct.unpack("<BhhhhhhhQ", binary)
            self.valid = valid
            self.gx    = gx
            self.gy    = gy
            self.gz    = gz
            self.ax    = ax
            self.ay    = ay
            self.az    = az
            self.temp  = temp
            self.t     = t
            self.errormsg = None
        except Exception:
            self.errormsg = ("Gyro: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Gyro valid %d gx %4d gy %4d gz %4d ax %4d ay %4d az %4d temp %d micros %d" %
            (self.valid, self.gx, self.gy, self.gz, self.ax, self.ay, self.az, self.temp, self.t))

class SensorParser(object):
    def __init__(self,radio,opts):
        self.radio = radio
    def receive(self):
        sensors = []
        statuses   = []
        frames = self.radio.read()
        for f in frames:
            (t,payload) = hxframing.untag(f)
            if t == 1:
                parsed = util.at_b_response(payload)
                if parsed != None:
                    statuses.append(parsed)
            if t == 98: # 'b' barometer
                sensors.append(Barometer(payload))
            if t == 99: # 'c' compass
                sensors.append(Compass(payload))
            if t == 103: # 'g' gyro
                sensors.append(Gyro(payload))
        return (statuses, sensors)

class SerialPortProvider(object):
    def __init__(self, device, opts):
        self.device = device
        self.baudrate = opts['baudrate']
        self.rtscts = opts['rtscts']
        self.dsrdtr = opts['dsrdtr']
        self.xonxoff = opts['xonxoff']
        self.opener_active = False
        self.port = None
        self.exception = ""

        self.launch_opener()

    def alive(self):
        return self.port != None

    def launch_opener(self):
        self.port = None
        launch_thread = threading.Thread(target=self.opener_thread)
        launch_thread.daemon = True
        launch_thread.start()

    def opener_thread(self):
        if self.opener_active:
            return
        self.opener_active = True
        while True:
            try:
                self.try_open()
                break
            except Exception as e:
                self.exception = str(e)
                time.sleep(1)
        self.opener_active = False

    def try_open(self):
        self.port = serial.Serial(self.device,
                self.baudrate,
                timeout=0,
                dsrdtr=self.dsrdtr,
                rtscts=self.rtscts,
                xonxoff=self.xonxoff)

    #
    # Delegation:
    #
    def write(self, b):
        if self.alive():
            try:
                self.port.write(b)
            except serial.SerialException:
                self.launch_opener()
    def flush(self):
        if self.alive():
            try:
                self.port.flush()
            except serial.SerialException:
                self.launch_opener()
    def read(self, count):
        if self.alive():
            try:
                return self.port.read(count)
            except serial.SerialException:
                self.launch_opener()
        return []
    def inWaiting(self):
        if self.alive():
            try:
                return self.port.inWaiting()
            except serial.SerialException:
                self.launch_opener()
        return 0

class RadioInterface(object):
    def __init__(self, port, opts):
        self.debug_enabled = opts['debug']
        self.unframer = hxframing.Unframer()
        self.port = port
    def write(self, payload, tag=0):
        buf = hxframing.frame(payload,tag)
        self.debug(">> ", util.bindebug( buf ))
        self.port.write(buf)
        self.port.flush()

    def _read(self):
        buf = []
        try:
            count = self.port.inWaiting()
            if count > 0:
                buf = self.port.read(count)
        except Exception:
            return []
        return buf

    def read(self):
        frames = []
        buf = self._read()
        if len(buf) > 0:
            self.debug("<< ", util.bindebug(buf))
        for i in range(len(buf)):
            got = self.unframer.parse(buf[i])
            if got:
                f = self.unframer.getframe()
                frames.append(f)
        return frames

    def debug(self, prefix, s):
        if self.debug_enabled:
            print(prefix + s)



if __name__ == "__main__":
    parser = optparse.OptionParser("sensors monitor")
    parser.add_option("--delay", type='float', default=0.1, help='delay between polling')

    parser.add_option("--baudrate", type='int', default=defaults['baudrate'], help='baud rate')
    parser.add_option("--rtscts", action='store_true', default=defaults['rtscts'], help='enable rtscts')
    parser.add_option("--dsrdtr", action='store_true', default=defaults['dsrdtr'], help='enable dsrdtr')
    parser.add_option("--xonxoff", action='store_true', default=defaults['xonxoff'], help='enable xonxoff')
    parser.add_option("--debug", action='store_true', default=defaults['debug'], help='enable debug text')

    opts, args = parser.parse_args()

    if len(args) != 1:
        print("usage: sensorsmonitor.py [opts] <DEVICE>")
        sys.exit(1)

    device = args[0]


    topts = dict(defaults)

    for k in topts:
        if hasattr(opts, k):
            topts[k] = getattr(opts, k)

    port = SerialPortProvider(device, topts)
    radio = RadioInterface(port, topts)
    parser = SensorParser(radio, topts)
    try:
        while True:
            (statuses, sensors) = parser.receive()
            for s in statuses:
                print ('Status< ' + str(s))
            for s in sensors:
                print ('Sensor< ' + s.display())

            if opts.delay > 0.0:
                time.sleep(opts.delay)
    except KeyboardInterrupt:
        sys.exit(0)


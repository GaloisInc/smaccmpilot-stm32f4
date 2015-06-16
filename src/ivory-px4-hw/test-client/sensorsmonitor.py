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
    'xonxoff': False,
    'rawlog': None
    }

def display_numeric_list(l):
    return ' '.join('{0:9.4f}'.format(x) for x in l)

class Barometer(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (ifail, sfail, pres, temp, t) = struct.unpack("!BBffQ", binary)
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
        return ("Baro  %d %d mbar % 9.4f degc % 5.2f micros %d" %
            (self.ifail, self.sfail, self.pres, self.temp, self.t))

class Mag(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (ifail, sfail, cal, x, y, z, t) = struct.unpack("!BBBfffQ", binary)
            self.ifail = ifail
            self.sfail = sfail
            self.cal   = cal
            self.x     = x
            self.y     = y
            self.z     = z
            self.t     = t
            self.errormsg = None
        except Exception:
            self.errormsg = ("Mag: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Mag   %d %d x % 9.4f y % 9.4f z % 9.4f                cal %d micros %d" %
            (self.ifail, self.sfail, self.x, self.y, self.z, self.cal, self.t))


class Accel(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (ifail, sfail, x, y, z, temp, t) = struct.unpack("!BBffffQ", binary)
            self.ifail = ifail
            self.sfail = sfail
            self.x     = x
            self.y     = y
            self.z     = z
            self.t     = t
            self.temp  = temp
            self.errormsg = None
        except Exception:
            self.errormsg = ("Accel: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Accel %d %d x % 9.4f y % 9.4f z % 9.4f temp % 9.4f micros %d" %
            (self.ifail, self.sfail, self.x, self.y, self.z, self.temp, self.t))

class Fusion(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            fields = struct.unpack("!22f", binary)
            self.orient = fields[0:4]
            self.vel = fields[4:7]
            self.pos = fields[7:10]
            self.gyro_bias = fields[10:13]
            self.wind = fields[13:16]
            self.mag_ned = fields[16:19]
            self.mag_xyz = fields[19:22]
            self.errormsg = None
        except Exception:
            self.errormsg = ("Fusion: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return "Fusion " + ' '.join(
            field + " " + display_numeric_list(getattr(self, field))
            for field in ('orient', 'vel', 'pos', 'gyro_bias', 'wind', 'mag_ned', 'mag_xyz')
        )

class Gyro(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (ifail, sfail, cal, x, y, z, temp, t) = struct.unpack("!BBBffffQ", binary)
            self.ifail = ifail
            self.sfail = sfail
            self.cal   = cal
            self.x     = x
            self.y     = y
            self.z     = z
            self.temp  = temp
            self.t     = t
            self.errormsg = None
        except Exception:
            self.errormsg = ("Gyro: bad size %d" % (len(binary) if binary else 0))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Gyro  %d %d x % 9.4f y % 9.4f z % 9.4f temp % 9.4f cal %d micros %d" %
            (self.ifail, self.sfail, self.x, self.y, self.z, self.temp, self.cal, self.t))

class Position(object):
    def __init__(self, binary):
        self.binary = binary
        try:
          ##  (ifail, sfail, x, y, z, t) = struct.unpack("!BBhhhQ", binary)
            (fix, num_sv, dop, lat, lon, alt, vnorth, veast, vdown, vground, heading, t) = \
                    struct.unpack("!BBfllllllLfQ", binary)
            self.fix        = fix
            self.num_sv     = num_sv
            self.dop        = dop
            self.lat        = lat
            self.lon        = lon
            self.alt        = alt
            self.vnorth     = vnorth
            self.veast      = veast
            self.vdown      = vdown
            self.vground    = vground
            self.heading    = heading
            self.t          = t
            self.errormsg = None
        except Exception:
            self.errormsg = ("Position : bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("Position fix %d sats %d dop %f lat %d lon %d alt %d vnorth %d veast %d vdown %d vground %d heading %f millis %d" %
            (self.fix, self.num_sv, self.dop, self.lat, self.lon, self.alt, self.vnorth, self.veast, self.vdown, self.vground, self.heading, self.t))

class PPM(object):
    def __init__(self, binary):
        self.binary = binary
        try:
            (c1, c2, c3, c4, c5, c6, c7, c8) = \
                    struct.unpack("!HHHHHHHH", binary)
            self.c1 = c1
            self.c2 = c2
            self.c3 = c3
            self.c4 = c4
            self.c5 = c5
            self.c6 = c6
            self.c7 = c7
            self.c8 = c8
            self.errormsg = None
        except Exception:
            self.errormsg = ("PPM: bad size %d" % (len(binary)))
    def display(self):
        if self.errormsg:
            return self.errormsg
        return ("PPM %d %d %d %d %d %d %d %d" %
            (self.c1, self.c2, self.c3, self.c4, self.c5, self.c6, self.c7, self.c8))

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
            if t == ord('a'): # 'a' accel
                sensors.append(Accel(payload))
            if t == ord('b'): # 'b' barometer
                sensors.append(Barometer(payload))
            if t == ord('m'): # 'm' magnetometer
                sensors.append(Mag(payload))
            if t == ord('f'): # 'f' fusion
                sensors.append(Fusion(payload))
            if t == ord('g'): # 'g' gyro
                sensors.append(Gyro(payload))
            if t == ord('p'): # 'p' position
                sensors.append(Position(payload))
            if t == ord('P'): # 'P' PPM
                sensors.append(PPM(payload))
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

        if opts['rawlog']:
            self.log = open(opts['rawlog'], 'w')
        else:
            self.log = None

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
                b = self.port.read(count)
                if self.log:
                    self.log.write(b)
                return b
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
    parser.add_option("--rawlog", action='store', default=defaults['rawlog'], help='log output')

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

            sys.stdout.flush()

            if opts.delay > 0.0:
                time.sleep(opts.delay)
    except KeyboardInterrupt:
        sys.exit(0)


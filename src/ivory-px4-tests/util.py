
import struct

def bindebug(buf):
    s = "["
    for i in range(len(buf)):
        if i > 0:
            s += ", "
        b = struct.unpack_from('<B', buf, i)[0]
        s += ("0x%0.2x" % b)
    s += "]"
    return s

at_b_request = struct.pack('<BB', ord('B'), ord('\r'))

def at_b_response(buf):
    try:
        (h,loc_rssi, loc_noise, loc_rx, rem_rssi, rem_noise, rem_rx,
                tx_errs, rx_errs, ser_tx_ovf, ser_rx_ovf, corr_errs, corr_pkts,
                ser_tx_ok, ser_rx_ok) = struct.unpack('>BBBHBBHHHHHHHHH', buf)
        if h == ord('B'):
            return dict({'loc_rssi': loc_rssi,
                'loc_noise': loc_noise,
                'loc_rx': loc_rx,
                'rem_rssi': rem_rssi,
                'rem_noise': rem_noise,
                'rem_rx': rem_rx,
                'tx_errs': tx_errs,
                'rx_errs': rx_errs,
                'ser_tx_ovf': ser_tx_ovf,
                'ser_rx_ovf': ser_rx_ovf,
                'corr_errs': corr_errs,
                'corr_pkts': corr_pkts,
                'ser_tx_ok': ser_tx_ok,
                'ser_rx_ok': ser_rx_ok})
        else:
            return None
    except Exception:
        return None


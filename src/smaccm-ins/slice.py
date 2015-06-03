#!/usr/bin/env python

import math
import sys

def raw_gyro(rad):
    return rad / math.pi * 180 * 16.4

calculated_columns = dict(
    { "roll":    (("q0", "q1", "q2", "q3"), lambda q0, q1, q2, q3: math.atan2(2 * (q0 * q1 + q2 * q3), 1 - 2 * (q1 * q1 + q2 * q2)))
    , "pitch":   (("q0", "q1", "q2", "q3"), lambda q0, q1, q2, q3: math.asin(2 * (q0 * q2 - q3 * q1)))
    , "yaw":     (("q0", "q1", "q2", "q3"), lambda q0, q1, q2, q3: math.atan2(2 * (q0 * q3 + q1 * q2), 1 - 2 * (q2 * q2 + q3 * q3)))
    , "rgx":     (("gx",), raw_gyro)
    , "rgy":     (("gy",), raw_gyro)
    , "rgz":     (("gz",), raw_gyro)
    , "rgbiasx": (("gbiasx",), raw_gyro)
    , "rgbiasy": (("gbiasy",), raw_gyro)
    , "rgbiasz": (("gbiasz",), raw_gyro)
    })

def columngetter(columns, name):
    calculator = calculated_columns.get(name)
    if calculator is not None:
        getters = columnsgetter(columns, calculator[0])
        return lambda row: calculator[1](*getters(row))

    idx = columns.index(name)
    return lambda row: float(row[idx])

def columnsgetter(columns, names):
    if not names:
        raise ValueError("must specify at least one column")
    getters = tuple(columngetter(columns, name) for name in names)
    return lambda row: tuple(getter(row) for getter in getters)

if __name__ == "__main__":
    columns = sys.stdin.readline().strip().split()
    selected_columns = sys.argv[1:]
    try:
        get_columns = columnsgetter(columns, selected_columns)
    except ValueError, e:
        print >>sys.stderr, e.message
        print >>sys.stderr, "usage: {0} columns...".format(sys.argv[0])
        print >>sys.stderr, "columns:", ' '.join(columns + calculated_columns.keys())
        sys.exit(1)

    print '\t'.join(selected_columns)
    for line in sys.stdin:
        row = get_columns(line.strip().split())
        print '\t'.join(map(str, row))

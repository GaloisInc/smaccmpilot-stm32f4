#!/usr/bin/env python

import sys
import pylab
import math

columns =   [ "time"
            , "ax"
            , "ay"
            , "az"
            , "gx"
            , "gy"
            , "gz"
            , "mx"
            , "my"
            , "mz"
            , "q0" # 10
            , "q1" # 11
            , "q2" # 12
            , "q3" # 13
            , "gbiasx"
            , "gbiasy"
            , "gbiasz"
            , "magn"
            , "mage"
            , "magd"
            , "magx"
            , "magy"
            , "magz"
            ]

def euler_angle(q):
    q0 = q[0]
    q1 = q[1]
    q2 = q[2]
    q3 = q[3]
    return [ math.atan2( 2 * (q0 * q1 + q2 * q3), 1 - 2 * (q1 * q1 + q2 * q2))
           , math.asin(2 * (q0 * q2 - q3 * q1))
           , math.atan2(2 * (q0 * q3 + q1 * q2), 1 - 2 * (q2 * q2 + q3 * q3))
           ]

def raw_gyro(rad):
    return rad / math.pi * 180 * 16.4

calculated_columns = dict(
    { "roll": lambda (row) : euler_angle(row[10:14])[0]
    , "pitch": lambda (row) : euler_angle(row[10:14])[1]
    , "yaw": lambda (row) : euler_angle(row[10:14])[2]
    , "rgx": lambda (row) : raw_gyro(row[4])
    , "rgy": lambda (row) : raw_gyro(row[5])
    , "rgz": lambda (row) : raw_gyro(row[6])
    })

def parse(f, cs):
    rows = dict({})
    for c in cs:
        rows[c] = []
    for c in calculated_columns:
        rows[c] = []
    for line in f:
        elems = line.split(' ')
        row = []
        for e in elems:
            try:
                row.append(float(e))
            except ValueError:
                pass
        if len(row) == len(cs):
            for i in range(0,len(row)):
                rows[cs[i]].append(row[i])
            for c in calculated_columns:
                rows[c].append(calculated_columns[c](row))
    return rows

if __name__ == "__main__":
    if len(sys.argv) > 1:
        rows = parse(sys.stdin, columns);
        for arg in sys.argv[1:]:
            if arg in rows:
                pylab.plot(rows["time"], rows[arg], label=arg)
            else:
                print "no column named " + arg
        pylab.legend()
        pylab.show()
    else:
        print "must provide columns as arguments"
        print "valid columns:"
        print (columns + calculated_columns.keys())

#!/usr/bin/env python

import sys
import pylab


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
            , "baro"
            , "orient_0"
            , "orient_1"
            , "orient_2"
            , "orient_3"
            , "vx"
            , "vy"
            , "vz"
            , "px"
            , "py"
            , "pz"
            , "gbiasx"
            , "gbiasy"
            , "gbiasz"
            , "windn"
            , "winde"
            , "windd"
            , "magn"
            , "mage"
            , "magd"
            , "magx"
            , "magy"
            , "magz"
            ]

def parse(f, cs):
    rows = dict({})
    for c in cs:
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
        print columns

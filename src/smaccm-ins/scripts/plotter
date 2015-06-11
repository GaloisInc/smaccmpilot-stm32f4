#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy
import sys

if __name__ == "__main__":
    columnnames = sys.stdin.readline().strip().split()
    columns = list(numpy.loadtxt(sys.stdin, unpack=True))

    times = None
    try:
        timecol = columnnames.index("time")
    except ValueError:
        pass
    else:
        times = columns.pop(timecol)
        del columnnames[timecol]

    for name, column in zip(columnnames, columns):
        if times is not None:
            plt.plot(times, column, label=name)
        else:
            plt.plot(column, label=name)

    plt.legend()
    plt.show()

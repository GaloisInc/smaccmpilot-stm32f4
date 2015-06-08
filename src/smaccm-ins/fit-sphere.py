#!/usr/bin/env python

import numpy
import pprint
import scipy.optimize
import sys

def fit_sphere(xs, ys, zs):
    def residual(p):
        a, b, c, r = p
        return numpy.sqrt((xs - a) ** 2 + (ys - b) ** 2 + (zs - c) ** 2) - r

    initial = [0.0, 0.0, 0.0, 0.0]
    return scipy.optimize.leastsq(residual, initial, full_output=True)

if __name__ == "__main__":
    samples = numpy.loadtxt(sys.stdin, unpack=True, skiprows=1)
    result = fit_sphere(*samples)
    pprint.pprint(result)

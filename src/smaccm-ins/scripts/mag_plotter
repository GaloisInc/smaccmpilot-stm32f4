#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy
import sys
import math

def sample_differential(d):
    dist = []
    prev = (d['mx'][0], d['my'][0], d['mz'][0])
    for x, y, z in zip (d['mx'], d['my'], d['mz']):
        prev_x, prev_y, prev_z = prev
        dx = x - prev_x
        dy = y - prev_y
        dz = z - prev_z
        dist.append(math.sqrt(dx*dx + dy*dy + dz*dz))
        prev = (x, y, z)
    return dist

def est_distance_from_last(d):
    dist = []
    end = (d['mbe_x'][-1], d['mbe_y'][-1], d['mbe_z'][-1])
    for x, y, z in zip (d['mbe_x'], d['mbe_y'], d['mbe_z']):
        end_x, end_y, end_z = end
        dx = x - end_x
        dy = y - end_y
        dz = z - end_z
        distance = math.sqrt(dx*dx + dy*dy + dz*dz)
        if distance > 500: # just to make the graph look better
            distance = 500
        dist.append(distance)
    return dist

if __name__ == "__main__":
    columnnames = sys.stdin.readline().strip().split()
    columns = list(numpy.loadtxt(sys.stdin, unpack=True))

    d = dict({})
    for name, column in zip(columnnames, columns):
        d[name] = column

    xy = plt.subplot(221)
    xy.scatter(d['mx'], d['my'], c='b', label='measurement')
    xy.scatter(d['mbe_x'][-1], d['mbe_y'][-1], c='r', label='bias')

    yz = plt.subplot(222)
    yz.scatter(d['my'], d['mz'], c='b', label='measurement')
    yz.scatter(d['mbe_y'][-1], d['mbe_z'][-1], c='r', label='bias')

    xz = plt.subplot(223)
    xz.scatter(d['mx'], d['mz'], c='b', label='measurement')
    xz.scatter(d['mbe_x'][-1], d['mbe_z'][-1], c='r', label='bias')

    points = plt.subplot(224)
    points.plot(d['mbe_good'], label='bias points')


    points.plot(sample_differential(d), label='measurement distance')
    points.plot(est_distance_from_last(d), label='estimate distance')

    plt.show()



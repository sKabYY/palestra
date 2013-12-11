#!/usr/bin/env python

from math import pi, sin, cos


def spiral(n, a):
    thetas = map(
        lambda n: pi / 5 + pi / 20 * n,
        xrange(n))
    rs = thetas
    return map(
        lambda (r, theta): (r * cos(theta + a), r * sin(theta + a)),
        zip(rs, thetas))


def gen_data():
    n = 100
    posdata = spiral(n, 0)
    negdata = spiral(n, pi)
    return (posdata, negdata)


def to_erldat(label, xys):
    return map(
        lambda (x, y): '{%s, [{1, %s}, {2, %s}]}' % (label, x, y),
        xys)


def save(fn, posdata, negdata):
    buf = []
    buf.extend(to_erldat(1, posdata))
    buf.extend(to_erldat(-1, negdata))
    with open(fn, 'w') as f:
        data = '[%s].' % ('\n,'.join(buf))
        print >>f, data


def draw(xys, opt):
    import matplotlib.pyplot as plt
    xs = map(lambda (x, y): x, xys)
    ys = map(lambda (x, y): y, xys)
    plt.plot(xs, ys, opt, mfc='none')


def draw_and_show(posdata, negdata):
    import matplotlib.pyplot as plt
    draw(posdata, 'ok')
    draw(negdata, 'xk')
    plt.show()


def main(will_draw):
    (posdata, negdata) = gen_data()
    save('testdata/twospirals.erldat', posdata, negdata)
    if will_draw:
        draw_and_show(posdata, negdata)


if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == 'draw':
        main(True)
    else:
        main(False)

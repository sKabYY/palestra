#!/usr/bin/env python


def randfloat():
    import random
    f = random.random()
    return 2 * f - 1


def gen_and_write(fn, num):
    buf = []
    with open(fn, 'w') as f:
        for _ in xrange(num):
            x = randfloat()
            y = randfloat()
            if x * y > 0:
                label = 1
            else:
                label = -1
            sample = '{%s, [{1, %s}, {2, %s}]}' % (label, x, y)
            buf.append(sample)
        data = '[%s].' % (',\n'.join(buf))
        print >>f, data


def main(num_train, num_test):
    def P(fn):
        _dir = 'testdata'
        return '%s/%s' % (_dir, fn)
    trainfn = P('traindata.erldat')
    testfn = P('testdata.erldat')
    gen_and_write(trainfn, num_train)
    gen_and_write(testfn, num_test)


if __name__ == '__main__':
    import sys
    num_train = int(sys.argv[1])
    num_test = int(sys.argv[2])
    main(num_train, num_test)

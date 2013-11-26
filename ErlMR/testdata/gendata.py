#!/usr/bin/env python


def randfloat():
    import random
    f = random.random()
    return 2 * f - 1


def gen_and_write(fn, num):
    with open(fn, 'w') as f:
        for _ in xrange(num):
            x = randfloat()
            y = randfloat()
            if x * y > 0:
                label = 1
            else:
                label = -1
            newline = '{%s, [{1, %s}, {2, %s}]}.' % (label, x, y)
            print >>f, newline


def main():
    num_train = 10000
    num_test = 1000
    trainfn = 'traindata.erldat'
    testfn = 'testdata.erldat'
    gen_and_write(trainfn, num_train)
    gen_and_write(testfn, num_test)


if __name__ == '__main__':
    main()

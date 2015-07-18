#!/usr/bin/env python

from itertools import tee


def printn(n, stream):
    for _ in xrange(n):
        print stream.next(),
    print


def integers_starting_from(i):
    while True:
        yield i
        i += 1


def stream_discard(stream, n):
    for _ in xrange(n):
        stream.next()


def stream_map(func, stream):
    while True:
        yield func(stream.next())


def stream_filter(pred, stream):
    while True:
        x = stream.next()
        if pred(x):
            yield x


def sieve():
    def divisible(x):
        return lambda e: e % x != 0
    stream = integers_starting_from(2)
    while True:
        x = stream.next()
        yield x
        stream = stream_filter(
            divisible(x),
            #lambda e: e % x != 0,  <-- Don't use this!
            stream)


def partial_sums(stream):
    acc = stream.next()
    yield acc
    while True:
        acc += stream.next()
        yield acc


def pi_summands():
    n = 1
    sign = 1
    while True:
        yield 1.0 / n * sign
        n += 2
        sign *= -1


def pi_stream():
    return stream_map(
        lambda x: 4 * x,
        partial_sums(pi_summands()))


def euler_transform(stream):
    def transform(s0, s1, s2):
        ds = s2 - s1
        return s2 - ds * ds / (s0 + s2 - 2 * s1)
    s0 = stream.next()
    s1 = stream.next()
    while True:
        s2 = stream.next()
        yield transform(s0, s1, s2)
        s0, s1 = s1, s2


def accelerated_sequence(transform, stream):
    s = stream
    while True:
        s, s_bak = tee(s)
        yield s.next()
        s = transform(s_bak)


def add_stream(s1, s2):
    while True:
        yield s1.next() + s2.next()


def stream_cons(v, f):
    yield v
    while True:
        yield f().next()


def partial_sums_with_init(stream, init):
    def _s():
        return stream_cons(init, lambda: add_stream(stream, feedback))
    output, feedback = tee(_s())
    return output


def integral(stream, init, dt):
    def _s():
        return stream_cons(
            init,
            lambda: add_stream(
                stream_map(
                    lambda x: x * dt,
                    stream),
                feedback))
    output, feedback = tee(_s())
    return output


def delayed_integral(delayed_stream, init, dt):
    def _s():
        return stream_cons(
            init,
            lambda: add_stream(
                stream_map(
                    lambda x: x * dt,
                    delayed_stream()),
                feedback))
    output, feedback = tee(_s())
    return output


def solve(f, y0, dt):
    '''
    dy/dt = f(y),
    y(0) = y0
    flow:

                                         y0
                                         |
                                         |
                                         v
                  +----------+      +----------+
                  |          |  dy  |          |
              +-->|  map: f  +----->| integral +---+----> y
              |   |          |      |          |   |
              |   +----------+      +----------+   |
              |                                    |
              |                                    |
              +------------------------------------+
    '''
    def _s():
        dy = lambda: stream_map(f, feedback)
        return delayed_integral(dy, y0, dt)
    output, feedback = tee(_s())
    return output


def odd(x):
    return x % 2 == 1


def even(x):
    return x % 2 == 0


def test_sieve():
    printn(100, sieve())


def test_partial_sums():
    printn(10,
           partial_sums(
               stream_filter(odd, integers_starting_from(1))))


def test_pi_stream():
    printn(10, pi_stream())


def test_euler_transform():
    printn(10, euler_transform(pi_stream()))


def test_accelerated_sequence():
    # Will overflow after 9.
    printn(9, accelerated_sequence(euler_transform, pi_stream()))


def test_add_stream():
    printn(10, add_stream(integers_starting_from(1),
                          integers_starting_from(2)))


def test_partial_sums_with_init():
    printn(10,
           partial_sums_with_init(
               integers_starting_from(1), 0))


def test_integral():
    '''
    1/3 * x^3 {x<-1} = 1/3
    '''
    n = 10000
    stream = integral(
        stream_map(
            lambda x: x * x,
            stream_map(
                lambda x: float(x) / n,
                integers_starting_from(0))),
        0,
        1.0 / n)
    stream_discard(stream, n)
    print stream.next()


def test_solve():
    '''
    dy/dt = y, y0 = 1 => y1 = e
    '''
    n = 10000
    stream = solve(lambda y: y, 1, 1.0 / n)
    stream_discard(stream, n)
    print stream.next()


def test_all(l):
    for f in l:
        print '%s:' % f.__name__
        f()
        print


if __name__ == '__main__':
    test_all([
        test_sieve,
        test_partial_sums,
        test_pi_stream,
        test_euler_transform,
        test_accelerated_sequence,
        test_add_stream,
        test_partial_sums_with_init,
        test_integral,
        test_solve,
    ])

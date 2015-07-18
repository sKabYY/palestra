(load "stream.scm")

(stream-for-n println
              (partial-sums integers)
              10)

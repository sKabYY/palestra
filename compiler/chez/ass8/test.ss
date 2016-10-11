(load "../lib/match.ss")
(load "../lib/helpers.ss")
(load "../lib/driver.ss")
(load "../lib/fmts.pretty")
;(load "../lib/wrapper.ss")
(load "a8-wrapper.ss")

(load "tests8.ss")

(define src
  ((language-wrapper) 'verify-uil (list-ref tests 27)))

(pretty-print src)
(display "output: ")
(pretty-print ((game-eval) src))

#lang racket

(require "wirelib.rkt")

(define (print-logical s) (displayln s))

(let* ((input1 (make-input))
       (input2 (make-input))
       (and-gate (make-and-gate 'a 'b 'output))
       (output (make-probe print-logical)))
  (wire! input1 (get-pin and-gate 'a))
  (wire! input2 (get-pin and-gate 'b))
  (wire! (get-pin and-gate 'output) output)
  (set-input! input1 logical-1)
  (set-input! input2 logical-1)
  (propagate))

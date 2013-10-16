(load "machine.scm")

(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

(define (println x)
  (begin
    (display x)
    (newline)))

(println (set-register-contents! gcd-machine 'a 206))
(println (set-register-contents! gcd-machine 'b 40))
(println (start gcd-machine))
(println (get-register-contents gcd-machine 'a))

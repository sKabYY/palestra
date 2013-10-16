(load "ck-machine.rkt")

(define (driver-loop)
  (let ((input (read-input)))
    (if (eof-object? input)
      'done
      (let ((output (eval-ck input)))
        (display-output output)
        (driver-loop)))))

(define input-prompt "> ")
(define output-prompt "")

(define (read-input)
  (display input-prompt)(read))

(define (display-output output)
  (display output-prompt)(display output)(newline))

; Run!
(driver-loop)
(newline)(display 'Bye~)(newline)

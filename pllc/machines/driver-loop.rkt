(define (driver-loop eval)
  (let ((input (read-input)))
    (if (eof-object? input)
      (begin (newline)(display 'Bye~)(newline))
      (let ((output (with-handlers
                      ((exn:fail?
                         (lambda (x)
                           (string-append "Error: " (exn-message x)))))
                      (eval input))))
        (display-output output)
        (driver-loop eval)))))

(define input-prompt "> ")
(define output-prompt "")

(define (read-input)
  (display input-prompt)(read))

(define (display-output output)
  (display output-prompt)(display output)(newline))

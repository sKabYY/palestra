(load "stream.scm")

; M_k => (s_k, t_k)
; M_0 = 0
; M_k + 2^k => (s_k, t_(k+1))
; M_k + 2^k + 2^k => (s_(k+1), t_(k+1))
; M_(k+1) = M_k + 2^(k+1)
; M_k = 2^(k+1) - 2
; M_k + 2^k + j * 2^(k+1) => (s_k, t_(k+1+j))
; M_k + (2*j - 1) * 2^k => (s_k, t_(k+j))

(define ipairs (pairs integers integers))

(define (=pair? p1 p2) (equal? p1 p2))

(define (count-ipairs p stream)
  (define (iter n s)
    (if (=pair? p (stream-car s))
      n
      (iter (+ n 1) (stream-cdr s))))
  (iter 0 stream))

; (1, 100): (integers_0, integers_99)
; k = 0
; j = 99
; M_k = 2^1 - 2 = 0
; M_k + (2*j - 1) * 2^k = 0 + (2*99 - 1) * 2^0 = 197

(display (count-ipairs (list 1 100) ipairs))
(newline)

(define (loop proc low high)
  (if (> low high)
    'done
    (begin
      (proc low)
      (loop proc (+ low 1) high))))

(define (M k) (- (expt 2 (+ k 1)) 2))

(loop
  (lambda (n)
    (begin
      (display (count-ipairs (list n n) ipairs))
      (display "::::")
      (display (M (- n 1)))
      (newline)))
  1
  10)

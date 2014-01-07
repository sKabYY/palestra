#lang racket

(provide logical-0
         logical-1
         logical-eq
         logical-not
         logical-and
         logical-or)

(provide wire!
         propagate
         make-input
         set-input!
         make-probe
         make-and-gate
         get-pin)

(require data/queue)

; logical
(define logical-0 #f)
(define logical-1 #t)
(define (logical-eq a b) (eqv? a b))
(define (logical-not s) (not s))
(define (logical-and a b) (and a b))
(define (logical-or a b) (or a b))
;;;;;;

; agenda
(define (empty-agenda) (make-queue))
(define (empty-agenda? a) (queue-empty? a))
(define (dequeue-agenda! a) (dequeue! a))
(define (enqueue-agenda! a item) (enqueue! a item))
(define the-agenda (empty-agenda))
(define (set-agenda! a) (set! the-agenda a))
(define pieces 0)
(define (inc-pieces!) (set! pieces (+ pieces 1)))
(define (initialize!)
  (set! the-agenda (empty-agenda))
  (set! pieces 0))
;;;;;;

; agenda-item
(define (make-item delay action) (cons delay action))
(define (expired-item? pieces item) (>= pieces (car item)))
(define (do-item item) ((cdr item)))
;;;;;;

(define (after-delay delay action)
  (lambda () (enqueue-agenda! the-agenda (make-item delay action))))

; pin
(define (make-pin)
  (let ((signal logical-0)
        (actions '()))
    (define (pin-set! v)
      (if (eqv? signal v)
          (void)
          (begin
            (set! signal v)
            (call-each actions))))
    (define (pin-add-action! delay action)
      (set! actions (cons (after-delay delay action) actions)))
    (lambda (msg)
      (cond ((eqv? msg 'get) signal)
            ((eqv? msg 'set) pin-set!)
            ((eqv? msg 'add-action) pin-add-action!)))))

(define (get-signal pin) (pin 'get))
(define (set-signal! pin v) ((pin 'set) v))
(define (add-action! pin delay action)
  ((pin 'add-action) delay action))
;;;;;;

; input
(define (make-input) (make-pin))
(define (set-input! input v) (set-signal! input v))
;;;;;;

; probe
(define (make-probe probe-proc)
  (define probe-delay 0)
  (let ((pin (make-pin)))
    (add-action! pin
                 probe-delay
                 (lambda ()
                   (probe-proc (get-signal pin))))
    pin))
;;;;;;

; component
(define (report-undefined-pin-error sym)
  (error "undefined pin:" sym))

(define (call-each procs)
  (for-each (lambda (proc) (proc)) procs))

(define (make-component delay inputs outputs)
  (define (register-actions actions)
    (for-each (lambda (sym-pin)
                (add-action! (cadr sym-pin)
                             delay
                             (lambda () (call-each actions))))
              inputs))
  (define (do-make-component pins)
    (lambda (sym)
      (let ((sym-pin (assoc sym pins)))
        (if (list? sym-pin)
            (cadr sym-pin)
            (report-undefined-pin-error sym)))))
  (define (proc->action proc pin)
    (lambda ()
      (set-signal! pin
                   (apply proc
                          (map (lambda (sym-pin)
                                 (get-signal (cadr sym-pin)))
                               inputs)))))
  (define (iter pins actions outputs)
    (if (null? outputs)
        (begin
          (register-actions actions)
          (do-make-component pins))
        (let ((out-tuple (car outputs)))
          (let ((sym (car out-tuple))
                (pin (cadr out-tuple))
                (proc (caddr out-tuple)))
            (iter (cons (list sym pin) pins)
                  (cons (proc->action proc pin) actions)
                  (cdr outputs))))))
  (iter inputs '() outputs))

(define (get-pin component sym) (component sym))
;;;;;;

; and-gate
(define (make-and-gate a b o)
  (define and-gate-delay 1)
  (let ((input1 (make-pin))
        (input2 (make-pin))
        (output (make-pin)))
    (make-component and-gate-delay
                    (list
                     (list a input1)
                     (list b input2))
                    (list
                     (list o output logical-and)))))
;;;;;;

(define (wire! pin1 pin2)
  (define wire-delay 0)
  (define (wire1 pin-from pin-to)
    (add-action! pin-from
                 wire-delay
                 (lambda ()
                   (set-signal! pin-to
                                (get-signal pin-from)))))
  (wire1 pin1 pin2)
  (wire1 pin2 pin1))

(define max-iteration 10)

(define (report-max-iteration-warning)
  (displayln "[WARN] reach max iteration"))

(define (propagate)
  (define (update)
    (define (iter count old-agenda delayed-agenda)
      (if (> count max-iteration)
          (begin
            (report-max-iteration-warning)
            (set-agenda! delayed-agenda))
          (if (empty-agenda? old-agenda)
              (if (empty-agenda? the-agenda)
                  (set-agenda! delayed-agenda)
                  (begin
                    (let ((new-agenda the-agenda))
                      (set-agenda! (empty-agenda))
                      (iter (+ count 1) new-agenda delayed-agenda))))
              (let ((item (dequeue-agenda! old-agenda)))
                (if (expired-item? pieces item)
                    (do-item item)
                    (enqueue-agenda! delayed-agenda item))
                (iter count old-agenda delayed-agenda)))))
    (if (empty-agenda? the-agenda)
        'done
        (begin
          (iter 0 (empty-agenda) (empty-agenda))
          (inc-pieces!)
          (update))))
  (update))

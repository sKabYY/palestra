(defvar *state* nil)
(defvar *ops* nil)

(defstruct op
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun gps (*state* goals *ops*)
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  ;(print `(> goal ,goal))
  (or (member goal *state*)
      (some #'apply-op (remove-if-not #'(lambda (op) (appropriate-p goal op)) *ops*))))

(defun appropriate-p (goal op)
  (member goal (op-add-list op)))

(defun apply-op (op)
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))


(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
      :preconds '(son-at-home car-works)
      :add-list '(son-at-school)
      :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
      :preconds '(car-needs-battery shop-knows-problem shop-has-money)
      :add-list '(car-works))
    (make-op :action 'tell-shop-problem
      :preconds '(in-communication-with-shop)
      :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
      :preconds '(know-phone-number)
      :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
      :preconds '(have-phone-book)
      :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
      :preconds '(have-money)
      :add-list '(shop-has-money)
      :del-list '(have-money))))


(print '--)
(princ "case 1:")
(print (gps '(son-at-home car-needs-battery have-money have-phone-book)
            '(son-at-school)
            *school-ops*))

(print '--)
(princ "case 2:")
(print (gps '(son-at-home car-needs-battery have-money)
            '(son-at-school)
            *school-ops*))

(print '--)
(princ "case 3:")
(print (gps '(son-at-home car-works)
            '(son-at-school)
            *school-ops*))

(print '--)
(princ "case 4:")
(print (gps '(son-at-home car-needs-battery have-money have-phone-book)
            '(have-money son-at-school)
            *school-ops*))

(print '--)
(princ "case 5:")
(print (gps '(son-at-home car-needs-battery have-money have-phone-book)
            '(son-at-school have-money)
            *school-ops*))

(print '--)
(princ "case 6:")
(print (gps '(son-at-home car-needs-battery have-money)
            '(have-money)
            *school-ops*))

(print '-)
(defun executing-p (x) (starts-with x 'executing))
(defun starts-with (list x) (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  (convert-op
    (make-op :action action :preconds preconds :add-list add-list :del-list del-list)))

(defvar *ops* nil)

(defstruct op
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun gps (state goals &optional (*ops* *ops*))
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve-all (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
      current-state)))

(defun achieve (state goal goal-stack)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (remove-if-not #'(lambda (op) (appropriate-p goal op)) *ops*)))))

(defun member-equal (item list) (member item list :test #'equal))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  (let ((state2 (achieve-all state (op-preconds op) (cons goal goal-stack))))
    (unless (null state2)
      (append (remove-if #'(lambda (x) (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))


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

(setf *ops* (mapc #'convert-op *school-ops*))


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
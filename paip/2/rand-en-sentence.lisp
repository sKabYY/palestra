(defparameter *simple-grammer*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took swa liked))
  "A grammer for trivial subset of English.")

(defvar *grammer* *simple-grammer*
  "The grammer used by generate. Initially, this is
  *simple-grammer*, but we can switch to other grammers.")

(defun rule-lhs (rule) (first rule))
(defun rule-rhs (rule) (rest (rest rule)))
(defun rewrites (category) (rule-rhs (assoc category *grammer*)))

(defun random-elt (lst) (elt lst (random (length lst))))

(defun generate (phrase)
  (cond
    ((listp phrase) (apply #'append (mapcar #'generate phrase)))
    ((let ((rhs (rewrites phrase)))
       (if rhs (generate (random-elt rhs)) nil)))
    (t (list phrase))))

(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'sentence))
(print '-)
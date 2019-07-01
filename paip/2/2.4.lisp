(defparameter *simple-grammer*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> a the)
    (Noun -> man ball woman table)
    (Verb -> hit took swa liked))
  "A grammer for trivial subset of English.")

(defvar *grammer* *simple-grammer*
  "The grammer used by generate. Initially, this is
  *simple-grammer*, but we can switch to other grammers.")

(defun rule-lhs (rule) (first rule))
(defun rule-rhs (rule) (rest (rest rule)))
(defun rewrites (category) (rule-rhs (assoc category *grammer*)))

(defun mappend (fn lst) (apply #'append (mapcar fn lst)))

(defun generate-all (phrase)
  (cond
    ((null phrase) (list nil))
    ((listp phrase) (combine-all (generate-all (first phrase))
                                 (generate-all (rest phrase))))
    ((rewrites phrase) (mappend #'generate-all (rewrites phrase)))
    (t (list (list phrase)))))

(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (x) (mapcar #'(lambda (y) (funcall fn x y)) ylist)) xlist))

(defun combine-all (xlist ylist) (cross-product #'append xlist ylist))

(print (generate-all 'Article))
(print (generate-all 'Noun))
(print (generate-all 'noun-phrase))
(print (length (generate-all 'sentence)))
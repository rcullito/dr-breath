(load "breathe.lisp")

(defparameter *page-example-1* "       32                                                                 theory was that increased efficiency of breathing would reduce")
(defparameter *page-example-2* "                                                                                   33")
(defparameter *example-chapter-heading* "       Dr. Breath                                                          What   Is It?")

(left-column *example-chapter-heading* *delimeter*)


(defun chapter-heading-right-column ()
  (equal "What   Is It?"
         (right-column *example-chapter-heading* *delimeter*)))

(defun chapter-heading-left-column ()
  "bob")

(every (lambda (x) (funcall x))
       '(chapter-heading-left-column
         chapter-heading-right-column))


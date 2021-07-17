(load "breathe.lisp")

(defparameter *page-example-1* "       32                                                                 theory was that increased efficiency of breathing would reduce")
(defparameter *page-example-2* "                                                                                   33")
(defparameter *example-page-heading* "       Dr. Breath                                                          What   Is It?")




(defun chapter-heading-right-column ()
  (equal "What   Is It?"
         (right-column *example-page-heading* *delimeter*)))

(defun chapter-heading-left-column ()
  (left-column *example-page-heading* *delimeter*))

(every (lambda (x) (funcall x))
       '(chapter-heading-left-column
         chapter-heading-right-column))

(search *delimeter* *example-page-heading*)

;; process page heading needs to return a cons cell of left and right, that
;; another function writes to the stream
(cdr (cons "Dr. Breath" "hi"))

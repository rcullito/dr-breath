(load "breathe.lisp")

(defparameter *page-example-1* "       32                                                                 theory was that increased efficiency of breathing would reduce")
(defparameter *page-example-2* "                                                                                   33")
(defparameter *example-page-heading* "       Dr. Breath                                                          What   Is It?")

(defun page-heading-test ()
  (equal (process-page-heading *example-page-heading*)
         '("Dr. Breath" . "What   Is It?")))

(every (lambda (x) (funcall x))
       '(page-heading-test))

;; (search *delimeter* *example-page-heading*)

;; ;; process page heading needs to return a cons cell of left and right, that
;; ;; another function writes to the stream
;; (cdr (cons "Dr. Breath" "hi"))

(load "breathe.lisp")

(defparameter *example-page-heading* "       Dr. Breath                                                          What   Is It?")
(defparameter *example-page-number-1* "       32                                                                 theory was that increased efficiency of breathing would reduce")
(defparameter *example-page-number-2* "                                                                                   33")
(defparameter *example-prose-line* "controlled at will and can effectively influence the passage of    undue stress on any part of the respiratory apparatus are sus-")

(defun page-heading-test ()
  (equal (process-page-heading *example-page-heading*)
         '("Dr. Breath" . "What   Is It?")))

(defun page-number-test-1 ()
    (equal (process-page-number *example-page-number-1*)
           '("32" . "theory was that increased efficiency of breathing would reduce")))

(defun page-number-test-2 ()
  (equal (process-page-number *example-page-number-2*)
         '("" . "33")))

(defun prose-line-test ()
    (equal (process-prose-line *example-prose-line*)
     '("controlled at will and can effectively influence the passage of" . "undue stress on any part of the respiratory apparatus are sus-")))

(defun all-tests-pass (tests)
  (every (lambda (x) (funcall x)) tests))

;; TODO
;; handle blank lines in a similar fashion

;; (all-tests-pass
;;  '(page-heading-test
;;    page-number-test-1
;;    page-number-test-2
;;    prose-line-test))


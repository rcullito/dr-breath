(defun trim-space (text)
  (when text
    (string-trim '(#\Space) text)))

(defun line-num-p (text)
  (every #'digit-char-p (trim-space text)))

(defun make-page-predicate (odd-or-even)
  (lambda (text)
    (let ((page-integer (parse-integer (trim-space text))))
      (and (funcall odd-or-even page-integer) (> page-integer 10)))))

(defvar odd-page-num-p (make-page-predicate #'oddp))
(defvar even-page-num-p (make-page-predicate #'evenp))

(defmacro file->file (input-file output-file &body body)
  `(with-open-file (output-stream ,output-file :direction :output :if-exists :supersede)
    (with-open-file (input-stream ,input-file :direction :input)
      (loop
      for current-line = (read-line input-stream nil 'eof) 
      until (eq current-line 'eof)
      do ,@body))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  ;; values returns the objects as multiple values
  (values (intern (apply #'mkstr args))))

(defvar *small-numbers*
  (loop for n from 2 to 99
        collect n))

(defmacro gambit (operator)
  `(progn ,@(mapcar (lambda (baked-in-n)
                      `(defun ,(symb operator baked-in-n)
                           (x)
                         (,operator x ,baked-in-n)))
                    *small-numbers*)))

;; exposes predicates
(gambit >)
(gambit <)



(defun safe-subseq (txt start &optional end)
  (let* ((txt-length (length txt))
         (handled-text (cond
                        ((>= start txt-length) nil)
                        (end (subseq txt start (min txt-length end)))
                        (t (subseq txt start)))))
    (trim-space handled-text)))


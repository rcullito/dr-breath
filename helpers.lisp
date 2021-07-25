(defun page-heading-p (txt)
  (search "  Dr. Breath  " txt))

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
      ,@body)))



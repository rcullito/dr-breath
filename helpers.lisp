(defparameter *line-cutoff* 65)

(defun page-heading-p (txt)
  (search "  Dr. Breath  " txt))

(defun trim-space (text)
  (string-trim '(#\Space) text))

(defun line-num-p (text)
  (every #'digit-char-p (trim-space text)))

(defun make-page-predicate (odd-or-even)
  (lambda (text)
    (let ((page-integer (parse-integer (trim-space text))))
      (and (funcall odd-or-even page-integer) (> page-integer 10)))))

(defvar odd-page-num-p (make-page-predicate #'oddp))
(defvar even-page-num-p (make-page-predicate #'evenp))

(defun first-half (text)
  (subseq text 0 *line-cutoff*))

(defun short-line-p (text)
  (< (length text) *line-cutoff*))

(defvar long-line-p (complement #'short-line-p))

(defun false-start-p (text delim)
  (equal delim (subseq text 0 (length delim))))

(defmacro file->file (input-file output-file &body body)
  `(with-open-file (output-stream ,output-file :direction :output :if-exists :supersede)
    (with-open-file (input-stream ,input-file :direction :input)
      ,@body)))



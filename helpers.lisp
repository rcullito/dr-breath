(defparameter *line-cutoff* 65)

(defun page-heading-p (txt)
  (search "  Dr. Breath  " txt))

(defun line-num-p (text)
  (some #'digit-char-p text))

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

(defun page-heading-p (txt)
  (search "  Dr. Breath  " txt))

(defun line-num-p (text)
  (some #'digit-char-p text))

(defun first-half (text)
  (subseq text 0 65))


(defmacro file->file (input-file output-file &body body)
  `(with-open-file (output-stream ,output-file :direction :output :if-exists :supersede)
    (with-open-file (input-stream ,input-file :direction :input)
      ,@body)))

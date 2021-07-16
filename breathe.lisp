(defparameter *delimeter* "    ")

(defun left-column (txt delim)
  (let ((end-of-left-column (search delim txt)))
    (subseq txt 0 end-of-left-column)))

(defun right-column (txt delim)
  (let ((adjustment (length delim))
        (backwards-search-position (search delim txt :from-end :backward)))
    (when backwards-search-position
      (let ((beginning-of-right-column (+ backwards-search-position adjustment)))
            (subseq txt beginning-of-right-column)))))

(with-open-file (my-stream "sample.txt" :direction :input)
  (loop
    for current-line = (read-line my-stream nil 'eof) ;; sets eof-error-p to nil and eof-value to 'eof
    until (eq current-line 'eof)
    do
       (let* ((left-part (left-column current-line *delimeter*))
              (right-part (right-column current-line *delimeter*)))
         (when right-part
           (print right-part)))))

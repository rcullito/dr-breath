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
  ;; todo split on blank space, and acc the left in one var and the right in another
  (loop
    for current-line = (read-line my-stream nil 'eof)
    until (eq current-line 'eof)
    do
       (let* ((first-line (left-column current-line *delimeter*))
              (second-line (right-column current-line *delimeter*)))
         (when second-line
           (print second-line)))))

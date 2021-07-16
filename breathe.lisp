(defparameter *delimeter* "    ")

(defun left-column (txt delim)
  (let ((end-of-left-column (search delim txt)))
    ;; we can't do a when here because there will be some valid
    ;; lines in the corpus that will not have a gap
    (subseq txt 0 end-of-left-column)))

;; (subseq "howdy good sir" 0 nil)

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
         ;; we can't alternate per line, we need to send each to a different stream and then join
         ;; those at the end of a page
         (when (not (equal "" left-part))
           (print left-part))
         (when right-part
           (print right-part)))))


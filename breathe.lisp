(defparameter *delimeter* "    ")
(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun left-column (txt delim)
  (let ((end-of-left-column (search delim txt)))
    ;; we can't do a when here because there will be some valid
    ;; lines in the corpus that will not have a gap
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
         (when (not (equal "" left-part))
           (write-line left-part *left-stream*))
         (when right-part
           (write-line right-part *right-stream*)))))


;; (progn 
;;   (get-output-stream-string *left-stream*)
;;   (get-output-stream-string *right-stream*))

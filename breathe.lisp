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

(defun line->text-streams (text)
  (let* ((left-part (left-column text *delimeter*))
         (right-part (right-column text *delimeter*)))
    (when (not (equal "" left-part))
      (write-line left-part *left-stream*))
    (when right-part
      (write-line right-part *right-stream*))))

(defun transcribe (input-file output-file)
  (with-open-file (output-stream output-file :direction :output)
    (with-open-file (input-stream input-file :direction :input)
      (loop
        for current-line = (read-line input-stream nil 'eof) ;; sets eof-error-p to nil and eof-value to 'eof
        until (eq current-line 'eof)
        do
           (line->text-streams current-line))
      (progn
        (write-line (get-output-stream-string *left-stream*) output-stream)
        (write-line (get-output-stream-string *right-stream*) output-stream)))))

;; (transcribe "sample.txt" "built.txt")


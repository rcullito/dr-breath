(load "helpers.lisp")

(defparameter *delimeter* "   ")
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

;; processors

(defun process-page-heading (text)
  (cons "Dr. Breath" (right-column text *delimeter*)))

(defun process-page-number (text)
  (cons (when (funcall long-line-p text)
          (string-trim '(#\Space) (first-half text)))
        (right-column text *delimeter*)))

(defun process-prose-line (text)
  (cond
    ((short-line-p text) (cons (subseq text 0 (min (length text) 65)) nil))
    (t (cons (left-column text *delimeter*) (right-column text *delimeter*)))))

;; io

(defun strings->text-streams (cell)
  (when (car cell)
      (write-line (car cell) *left-stream*))
  (when (cdr cell)
      (write-line (cdr cell) *right-stream*)))

(defun flush-output-streams-to-file (text output-stream)
  (when (funcall even-page-num-p text)
      (write-line (get-output-stream-string *left-stream*) output-stream))
  (when (funcall odd-page-num-p text)
    (write-line (get-output-stream-string *right-stream*) output-stream)))


;; workhorses

(defun line->text-streams (text output-stream)
  (cond
    ((zerop (length text)) nil)
    ((line-num-p text) (progn (strings->text-streams (process-page-number text))
                              (flush-output-streams-to-file text output-stream)))
    ((page-heading-p text) (strings->text-streams (process-page-heading text)))
    (t (strings->text-streams (process-prose-line text)))))

(defun transcribe (input-file output-file)
  (file->file input-file output-file
    (loop
      ;; set eof-error-p to nil and eof-value to 'eof
      for current-line = (read-line input-stream nil 'eof) 
      until (eq current-line 'eof)
      do
         (line->text-streams current-line output-stream))))


;; TODO if page number is even, flush left, if odd, flush right. that is the key to it all!
(transcribe "Chapter5.txt" "built.txt")

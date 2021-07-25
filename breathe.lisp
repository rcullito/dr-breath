(load "helpers.lisp")

(defparameter *column-break* 67)
(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun left-column (txt)
  (subseq txt 0 (min (length txt) *column-break*)))

(defun right-column (txt)
  (when (> (length txt) *column-break*)
    (subseq txt *column-break*)))

;; processors

;; todo make a macro that defines a function
;; that trims space and cons's 2 things

;; TODO macro to make >67 a predicate

(defun process-page-heading (text)
  (cons "Dr. Breath" (trim-space (right-column text))))

(defun process-page-number (text)
  (cons (when (funcall long-line-p text)
          (trim-space (first-half text)))
        (trim-space (right-column text))))

(defun process-prose-line (text)
  (cons (trim-space (left-column text)) (trim-space (right-column text))))

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
;; (transcribe "Chapter5.txt" "built.txt")


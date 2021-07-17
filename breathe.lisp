(load "helpers.lisp")

(defparameter *delimeter* "    ")
(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun process-line-num (text)
  (when (> (length text) 65)
      (let ((first-half (first-half text)))
     (when (some #'digit-char-p first-half)
       (write-line (string-trim '(#\Space) first-half) *left-stream*)))
      (write-line (right-column text *delimeter*) *right-stream*)))

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

(defun flush-output-streams-to-file (output-stream)
;;  (write-line "toast-left" *left-stream*)
  (write-line (get-output-stream-string *left-stream*) output-stream)
;;  (write-line "toast-right" *right-stream*)
  (write-line (get-output-stream-string *right-stream*) output-stream))

(defun process-page-heading (text output-stream)
  ;; TODO we will also have to do this for chapter title pages
  (flush-output-streams-to-file output-stream)
  (cons "Dr. Breath" (right-column text *delimeter*)))

(defun strings->text-streams (cell)
  (write-line (car cell) *left-stream*)
  (write-line (cdr cell) *right-stream*))

(defun process-prose-line (text)
  (let* ((left-part (left-column text *delimeter*))
         (right-part (right-column text *delimeter*)))
    (when (not (equal "" left-part))
      (write-line left-part *left-stream*))
    (when right-part
      (write-line right-part *right-stream*))))

(defun line->text-streams (text output-stream)
  (cond
    ((zerop (length text)) (flush-output-streams-to-file output-stream))
    ((line-num-p text) (process-line-num text))
    ((page-heading-p text) (strings->text-streams (process-page-heading text output-stream)))
    (t (process-prose-line text))))

(defun transcribe (input-file output-file)
  (with-open-file (output-stream output-file :direction :output)
    (with-open-file (input-stream input-file :direction :input)
      (loop
        for current-line = (read-line input-stream nil 'eof) ;; sets eof-error-p to nil and eof-value to 'eof
        until (eq current-line 'eof)
        do
           (line->text-streams current-line output-stream)))))

;; (transcribe "sample2.txt" "built.txt")









*example-chapter-heading*





(search "platypus" *example-chapter-heading*)


(parse-integer "sure" :junk-allowed t)






(left-column *page-example-1* *delimeter*)
(right-column *page-example-1* *delimeter*)




(if (some #'digit-char-p (first-half *page-example-2*))
    (string-trim '(#\Space) *page-example-2*)
    "")




(some #'digit-char-p (subseq *page-example-2* 0 65))

(left-column *page-example-2* *delimeter*)
(right-column *page-example-2* *delimeter*)

(some #'digit-char-p *page-example-1*)

(some #'digit-char-p *page-example-2*)

(some #'digit-char-p *example-chapter-heading*)


(defparameter *vexing-line* "  ti                                           ' a mg comp 1-")
(length *vexing-line*)
(line-num-p *vexing-line*)


(load "helpers.lisp")

(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun process-line (txt)
  (split-into-columns txt 67))

(defun flush-streams (text file-stream)
  (empty-left-page text file-stream)
  (empty-right-page text file-stream))

(defun line->text-streams (text output-stream)
  (multiple-value-bind (left right) (process-line text)
    (when left
      (write-line left *left-stream*))
    (when right
      (write-line right *right-stream*)))
  (when (line-num-p text)
    (flush-streams text output-stream)))

(defun transcribe (input-file output-file)
  (file->file input-file output-file
    (line->text-streams current-line output-stream)))


;; if you go through and prune the page numbers to
;; consistently be left, even
;;                 right, odd
;; then this works like a charm, wahoo!

;; (transcribe "Chapter5.txt" "built.txt")






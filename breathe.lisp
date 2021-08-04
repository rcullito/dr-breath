
(load "helpers.lisp")

(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun flush-streams (text file-stream)
  (empty-left-page text file-stream)
  (empty-right-page text file-stream))

(defun process-line (text output-stream)
  (when-multi (left right) (split-into-columns text)
    (write-line left *left-stream*)
    (write-line right *right-stream*))
  (when (line-num-p text)
    (flush-streams text output-stream)))

(defun transcribe (input-file output-file)
  (file->file input-file output-file
    (process-line current-line output-stream)))


;; if you go through and prune the page numbers to
;; consistently be left, even
;;                 right, odd
;; then this works like a charm, wahoo!

;; (transcribe "Chapter5.txt" "built.txt")







(load "helpers.lisp")

(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun process-line (text file-stream)
  (write-lines-to-text-streams)
  (when (line-num-p text)
    (flush-text-streams-to-file)))

(defun transcribe (input-file output-file)
  (file->file input-file output-file
    (process-line it output-stream)))


;; if you go through and prune the page numbers to
;; consistently be left, even
;;                 right, odd
;; then this works like a charm, wahoo!

;; (transcribe "Chapter5.txt" "built.txt")






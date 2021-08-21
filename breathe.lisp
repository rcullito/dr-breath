
(load "helpers.lisp")

(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defmacro flush-text-streams-to-file ()
  `(progn
    (empty-left-page text file-stream)
    (empty-right-page text file-stream)))

(defmacro write-lines-to-text-streams ()
  `(when-multi (left right) (split-into-columns text)
    (write-line left *left-stream*)
    (write-line right *right-stream*)))

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







(load "helpers.lisp")

(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun process-line (txt)
  (let* ((left-part (safe-subseq txt 0 67))
         (right-part (safe-subseq txt 67)))
    (values left-part right-part)))

(defun strings->text-streams (left right)
  (write-line left *left-stream*)
  (write-line right *right-stream*))

(defun flush-streams (text file-stream)
  (empty-left-page text file-stream)
  (empty-right-page text file-stream))

(defun line->text-streams (text output-stream)
  (strings->text-streams (process-line text))
  (when (line-num-p text)
    (flush-streams text output-stream)))

(defun transcribe (input-file output-file)
  (file->file input-file output-file
    (line->text-streams current-line output-stream)))


;; (transcribe "Chapter5.txt" "built.txt")

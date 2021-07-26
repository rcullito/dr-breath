(load "helpers.lisp")

(defparameter *left-stream* (make-string-output-stream))
(defparameter *right-stream* (make-string-output-stream))

(defun process-line (txt)
  (let* ((left-part (safe-subseq txt 0 67))
         (right-part (safe-subseq txt 67)))
    (values left-part right-part)))

(defun strings->text-streams (cell)
  (write-line (car cell) *left-stream*)
  (write-line (cdr cell) *right-stream*))

(defun flush-output-streams-to-file (text output-stream)
  (when (funcall even-page-num-p text)
      (write-line (get-output-stream-string *left-stream*) output-stream))
  (when (funcall odd-page-num-p text)
    (write-line (get-output-stream-string *right-stream*) output-stream)))

(defun line->text-streams (text output-stream)
  (strings->text-streams (process-prose-line text)))

(defun transcribe (input-file output-file)
  (file->file input-file output-file
    (line->text-streams current-line output-stream)))



(load "prerequisites.lisp")

(defun trim-space (text)
  (when text
    (string-trim '(#\Space) text)))

(defun line-num-p (text)
  (every #'digit-char-p (trim-space text)))

(defun safe-subseq (txt start &optional end)
  (let* ((txt-length (length txt))
         (handled-text (cond
                        ((>= start txt-length) nil)
                        (end (subseq txt start (min txt-length end)))
                        (t (subseq txt start)))))
    (trim-space handled-text)))

(defun split-into-columns (txt threshold)
  (values (safe-subseq txt 0 threshold)
          (safe-subseq txt threshold)))

(defun make-page-predicate (odd-or-even)
  (lambda (text)
    (when (> (length (trim-space text)) 0)
      (let ((page-integer (parse-integer (trim-space text))))
        (and (funcall odd-or-even page-integer) (> page-integer 10))))))


(defmacro! empty-page (page-type base-pred page-stream)
  (let ((page-pred (make-page-predicate base-pred)))
    `(defun ,(symb 'empty- page-type '-page) (,g!text ,g!file-stream)
      (when (funcall ,page-pred ,g!text)
        (write-line (get-output-stream-string ,page-stream)
                    ,g!file-stream)))))

(empty-page left #'evenp *left-stream*)
(empty-page right #'oddp *right-stream*)


(defmacro file->file (input-file output-file &body body)
  `(with-open-file (output-stream ,output-file :direction :output :if-exists :supersede)
    (with-open-file (input-stream ,input-file :direction :input)
      (loop
      for current-line = (read-line input-stream nil 'eof) 
      until (eq current-line 'eof)
      do ,@body))))

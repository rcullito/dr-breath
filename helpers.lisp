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

(let ((threshold 67))
  (defun split-into-columns (txt)
   (values (safe-subseq txt 0 threshold)
           (safe-subseq txt threshold))))

(defun make-page-predicate (odd-or-even)
  (lambda (text)
    (when (> (length (trim-space text)) 0)
      (let ((page-integer (parse-integer (trim-space text))))
        (and (funcall odd-or-even page-integer) (> page-integer 10))))))


(defmacro! empty-page (page-type base-pred page-stream)
  `(let ((,g!page-pred ,(make-page-predicate base-pred)))
    (defun ,(symb 'empty- page-type '-page) (,g!text ,g!file-stream)
      (when (funcall ,g!page-pred ,g!text)
        (write-line (get-output-stream-string ,page-stream)
                    ,g!file-stream)))))

(empty-page left evenp *left-stream*)
(empty-page right oddp *right-stream*)


;; On Lisp, by Paul Graham. pg 191
(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))


(defmacro file->file (input-file output-file &body body)
  `(with-open-file (output-stream ,output-file :direction :output :if-exists :supersede)
     (with-open-file (input-stream ,input-file :direction :input)
       (awhile (read-line input-stream nil)
         ,@body))))


(defmacro mac (expr)
  `(macroexpand-1 ',expr))

(defmacro when-multi ((binding1 binding2) binding-form &body body)
  `(multiple-value-bind (,binding1 ,binding2) ,binding-form
     (when ,binding1
       ,(first body))
     (when ,binding2
       ,(second body))))

(defmacro flush-text-streams-to-file ()
  `(progn
    (empty-left-page text file-stream)
    (empty-right-page text file-stream)))

(defmacro write-lines-to-text-streams ()
  `(when-multi (left right) (split-into-columns text)
    (write-line left *left-stream*)
    (write-line right *right-stream*)))


;; (with-open-file (input-stream "test.txt" :direction :input)
;;   (awhile (read-line input-stream nil)
;;    (print it)))

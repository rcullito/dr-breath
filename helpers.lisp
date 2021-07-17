(defun page-heading-p (txt)
  (search "  Dr. Breath  " txt))

(defun line-num-p (text)
  (some #'digit-char-p text))

(defun first-half (text)
  (subseq text 0 65))

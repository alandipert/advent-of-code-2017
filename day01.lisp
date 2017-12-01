(defun day01 (str &optional (offset 1))
  (loop for i from 0 to (1- (length str))
        for c1 = (aref str i)
        for c2 = (aref str (mod (+ offset i) (length str)))
        when (eq c1 c2)
        sum (digit-char-p c1)))

;; For first part of puzzle:
;; (day01 input)
;; For second part of puzzle:
;; (day01 input (floor (/ (length input) 2)))

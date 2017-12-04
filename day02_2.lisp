#!/usr/bin/sbcl script
;; sbcl --script day02_2.lisp < ~/Desktop/input

(defun parse (line)
  (let* ((nums (coerce (read-from-string (concatenate 'string "(" line ")")) 'vector))
         (len (1- (length nums))))
    (loop named outer
          for i from 0 to len
          do (loop for j from 0 to len
                   do (when (not (eq i j))
                        (let ((pair (sort (mapcar (lambda (n) (aref nums n)) (list i j)) #'>)))
                          (when (zerop (apply #'mod pair))
                            (return-from outer (apply #'/ pair)))))))))

(princ
 (loop
   for line = (read-line nil nil nil)
   while line
   sum (parse line)))

#!/usr/bin/sbcl script

(defun split-string (str delim-char)
  (loop for pos0 = -1 then pos1
        for pos1 = (position delim-char str :start (1+ pos0))
        collect (subseq str (1+ pos0) (or pos1 (length str)))
        while pos1))

(defun pairs (list)
  (mapcan (lambda (pair)
			(list pair (reverse pair)))
		  (mapcon (lambda (tail)
					(mapcar (lambda (x)
							  (list (car tail) x))
							(cdr tail)))
				  list)))

(defun parse (line)
  (mapcar #'read-from-string
		  (split-string line #\tab)))

(princ
 (loop
   for line = (read-line nil nil nil)
   while line
   sum (parse line)))

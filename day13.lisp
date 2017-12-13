;; CL port of Mike Fikes's awesome solution at
;; https://github.com/mfikes/advent-of-code/blob/master/src/advent_2017/day_13.cljc

(defun read-input (f)
  (with-open-file (stream f)
    (loop for line = (remove #\: (read-line stream nil nil))
          while line
          collect (read-from-string (concatenate 'string "(" line ")")))))

(defparameter *input* (read-input "/Users/alandipert/Desktop/input"))

(defun caught? (depth range)
  (zerop (mod depth (* 2 (1- range)))))

(defun part-1 ()
  (loop for (depth range) in *input*
        when (caught? depth range)
          sum (* depth range)))

(defun part-2 ()
  (do ((delay 0 (1+ delay)))
      ((loop for (depth range) in *input*
             when (caught? (+ depth delay) range)
               do (return nil)
             finally (return t))
       delay)))

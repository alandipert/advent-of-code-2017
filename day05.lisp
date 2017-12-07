(defun read-input (f)
  (with-open-file (stream f)
    (let ((nums (loop for line = (read stream nil nil)
                      while line
                      collect line)))
      (coerce nums '(simple-array fixnum (*))))))

(defparameter *input* (read-input "/Users/alandipert/Desktop/input"))

;; Adapted from http://lpaste.net/360573 by jdz on in #lisp on freenode

(defmacro make-runner (jmp-expr)
  `(lambda (array)
    (declare (type (simple-array fixnum (*)) array)
             (optimize (speed 3) (safety 0)))
    (flet ((jmp-fun (%) ,jmp-expr))
      (let* ((i 0)
             (j 0)
             (array (copy-seq array))
             (length (length array))
             (steps 0))
        (declare (type fixnum i j steps))
        (loop
          (incf steps)
          (setf j (aref array i))
          (incf (aref array i) (jmp-fun j))
          (incf i j)
          (when (<= length i)
            (return steps)))))))

;; part 1

(time (funcall (make-runner (progn % 1)) *input*))

;; part 2

(time (funcall (make-runner (if (<= 3 %) -1 1)) *input*))

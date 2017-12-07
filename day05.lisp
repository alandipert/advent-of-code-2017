;;;; http://adventofcode.com/2017/day/5

;;;; util funs

(defun read-input (f)
  (with-open-file (stream f)
    (coerce
     (loop for line = (read stream nil nil)
           while line
           collect line)
     'simple-vector)))

(defparameter input (read-input "/Users/alandipert/Desktop/input"))

;;;; part 1

(defun ev1! (code ptr &key (jmp-fun #'1+))
  (let ((jmp (aref code ptr)))
    (setf (aref code ptr) (funcall jmp-fun jmp))
    (+ ptr jmp)))

(defun exited? (code-length ptr)
  (declare (optimize (speed 3))
           (type fixnum code-length ptr))
  (or (>= ptr code-length) (< ptr 0)))

(defun run (code evfun)
  (declare (optimize (speed 3))
           (type simple-vector code)
           (type function evfun))
  (loop with steps = 0
        with ptr = 0
        with code = (copy-seq code)
        with code-length = (length code)
        until (exited? code-length ptr)
        do (progn (setf ptr (funcall evfun code ptr))
                  (incf (the fixnum steps)))
        finally (return steps)))

(time (run input #'ev1!))

;;;; part 2

(defun ev2! (code ptr)
  (declare (optimize (speed 3))
           (type simple-vector code))
  (let* ((jmp (aref code ptr))
         (new-jmp (+ jmp (if (>= jmp 3) -1 1))))
    (declare (type fixnum jmp new-jmp))
    (setf (aref code ptr) new-jmp)
    (the fixnum (+ ptr jmp))))

(time (run input #'ev2!))

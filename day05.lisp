(defun evcode! (code ptr &key (jmp-fun #'1+))
  (let ((jmp (aref code ptr)))
    (setf (aref code ptr) (funcall jmp-fun jmp))
    (+ ptr jmp)))

(defun exited? (code-length ptr)
  (or (>= ptr code-length) (< ptr 0)))

(defun run (code &key (jmp-fun #'1+))
  (loop with steps = 0
        with ptr = 0
        with code = (copy-seq code)
        with code-length = (length code)
        until (exited? code-length ptr)
        do (progn (setf ptr (evcode! code ptr :jmp-fun jmp-fun))
                  (incf steps))
        finally (return steps)))

;; part1
(run input)

;; part2
(run input :jmp-fun (lambda (jmp) (+ jmp (if (>= jmp 3) -1 1))))

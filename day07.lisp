(defstruct program name weight aboves)

(defun read-input (f)
  (with-open-file (stream f)
    (loop with structs = (make-hash-table)
          for line = (substitute #\space #\, (read-line stream nil nil))
          while line
          for row = (read-from-string (concatenate 'string "(" line ")"))
          do (setf (gethash (nth 0 row) structs)
                   (make-program :name (nth 0 row)
                                 :weight (car (nth 1 row))
                                 :aboves (nthcdr 3 row)))
          finally (return structs))))

(defparameter *programs* (read-input "/home/alan/Desktop/input.txt"))

(defun hash-keys (hash-table)
  (loop for k being the hash-keys of hash-table collect k))

(defun part-1 ()
  (let ((children (make-hash-table))
        (roots (make-hash-table)))
    (maphash (lambda (below program)
               (loop for above in (program-aboves program)
                     do (progn (remhash above roots)
                               (setf (gethash above children) t)
                               (when (not (gethash below children))
                                 (setf (gethash below roots) below)))))
             *programs*)
    (assert (= 1 (hash-table-count roots)))
    (car (hash-keys roots))))

(defun total-weight (program)
  (+ (program-weight program)
     (loop for above-name in (program-aboves program)
           sum (total-weight (gethash above-name *programs*)))))

(defun above (program)
  (mapcar (lambda (name) (gethash name *programs*))
          (program-aboves program)))

(defun next-unbalanced (program)
  (let* ((aboves (above program))
         (weights (mapcar #'total-weight aboves)))
    (when (not (apply #'= weights))
      (elt aboves (position (apply #'max weights) weights)))))

(defun deepest-unbalanced (program)
  (let ((next (next-unbalanced program)))
    (if next
        (deepest-unbalanced next)
        program)))

(defun part-2 ()
  (let* ((root (gethash (part-1) *programs*))
         (weights (mapcar #'total-weight (above root)))
         (discrepancy (abs (apply #'- (remove-duplicates weights)))))
    (- (program-weight (deepest-unbalanced root)) discrepancy)))

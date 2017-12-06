(defun biggest (banks)
  (loop with biggest = (elt banks 0)
        with biggest-idx = 0
        for idx from 1 to (1- (length banks))
        when (> (elt banks idx) biggest)
          do (setq biggest (elt banks idx)
                   biggest-idx idx)
        finally (return biggest-idx)))

(defun next-idx (banks idx)
  (mod (1+ idx) (length banks)))

(defun redistribute1 (banks)
  (loop with biggest-idx = (biggest banks)
        with amount = (elt banks biggest-idx)
        with new-banks = (copy-seq banks)
          initially (setf (elt new-banks biggest-idx) 0)
        with idx = (next-idx new-banks biggest-idx)
        while (not (zerop amount))
        do (progn (incf (elt new-banks idx))
                  (decf amount)
                  (setf idx (next-idx new-banks idx)))
        finally (return new-banks)))

(defun redistribute (banks)
  (loop with seen = (make-hash-table :test #'equalp)
        with i = 0
        for b = banks then (redistribute1 b)
        while (not (gethash b seen))
        do (progn (incf i)
                  (setf (gethash b seen) t))
        finally (return (values i b))))

(setq input #(4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5))
;; first part:
(nth-value 0 (redistribute input))
;; second part:
(multiple-value-bind (n banks) (redistribute input)
    (nth-value 0 (redistribute banks)))


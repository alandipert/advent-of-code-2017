;; Incorporates some ideas from https://gist.github.com/death/02e716211827683c48a9c580348fd5a5

(defun biggest (banks)
  (position (reduce #'max banks) banks))

(defun next-idx (banks idx)
  (mod (1+ idx) (length banks)))

(defun redistribute1 (banks)
  (loop with biggest-idx = (biggest banks)
        with amount = (aref banks biggest-idx)
        with new-banks = (copy-seq banks)
          initially (setf (aref new-banks biggest-idx) 0)
        with idx = (next-idx new-banks biggest-idx)
        until (zerop amount)
        do (progn (incf (aref new-banks idx))
                  (decf amount)
                  (setf idx (next-idx new-banks idx)))
        finally (return new-banks)))

(defun redistribute (banks)
  (loop with seen = (make-hash-table :test #'equalp)
        with i = 0
        for b = banks then (redistribute1 b)
        until (gethash b seen)
        do (progn (incf i)
                  (setf (gethash b seen) t))
        finally (return (values i b))))

(setq input #(4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5))
;; first part:
(nth-value 0 (redistribute input))
;; second part:
(multiple-value-bind (n banks) (redistribute input)
    (nth-value 0 (redistribute banks)))


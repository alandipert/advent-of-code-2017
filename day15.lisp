(defconstant +divisor+ 2147483647)
(defconstant +factor-a+ 16807)
(defconstant +factor-b+ 48271)

(defun next (factor prev)
  (mod (* factor prev) +divisor+))

(defun judge= (a b)
  (= (boole boole-andc1 a #xFFFF)
     (boole boole-andc1 b #xFFFF)))

(defun part-1 (start-a start-b iters)
  (do ((a (next +factor-a+ start-a) (next +factor-a+ a))
       (b (next +factor-b+ start-b) (next +factor-b+ b))
       (i 0 (1+ i))
       (total 0 (if (judge= a b) (1+ total) total)))
      ((>= i iters) total)))

(defconstant +multiple-a+ 4)
(defconstant +multiple-b+ 8)

(defun next2 (factor multiple prev)
  (do ((succ (next factor prev) (next factor succ)))
      ((zerop (mod succ multiple)) succ)))

(defun part-2 (start-a start-b iters)
  (do ((a (next2 +factor-a+ +multiple-a+ start-a)
          (next2 +factor-a+ +multiple-a+ a))
       (b (next2 +factor-b+ +multiple-b+ start-b)
          (next2 +factor-b+ +multiple-b+ b))
       (i 0 (1+ i))
       (total 0 (if (judge= a b) (1+ total) total)))
      ((>= i iters) total)))



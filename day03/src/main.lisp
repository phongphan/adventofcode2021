(defpackage day03.main
  (:use :cl)
  (:export :run :run-2))

(in-package :day03.main)

(defun read-input (filename)
  (mapcar 'parse (uiop:read-file-lines filename)))

(defun parse (line)
  (map 'list 'digit-char-p line))

(defun transpose (input)
  (loop for i from 0 to (- (length (car input)) 1)
        collect (mapcar #'(lambda (row) (nth i row)) input)))

(defun calculate-gamma (transposed-input)
  (let* ((threshold (/ (length (car transposed-input)) 2))
         (most-common-bit (lambda (row)
                            (if (> (reduce #'+ row) threshold) #\1 #\0))))
    (map 'string most-common-bit transposed-input)))

(defun calculate-epsilon-from-gamma (bit-string)
  (map 'string #'(lambda (c) (if (eq c #\0) #\1 #\0)) bit-string))

(defun run ()
  (let* ((input (transpose (read-input "input/input.txt")))
         (gamma-bs (calculate-gamma input))
         (epsilon-bs (calculate-epsilon-from-gamma gamma-bs)))
    (* (parse-integer gamma-bs :radix 2) (parse-integer epsilon-bs :radix 2))))

(defun row-to-int (row)
  (parse-integer
   (map 'string #'(lambda (n) (if (= n 1) #\1 #\0)) row)
   :radix 2))

(defun calculate-o2 (input)
  (labels ((rec (input bit-no)
             (if (> (length input) 1)
                 (let* ((threshold (/ (length input) 2))
                        (bitval (if (>= (reduce #'+ (nth bit-no (transpose input))) threshold) 1 0)))
                   (rec
                    (remove-if-not #'(lambda (row) (= bitval (nth bit-no row))) input)
                    (1+ bit-no)))
                 (car input))))
    (rec input 0)))

(defun calculate-co2 (input)
  (labels ((rec (input bit-no)
             (if (> (length input) 1)
                 (let* ((threshold (/ (length input) 2))
                        (bitval (if (>= (reduce #'+ (nth bit-no (transpose input))) threshold) 0 1)))
                   (rec
                    (remove-if-not #'(lambda (row) (= bitval (nth bit-no row))) input)
                    (1+ bit-no)))
                 (car input))))
    (rec input 0)))

(defun run-2 ()
  (let* ((input (read-input "input/input.txt"))
         (o2 (calculate-o2 input))
         (co2 (calculate-co2 input)))
    (* (row-to-int o2) (row-to-int co2))))

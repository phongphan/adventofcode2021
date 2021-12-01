(defpackage day01.main
  (:use :cl)
  (:export :run-1
           :run-2))

(in-package :day01.main)

(defun read-input (filename)
  (let ((input (mapcar 'parse-integer (uiop:read-file-lines filename))))
    (coerce input 'vector)))

(defun sum (input)
  (reduce '+ input))

(defun make-groups (input size)
  (loop for i from 0 to (- (length input) size)
        collect (subseq input i (+ i size))))

(defun calc-depth-increase (input)
  (loop for (a b) in input
        counting (> (sum b) (sum a))))

(defun run-1 ()
  (let* ((input (read-input "input/1.txt"))
         (pairs (make-groups (make-groups input 1) 2)))
    (calc-depth-increase pairs)))


(defun run-2 ()
  (let* ((input (read-input "input/1.txt"))
         (pairs (make-groups (make-groups input 3) 2)))
    (calc-depth-increase pairs)))

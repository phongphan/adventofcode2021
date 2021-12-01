(defpackage day01.main
  (:use :cl)
  (:import-from :arrow-macros :->)
  (:export :run-1a
           :run-2a
           :run-1b
           :run-2b
           :run-1c
           :run-2c))

(in-package :day01.main)

(defun read-input (filename)
  (mapcar 'parse-integer (uiop:read-file-lines filename)))

(defun sum (input)
  (reduce '+ input))

(defun make-groups (input size)
  (loop for i from 0 to (- (length input) size)
        collect (subseq input i (+ i size))))

(defun calc-depth-increase (input)
  (loop for (a b) in input
        counting (> (sum b) (sum a))))

(defun run-1a ()
  (let* ((input (read-input "input/1.txt"))
         (pairs (make-groups (make-groups input 1) 2)))
    (calc-depth-increase pairs)))

(defun run-2a ()
  (let* ((input (read-input "input/1.txt"))
         (pairs (make-groups (make-groups input 3) 2)))
    (calc-depth-increase pairs)))

(defun run-1b ()
  (-> (read-input "input/1.txt")
    (coerce 'vector)
    (make-groups 1)
    (make-groups 2)
    (calc-depth-increase)))

(defun run-2b ()
  (-> (read-input "input/1.txt")
    (coerce 'vector)
    (make-groups 3)
    (make-groups 2)
    (calc-depth-increase)))

;; competition mode
(defun run-1c ()
  (loop for (a b) on (read-input "input/1.txt")
        while (and a b)
        count (> b a)))

(defun run-1c ()
  (loop for (a b) on (loop for (a b c) on (read-input "input/1.txt")
                           while (and a b c)
                           collect (+ a b c))
        while (and a b)
        count (> b a)))

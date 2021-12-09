(defpackage day07.main
  (:use :cl)
  (:export :run :run-2))

(in-package :day07.main)

(defun read-input (filename)
  (apply 'concatenate 'list (mapcar 'parse (uiop:read-file-lines filename))))

(defun parse (line)
  (mapcar 'parse-integer (uiop:split-string line :separator ",")))

(defun sum-1-to (to-n)
  (* to-n (/ (+ 1 to-n) 2)))

(defun calculate-fuel (input target-position)
  (apply '+ (mapcar #'(lambda (x) (abs (- x target-position))) input)))

(defun calculate-fuel-1 (input target-position)
  (apply '+ (mapcar #'(lambda (x) (sum-1-to (abs (- x target-position)))) input)))

(defun calculate-fuel-positions (input calculator-fn)
  (loop for target-pos from 0 to (apply 'max input)
        collect (cons target-pos (apply calculator-fn (list input target-pos)))))

(defun find-min (position-fuels)
  (reduce #'(lambda (x y) (if (< (cdr x) (cdr y)) x y)) position-fuels))

(defun run-ex ()
  (find-min (calculate-fuel-positions (read-input "input/ex.txt") 'calculate-fuel)))

(defun run-ex-2 ()
  (find-min (calculate-fuel-positions (read-input "input/ex.txt") 'calculate-fuel-1)))

(defun run ()
  (find-min (calculate-fuel-positions (read-input "input/input.txt") 'calculate-fuel)))

(defun run-2 ()
  (find-min (calculate-fuel-positions (read-input "input/input.txt") 'calculate-fuel-1)))

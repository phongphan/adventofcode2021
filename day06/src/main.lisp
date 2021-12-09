(defpackage day06.main
  (:use :cl)
  (:export :run :run-2))

(in-package :day06.main)

(defun read-input (filename)
  (let* ((fish (make-list 9 :initial-element 0))
         (input (apply 'concatenate 'list (mapcar 'parse (uiop:read-file-lines filename)))))
    (loop for i in input
          do (setf (nth i fish) (1+ (nth i fish))))
    fish))

(defun parse (line)
  (mapcar 'parse-integer (uiop:split-string line :separator ",")))

(defun do-next (input)
  (let* ((zeroed (car input))
         (new-input (copy-list (nconc (cdr input) (list 0)))))
    (progn
      (setf (nth 6 new-input) (+ (nth 6 new-input) zeroed))
      (setf (nth 8 new-input) zeroed))
    new-input))

(defun next (input &key (days 1))
  (labels ((rec (input days)
             (if (> days 0)
                 (rec (do-next-1 input) (1- days))
                 input)))
    (rec input days)))

(defun run ()
  (reduce #'+ (next (read-input "input/input.txt") :days 80)))

(defun run-2 ()
  (reduce #'+ (next (read-input "input/input.txt") :days 256)))

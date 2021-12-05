(defpackage day04.main
  (:use :cl)
  (:import-from :arrow-macros :->)
  (:export :run :run-2))

(in-package :day04.main)

(defun read-input (filename)
  (uiop:read-file-lines filename))

(defun read-bingo-numbers (input)
  (-> (car input)
    (uiop:split-string :separator ",")
    (lambda (xs) (map 'list 'parse-integer xs))))

(defun tr (line)
  (mapcar #'parse-integer (cl-ppcre:split "\\s+" (string-trim " " line))))

(defun read-boards (input)
  (let* ((ll (remove-if #'(lambda (n) (string-equal "" n)) (cdr input)))
         (lines (mapcar #'tr ll)))
    (loop for i from 0 below (/ (length lines) 5)
          for start integer = (* i 5)
          for end integer = (+ start 5)
          collect (make-array '(5 5) :initial-contents (subseq lines start end)))))

(defun punch-hole (boards n)
  (loop for board in boards
        do (loop for x from 0 to 4
                 append (loop for y from 0 to 4
                              when (eq (aref board x y) n)
                                do (setf (aref board x y) NIL)))))

(defun h-bingo (board)
  (loop for x from 0 to 4
        collect (loop for y from 0 to 4
                      count (eq NIL (aref board x y)))))

(defun v-bingo (board)
  (loop for y from 0 to 4
        collect (loop for x from 0 to 4
                      count (eq NIL (aref board x y)))))

(defun bingo? (board)
  (or
   (find-if #'(lambda (x) (= x 5)) (h-bingo board))
   (find-if #'(lambda (x) (= x 5)) (v-bingo board))))

(defun find-bingo-board (boards)
  (remove-if-not 'bingo? boards))

(defun find-non-bingo-boards (boards)
  (remove-if 'bingo? boards))

(defun sum-bingo-board (board)
  (loop for i below (array-dimension board 0)
        sum (loop for j below (array-dimension board 1)
                  when (aref board i j)
                    sum (aref board i j))))

(defun run ()
  (let* ((input (read-input "input/input.txt"))
         (numbers (read-bingo-numbers input))
         (boards (read-boards input)))

    (labels ((rec (numbers)
               (progn
                 (punch-hole boards (car numbers))
                 (let ((bingo (car (find-bingo-board boards))))
                   (if bingo
                       (list (car numbers) bingo (* (car numbers) (sum-bingo-board bingo)))
                       (rec (cdr numbers)))))))
      (rec numbers))))

(defun run-2 ()
  (let* ((input (read-input "input/input.txt"))
         (numbers (read-bingo-numbers input))
         (boards (read-boards input)))

    (labels ((rec (numbers boards)
               (progn
                 (punch-hole boards (car numbers))
                 (let ((bingo (car (find-bingo-board boards))))
                   (if (and bingo (= 1 (length boards)))
                       (list (car numbers) bingo (* (car numbers) (sum-bingo-board bingo)))
                       (rec (cdr numbers) (find-non-bingo-boards boards)))))))
      (rec numbers boards))))

(defpackage day10.main
  (:use :cl)
  (:export :run :run-2))

(in-package :day10.main)

(defun read-input (filename)
  (mapcar 'parse (uiop:read-file-lines filename)))

(defun parse (line)
  (coerce line 'list))

(defparameter *open* '(#\( #\[ #\{ #\<))

(defun find-close (c)
  (cond
    ((eq #\( c) #\))
    ((eq #\[ c) #\])
    ((eq #\{ c) #\})
    ((eq #\< c) #\>)))

(defun matched (a b)
  (or
   (and (eq #\) a) (eq #\( b))
   (and (eq #\] a) (eq #\[ b))
   (and (eq #\} a) (eq #\{ b))
   (and (eq #\> a) (eq #\< b))))

(defun match-parens (input)
  (labels ((rec (input aux)
             (if (null input)
                 (mapcar 'find-close aux)
                 (let ((n (car input))
                       (p (car aux)))
                   (cond ((member n *open*)
                          (rec (cdr input) (cons n aux)))
                         ((matched n p)
                          (rec (cdr input) (cdr aux)))
                         (t n))))))
    (rec input '())))

(defun convert-corrupted-point (input)
  (cond ((null input) 0)
        ((listp input) 0)
        ((eq #\) input) 3)
        ((eq #\] input) 57)
        ((eq #\} input) 1197)
        ((eq #\> input) 25137)
        (t (error (format t "Unknown token ~a" input)))))

(defun convert-autocompleted-point (input)
  (cond ((eq #\) input) 1)
        ((eq #\] input) 2)
        ((eq #\} input) 3)
        ((eq #\> input) 4)
        (t (error (format t "Unknown token ~a" input)))))

(defun calculate-autocomplete-score (input)
  (reduce #'(lambda (a b)
              (+ (* a 5) b))
          (cons 0 input)))

(defun run ()
  (let ((input (read-input "input/input.txt")))
    (reduce #'+
            (mapcar 'convert-corrupted-point
                    (mapcar 'match-parens input)))))

(defun run-2 ()
  (let* ((input (read-input "input/input.txt"))
         (incompletes (remove-if-not 'listp (mapcar 'match-parens input)))
         (scores (sort
                  (loop for xs in incompletes
                        collect (calculate-autocomplete-score
                                 (mapcar 'convert-autocompleted-point xs)))
                  #'>)))
    (nth (floor (/ (length scores) 2)) scores)))

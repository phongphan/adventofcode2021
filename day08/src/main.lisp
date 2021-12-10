(defpackage day08.main
  (:use :cl)
  (:export :run :run-2))

(in-package :day08.main)

(defun read-input (filename)
  (mapcar 'parse (uiop:read-file-lines filename)))

(defun parse (line)
  (destructuring-bind (a b) (cl-ppcre:split "\\s*\\|\\s*" line)
    (cons (cl-ppcre:split "\\s+" a) (cl-ppcre:split "\\s+" b))))

(defun read-result (input)
  (apply 'concatenate 'list (loop for s in input
                                  collect (cdr s))))

(defparameter *keys*
    '(#\a #\b #\c #\d #\e #\f #\g))

(defparameter *segments*
  '("abcefg"  ; 0
    "cf"      ; 1
    "acdeg"   ; 2
    "acdfg"   ; 3
    "bcdf"    ; 4
    "abdfg"   ; 5
    "abdefg"  ; 6
    "acf"     ; 7
    "abcdefg" ; 8
    "abcdfg"  ; 9
    ))

(defparameter *n-segments*
  (loop for xs in (mapcar #'(lambda (s) (coerce s 'list)) *segments*)
        collect (mapcar #'(lambda (c)
                            (- (char-code c) (char-code #\a))) xs)))

(defun create-segment (combinator)
  (mapcar #'(lambda (n-segment)
              (coerce
               (mapcar #'(lambda (i) (nth i combinator)) n-segment)
               'string))
          *n-segments*))

(defun permutations (bag)
  (if (null bag)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p)
                              (cons e p))
                          (permutations
                           (remove e bag :count 1 :test #'equal))))
              bag)))

(defparameter *segment-permutations* (mapcar #'create-segment (permutations *keys*)))

(defun find-observe (observe)
  (find observe
      *segment-permutations*
      :test #'(lambda (a b)
                (null (set-exclusive-or a b
                                        :test #'(lambda (a b)
                                                  (null
                                                   (set-exclusive-or (coerce a 'list) (coerce b 'list)
                                                                     :test #'eq))))))))

(defun convert-digits (segments result)
  (position result segments :test #'(lambda (a b)
                                     (null
                                      (set-exclusive-or (coerce a 'list) (coerce b 'list)
                                                        :test #'eq)))))

(defun resolve-digits (input)
  (let* ((observe (car input))
         (result (cdr input)))
    (mapcar #'(lambda (x)
                (convert-digits (find-observe observe) x))
                result)))

(defun run ()
  (let* ((input (read-input "input/input.txt"))
         (result (read-result input)))
    (loop for r in result
          count (member (length r) '(2 3 4 7)))))

(defun run-2 ()
  (let* ((input (read-input "input/input.txt"))
         (digits (mapcar #'(lambda (x) (resolve-digits x)) input)))
    (reduce #'+
            (mapcar #'(lambda (x)
                        (parse-integer (apply 'concatenate 'string (mapcar 'write-to-string x))))
                    digits))))

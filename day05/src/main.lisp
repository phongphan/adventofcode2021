(defpackage day05.main
  (:use :cl)
  (:import-from :arrow-macros :->)
  (:export :run :run-2))

(in-package :day05.main)

(defun read-input (filename)
  (mapcar 'parse (uiop:read-file-lines filename)))

(defun parse (line)
  (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "(\\d+)\\,(\\d+)\\s+->\\s+(\\d+)\\,(\\d+)" line)
    (destructuring-bind (x0 y0 x1 y1) (map 'list 'parse-integer b)
      (list (cons x0 y0) (cons x1 y1)))))

(defun horizontal-line? (from to)
  (= (cdr from) (cdr to)))

(defun vertical-line? (from to)
  (= (car from) (car to)))

(defun points-as-line (from to &key diagonal?)
  (cond
    ((horizontal-line? from to) (let* ((y (cdr to))
                                       (start (min (car from) (car to)))
                                       (end (max (car from) (car to))))
                                  (loop for x from start to end collect (cons x y))))
    ((vertical-line? from to) (let* ((x (car from))
                                     (start (min (cdr from) (cdr to)))
                                     (end (max (cdr from) (cdr to))))
                                (loop for y from start to end collect (cons x y))))
    (diagonal? (find-points-diagonal-45 from to))
    (t '())))

(defun all-points (input &key diagonal?)
  (apply 'concatenate 'list
         (remove-if-not 'identity
                        (mapcar #'(lambda (x) (points-as-line (car x) (cadr x) :diagonal? diagonal?)) input))))

(defun fill-table (&key x-size y-size points)
  (let ((table (make-array (list x-size y-size))))
    (loop for (x . y) in points
          do (setf (aref table y x) (1+ (aref table y x))))
    table))

(defun count-overlap (table)
  (loop for y from 0 below (array-dimension table 0)
        sum (loop for x from 0 below (array-dimension table 1)
                  count (> (aref table y x) 1))))

(defun distance (p0 p1)
  (sqrt
   (+
    (expt (abs (- (car p0) (car p1))) 2)
    (expt (abs (- (cdr p0) (cdr p1))) 2))))

(defun range (from-n to-n)
  (if (< from-n to-n)
      (loop for i from from-n to to-n collect i)
      (loop for i from from-n downto to-n collect i)))

(defun find-points-diagonal-45 (p0 p1)
  (when (=
       (abs (- (car p0) (car p1)))
       (abs (- (cdr p0) (cdr p1))))
    (loop for x in (range (car p0) (car p1))
          for y in (range (cdr p0) (cdr p1))
          collect (cons x y))))

(defun run ()
  (let* ((input (read-input "input/input.txt"))
         (points (all-points input))
         (max-y (apply 'max (mapcar 'car points)))
         (max-x (apply 'max (mapcar 'cdr points))))
    (-> (fill-table :x-size (1+ max-x) :y-size (1+ max-y) :points points)
      (count-overlap))))

(defun run-2 ()
  (let* ((input (read-input "input/input.txt"))
         (points (all-points input :diagonal? t))
         (max-y (apply 'max (mapcar 'car points)))
         (max-x (apply 'max (mapcar 'cdr points))))
    (-> (fill-table :x-size (1+ max-x) :y-size (1+ max-y) :points points)
      (count-overlap))))

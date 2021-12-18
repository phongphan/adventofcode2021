(defpackage day09.main
  (:use :cl)
  (:export :run :run-2))

(in-package :day09.main)

(defun read-input (filename)
  (mapcar 'parse (uiop:read-file-lines filename)))

(defun parse (line)
  (map 'list 'digit-char-p (coerce line 'list)))

(defun create-table (input)
  (let ((w (length (car input)))
        (h (length input)))
    (make-array (list h w) :initial-contents input)))

(defun find-adjacents (x y table)
  (let ((w (array-dimension table 1))
        (h (array-dimension table 0))
        (points `((,(1- x) ,y)
                  (,(1+ x) ,y)
                  (,x ,(1- y))
                  (,x ,(1+ y)))))
    (loop for (x y) in points
          when (and (>= x 0) (>= y 0) (< x w) (< y h))
            append `((,x ,y)))))

(defun is-smallest (x y table)
  (let* ((points (find-adjacents x y table))
         (values (mapcar #'(lambda (pp) (aref table (cadr pp) (car pp))) points)))
    (null (remove-if #'(lambda (v) (> v (aref table y x))) values))))

(defun find-smallest-points (table)
  (let ((w (array-dimension table 1))
        (h (array-dimension table 0)))
    (loop for y from 0 below h
          append (loop for x from 0 below w
                       when (is-smallest x y table)
                         collect (list x y)))))

(defun find-upwards (x y table)
  (let ((val (aref table y x)))
    (let* ((adjacents (find-adjacents x y table))
           (upwards (remove-if-not #'(lambda (pts)
                                       (let ((nval (aref table (cadr pts) (car pts))))
                                         (and (< val nval) (/= 9 nval))))
                                   adjacents)))
      (if (null upwards)
          '(())
          (mapcan #'(lambda (pts)
                      (mapcar #'(lambda (p)
                                  (cons pts p))
                              (find-upwards (car pts) (cadr pts) table)))
                  upwards)))))

(defun find-basin-from-point (x y table)
  (cons (list x y)
        (remove-duplicates (apply 'concatenate 'list (find-upwards x y table)) :test 'equal)))

(defun run ()
  (let* ((input (read-input "input/input.txt"))
         (table (create-table input)))
    (reduce #'+
            (mapcar #'(lambda (points)
                        (1+ (aref table (cadr points) (car points))))
                    (find-smallest-points table)))))

;(find-smallest-points *table*)
;(length (find-basin-from-point 2 2 *table*))

(defun run-2 ()
  (let* ((input (read-input "input/input.txt"))
         (table (create-table input))
         (lowpoints (find-smallest-points table))
         (basins (mapcar #'(lambda (pts)
                             (find-basin-from-point (car pts) (cadr pts) table))
                         lowpoints)))
    (reduce #'* (subseq (sort (mapcar #'length basins) #'>) 0 3))))

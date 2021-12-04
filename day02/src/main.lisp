(defpackage day02.main
  (:use :cl)
  (:import-from :arrow-macros :->>)
  (:export :run :run-2))

(in-package :day02.main)

(defstruct submarine
  (x-position 0)
  (y-position 0)
  (aim 0))

(defvar *submarine*)

(defun read-input (filename)
  (mapcar 'parse-command (uiop:read-file-lines filename)))

(defun parse-command (command)
  (let ((params (uiop:split-string command)))
    (cons (intern (car params) :keyword) (parse-integer (cadr params)))))

(defun run-command (command)
  (let* ((cmd (car command))
         (value (cdr command)))
    (case cmd
      (:|forward| (setf (submarine-x-position *submarine*) (+ value (submarine-x-position *submarine*))))
      (:|down| (setf (submarine-y-position *submarine*) (+ value (submarine-y-position *submarine*))))
      (:|up| (setf (submarine-y-position *submarine*) (- (submarine-y-position *submarine*) value)))
      (t (error (format nil "Unknown clauses ~a" cmd))))))

(defun run ()
  (let ((*submarine* (make-submarine)))
    (let ((commands (read-input "input/input.txt")))
      (loop for command in commands
            do (run-command command))
      (* (submarine-x-position *submarine*) (submarine-y-position *submarine*)))))

(defun run-command-2 (command)
  (let* ((cmd (car command))
         (value (cdr command)))
    (case cmd
      (:|down| (setf (submarine-aim *submarine*) (+ value (submarine-aim *submarine*))))
      (:|up| (setf (submarine-aim *submarine*) (- (submarine-aim *submarine*) value)))
      (:|forward|
       (progn
         (setf (submarine-x-position *submarine*) (+ value (submarine-x-position *submarine*)))
         (setf (submarine-y-position *submarine*)
               (+
                (* value (submarine-aim *submarine*))
                (submarine-y-position *submarine*)))))
      (t (error (format nil "Unknown clauses ~a" cmd))))))

(defun run-2 ()
  (let ((*submarine* (make-submarine)))
    (let ((commands (read-input "input/input.txt")))
      (loop for command in commands
            do (run-command-2 command))
      (* (submarine-x-position *submarine*) (submarine-y-position *submarine*)))))

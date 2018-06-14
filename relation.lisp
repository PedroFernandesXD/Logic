#!/usr/local/bin/sbcl --script

#|
(defpackage :relation
  (:use :cl)
  (:export :main))

(in-package :relation)
|#
(let ((dic '((0 . (1 1))  (1 . (2 2))  (2 . (3 3))  (3 . (4 4))
		    (4 . (1 2))  (5 . (2 1))  (6 . (3 4))  (7 . (4 1))		     
		    (8 . (1 3))  (9 . (2 3))  (10 . (3 1)) (11 . (1 4))
		    (12 . (4 3)) (13 . (3 2)) (14 . (2 4)) (15 . (4 2)))))
(defun digits(n)
  (declare (optimize speed))
  (multiple-value-bind (q r) (floor n 2)
    (if (and (zerop q) (zerop r))
	nil (cons r (digits q)))))

(defun normalize(xs)
  (declare (optimize speed))
  (if (= (length xs) 16)
      xs
      (normalize (cons 0 xs))))

(defun 2bit(n)
  (declare (optimize speed)
	   (integer n))
  (reverse (coerce (normalize (reverse (digits n))) 'bit-vector)))

(defun ref(n)
  (declare (optimize speed))
  (loop with bv = (2bit n)
     for x from 0 to 3 do
       (unless (zerop (bit bv x))
	 (return t))))

(defun inref(n)
  (declare (optimize speed))
  (not (ref n)))

(defun all-aux(n &optional (r nil))
  (loop with bv = (2bit n)
     for x from 0 to 15 do
       (unless (zerop (bit bv x))
	 (push x r))) r)

(defun bus-A(n &optional (r nil))
  (declare (optimize speed))
  (loop for x from 0 to 15 do
       (when (= (cadr (assoc x dic)) n)
	 (push x r))) r)
(defun bus-B(n &optional (r nil))
  (declare (optimize speed))
  (loop for x from 0 to 15 do
       (when (= (caddr (assoc x dic)) n)
	 (push x r))) r)

(defun sim(n &optional (r nil))
  (declare (optimize speed))
  (labels ((apar (x y) (when (and (= (cadr (assoc x dic)) (caddr (assoc y dic)))
				  (= (caddr (assoc x dic)) (cadr (assoc y dic)))) t)))
    (if (<= n 15) t
	(loop with bv = (2bit n)
	   for x from 4 to 14 do
	     (loop for i from 15 downto (+ x 1) do
		  (unless  (or (zerop (bit bv x))
			       (zerop (bit bv i)))
		    (when (apar x i) (setf r t))) )
	     (return r))) ))


(defun tra(n &optional (r nil))
  (declare (optimize speed))
  (labels ((apar (x y &optional (rx (cdr (assoc x dic))) (ry (cdr (assoc y dic))) (r nil))
	     (when (= (cadr rx) (car ry))
	       (loop for i in (bus-A (car rx))
		     for j in (bus-B (cadr ry)) do
		    (when (= i j)
		      (setf r i)) ))
	     r))
    (if (<= n 15) t
	(loop with vec = (2bit n)
	   for x from 4 to 14 do
	     (loop for i from 15 downto (+ x 1) do
		  (when (or (zerop (bit vec x))
			    (zerop (bit vec i)))
		    (let ((n (apar x i)))
		      (when (and (numberp n) (zerop (bit vec n)))
			(setf r t))) ))
	     (return r))) ))

(defun fuc(n &optional (r nil))
  (declare (optimize speed))
  (loop with vec = (2bit n)
     for x from 0 to 15 do
       (unless (zerop (bit vec x))
	 (push (cadr (assoc x dic)) r)))
  (if (= (length r) 4)
      (loop for x from 1 to 4 do
	   (if (> (length (member x r)) 1)
	       (return nil)
	       (return t)))
      nil))
      
(defun sbi(n &optional (r nil))
  (declare (optimize speed))
  (when (fuc n)
      (loop with vec = (2bit n)
	 for x from 0 to 15 do
	   (unless (zerop (bit vec x))
	     (push (caddr (assoc x dic)) r)))
      (loop for x from 1 to 4 do
	   (when (= (length (member x r)) 1)
	     (return t)))))

(defun relation(n &optional (s t))
  (declare (number n)
	   (optimize speed))
  (if (and (ref n) (sim n) (tra n))
      (format s "RSTE")
      (progn
	(if (ref n)
	    (format s "R")
	    (format s "I"))
	(when (sim n)
	  (format s "S"))
	(when (tra n)
	  (format s "T"))))
  (when (sbi n)
    (format s "FFbFsFi"))
  (when (fuc n)
    (format s "F")))
  
(defun frelation(pth)
  (declare (optimize speed))
  (with-open-file
      (W pth :direction :output
	 :if-exists :supersede)
    (format W "{}STI")
    (loop for x from 1 to 65535 do
	 (format W "~%~{(~{~a~#[~;,~:;~]~})~}" (loop for i in (all-aux x) collect (cdr (assoc i dic))))
	 ;(format W "~%~a " x)
	 (relation x W))))
	 
(defun main()
  (declare (optimize speed))
  (frelation #P"relation.txt"))

(main))

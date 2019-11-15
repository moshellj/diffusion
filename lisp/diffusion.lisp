#!/usr/bin/sbcl --script

;;;; CONSTANTS
(defconstant Cdivs 5)
(defconstant Cdmax (- Cdivs 1))
(defconstant Lcube 5.0d0)
(defconstant Urms 250.0d0)
(defconstant D 0.175d0)
(defconstant Cdist (/ Lcube Cdivs))
(defconstant Tstep (/ Cdist Urms))
(defconstant Dterm (* D (/ Tstep Cdist Cdist)))

;;;; FUNCTIONS
(defun minMaxRatio (cubep) 
	(let ((maxval 0.0d0) (minval 1d100))
	(dotimes (x Cdmax) (dotimes (y Cdmax) (dotimes (z Cdmax)
		( setf maxval (max maxval (aref cubep x y z) ) )
		( setf minval (min minval (aref cubep x y z) ) )
	)))
	(/ minval maxval))
)

;;;; MAIN

(defvar cube (make-array (list Cdivs Cdivs Cdivs) :element-type 'double-float :initial-element 0.0d0))
(setf (aref cube 0 0 0) 1d21)
(write (minMaxRatio cube))

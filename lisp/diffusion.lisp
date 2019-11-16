#!/usr/bin/sbcl --script

;At Cdivs = 10, equilibriated in 61.726 seconds simulated, 1m10.791s real time.
; (this is different from all my other programs, but alas, it can't be helped.)

;;;; CONSTANTS
(defconstant Cdivs 10)
(defconstant Cdmax (- Cdivs 1))
(defconstant Lcube 5.0d0)
(defconstant Urms 250.0d0)
(defconstant D 0.175d0)
(defconstant Cdist (/ Lcube Cdivs))
(defconstant Tstep (/ Cdist Urms))
(defconstant Dterm (* D (/ Tstep Cdist Cdist)))

(defconstant offsets (list	
(list -1 0 0) (list 1 0 0) (list 0 -1 0) (list 0 1 0) (list 0 0 -1) (list 0 0 1)))

;;;; FUNCTIONS

(defun inbounds (x y z)
	(not (or
		(< x 0) (> x Cdmax)
		(< y 0) (> y Cdmax)
		(< z 0) (> z Cdmax)
	))
)

(defun minMaxRatio (cubep) 
	(let ((maxval 0.0d0) (minval 1d100))
	;;; NEAT LANGUAGE THING: dotimes is a compact for loop, and i love those.
	(dotimes (x Cdmax) (dotimes (y Cdmax) (dotimes (z Cdmax)
		( setf maxval (max maxval (aref cubep x y z) ) )
		( setf minval (min minval (aref cubep x y z) ) )
	)))
	(/ minval maxval))
)

; here, off, nbr are xyz lists
(defun diffuse (cube here off)
	(let	((nbr (list		(+ (elt here 0) (elt off 0))
							(+ (elt here 1) (elt off 1))
							(+ (elt here 2) (elt off 2)))
			) (change))
	(when (and (apply #'inbounds here) (apply #'inbounds nbr)) (progn 
		;;; NEAT LANGUAGE THING: APPLY lets us use a vector to index an array
		(setf change (* Dterm (- (apply #'aref cube here) (apply #'aref cube nbr) )))
		(setf (apply #'aref cube here) (- (apply #'aref cube here) change))
		(setf (apply #'aref cube nbr)  (+ (apply #'aref cube nbr)  change))
	))
))

;;;; MAIN

(defvar cube (make-array (list Cdivs Cdivs Cdivs) :element-type 'double-float :initial-element 0.0d0))

(setf (aref cube 0 0 0) 1d21)
(defvar Ttotal 0.0d0)
(defvar conratio 0.0d0)

(loop
	(dotimes (x Cdmax)
		(dotimes (y Cdmax)
			(dotimes (z Cdmax)
				; I had no idea smart for loops were this old
				(loop for off in offsets do
					(diffuse cube (list x y z) off)
				)
			)
		)
	)
	(setf Ttotal (+ Ttotal Tstep))
	(setf conratio (minMaxRatio cube))
	;(write Ttotal) (terpri)
	;(write conratio) (terpri) (terpri)
	(when (> conratio 0.99d0) (return t))
)
(write Ttotal) (terpri)
(write conratio) (terpri) (terpri)

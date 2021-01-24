;;;-*-EMACS-LISP-*-

;;;
;;; Routines to calculate 
;;; - Maidenhead Grid Square (QRA, IARU Grid Square) from latitude and longitude
;;; - Latitude and longitude from QRA
;;; - Great Circle distance from grid square to grid square
;;; - Bearing from Grid Square to Grid Square
;;; 
;;; Leigh L. Klotz, Jr. WA5ZNU <klotz@graflex.org>
;;; Assembled from various places -- see comments.
;;;


;;; Examples
;;; (qra 37.428833 -122.114667) 
;;; This gives CM87wk which is correct
; (latlon "CM87wk")
; Gives 37.4375 -122.125
;;
; (latlon "CM87wk")

; (haversine-great-circle-distance-miles (latlon "CM87wk") (latlon "QN16ix")) ; 4605 miles
; (haversine-great-circle-distance-miles (latlon "CM87wk") (latlon "JN62vu")) ; 6232 miles

(defun qra-distance (x  y)
  (interactive "sGrid Square: 
sGrid Square: ")
  (let ((m (haversine-great-circle-distance-miles (latlon x) (latlon y))))
    (if (< m 10)
	(message "%f miles" (/ (round (* m 10.0)) 10.0))
      (message "%d miles" (round m)))))

(defun qra-bearing (x  y)
  (interactive "sGrid Square: 
sGrid Square: ")
  (message "%d degrees" (round (haversine-great-circle-bearing (latlon x) (latlon y)))))

(defun latlon-dist (lat2 lon2)
  (interactive "nLatitude 2: 
nLongitude 2:")
  (message "%f miles" (haversine-great-circle-distance-miles  '(37.428964 -122.11505) (list lat2 lon2))))
												       
(defun latlon-bearing (lat2 lon2)
  (interactive "nLatitude 2:
nLongitude 2:")  
  (message "%d degrees" (round (haversine-great-circle-bearing  '(37.428964 -122.11505) (list lat2 lon2)))))

;; Note: All calculations in degrees
;; lo = longitude + 180 (offset origin to -180degrees longitude,)
;; La = latitude + 90 (-90 degrees latitude.)

;; C1 = ASCII CHAR ('IP(lo/20)+65')
;; C2 = ASCII CHAR ('IP(la/10)+65')
;; C3 = ASCII CHAR ('IP(FP(lo/20)*10)+48')    
;; C4 = ASCII CHAR ('IP(FP(la/10)*10)+48')
;; C5 = ASCII CHAR ('IP(FP(lo/2)*24)+65')
;; C6 = ASCII CHAR ('IP(FP(la)*24)+65')
;; QRA Locator = String C1+C2+C3+C4+C5+C6

(defun qra (latitude longitude) 
  (setq longitude (+ longitude 180.0))
  (setq latitude (+ latitude 90.0))
  (format
   "%c%c%c%c%c%c"
   (+ ?A (floor longitude 20.0))
   (+ ?A (floor latitude 10.0))
   (+ ?0 (floor (* 10 (- (/ longitude 20.0) (floor longitude 20.0)))))
   (+ ?0 (floor (* 10 (- (/ latitude 10.0) (floor latitude 10.0)))))
   (+ 32 ?A (floor (* 24 (- (/ longitude 2.0) (floor longitude 2.0)))))
   (+ 32 ?A (floor (* 24 (- latitude (floor latitude)))))))

(defun latlon (qra) 
  (let ((c1 (upcase (aref qra 0)))
	(c2 (upcase (aref qra 1)))
	(c3 (aref qra 2))
	(c4 (aref qra 3))
	(c5 (if (> (length qra) 4) (upcase (aref qra 4)) nil))
	(c6 (if (> (length qra) 5) (upcase (aref qra 5)) nil)))
    (list
     ;; Latitude
     (- (+ (* (- c2 ?A) 10.0)
	   (* (- c4 ?0) 1.0)
	   (if (null c6) 0.0 (/ (+ 0.5 (- c6 ?A)) 24.0)))
	90.0)
     ;; Longitude
     (- (+ (* (- c1 ?A) 20.0)
	   (* (- c3 ?0) 2.0)
	   (if (null c5) 0.0 (/ (+ 0.5 (- c5 ?A)) 12.0)))
	180.0))))

;;;
;;; Great Circle distance and bearing based on Haversine formula 
;; (from R.W. Sinnott, "Virtues of the Haversine", Sky and Telescope, vol. 68, no. 2, 1984, p. 159): 
;; as described by N9SSA in
;;; http://www.qsl.net/n9ssa/mpwnotes.html

(defun haversine-great-circle-distance-km (latlon1 latlon2)
  (let* ((k (/ 180.0 pi))
	 (lat1r (/ (car latlon1) k))
	 (lon1r (/ (cadr latlon1) k))
	 (lat2r (/ (car latlon2) k))
	 (lon2r (/ (cadr latlon2) k))
	 (R 6367000.0)
	 (dlonr (- lon2r lon1r))
	 (dlatr (- lat2r lat1r))
	 (a (+ (* (sin (/ dlatr 2.0)) (sin (/ dlatr 2.0)))
	       (* (cos lat1r) (cos lat2r) (sin (/ dlonr 2.0)) (sin (/ dlonr 2.0)))))
	 (c (* 2 (atan2 (sqrt a) (sqrt (- 1.0 a)))))
	 (d (* R c)))
    (/ d 1000.0)))

(defun haversine-great-circle-distance-miles (latlon1 latlon2)
  (* (haversine-great-circle-distance-km latlon1 latlon2) 0.621371192))

(defun atan2 (y x)
  (if (or (= x 0) (= y 0))
      (if (>= y 0.0)
	  (/ pi 2.0)
	(- (/ pi 2.0)))
    (if (< x 0.0) 
	(if (>= y 0.0)
	    (- pi (atan (- (/ y x))))
	  (+ (- pi) (atan (/ y x))))
      (if (> y 0)
	  (atan(/ y x))
	(- (atan (- (/ y x))))))))

(defun haversine-great-circle-bearing (latlon1 latlon2) 
  ;; theta = atan2(sin(delta long).cos(lat2), 
  ;;               cos(lat1).sin(lat2) - sin(lat1).cos(lat2).cos(deltalong)
  (let* ((k (/ 180.0 pi))
	 (lat1r (/ (car latlon1) k))
	 (lon1r (/ (cadr latlon1) k))
	 (lat2r (/ (car latlon2) k))
	 (lon2r (/ (cadr latlon2) k))
	 (delta-long (- lon2r lon1r))
	 (delta-lat (- lat2r lat1r))
	 (bearing (atan2 (* (sin delta-long)
			       (cos lat2r))
			    (- (* (cos lat1r) (sin lat2r))
			       (* (sin lat1r) (cos lat2r) (cos delta-long))))))
    
    (mod (+ 360.0 (* k bearing)) 360.0)))


(provide 'qra)

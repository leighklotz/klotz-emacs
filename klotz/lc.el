(defconst pi (* 4.0 (atan 1.0)))

(defun resonant-frequency (l c) 
  (/ 1.0 (* 2 pi (sqrt (* l c)))))

(defun mhz (hz)
  (/ hz 1e6))

(defun uf (p)
  (/ p 1e6))

(defun pf (p)
  (/ p 1e12))

(defun uf (u)
  (/ u 1e6))

(defun uh (u)
  (/ u 1e6))


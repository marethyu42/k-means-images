;;; packages !! import a lot of stuff for kmeans
(load "cl-math/math.lisp")
(load "cl-math/matrix.lisp")
(load "cl-math/list.lisp")
(load "cl-math/random.lisp")
(load "cl-ml/k-means/k-means.lisp")
(ql:quickload :split-sequence)
;; (load "k-means/Lisp/km.lisp")
;; (load "k-means/Lisp/km_test.lisp")

;;; file handling things!

;; image size for my example is: 341 x 540
;; use imagemagick to convert jpeg to ppm: sudo magick input-jpeg\ copy.jpeg -resize 25% 25input.ppm

(defun string-to-list (str)
  "convert a STRING of integers into a list of integers"
  (butlast (mapcar (lambda (x) (if (not (equal x "")) (parse-integer x)))
		   (cddr (split-sequence:split-sequence #\SPACE str)))))

(defun fit-into-threes-2 (lst)
  "convert a LST of values into trios (i.e. (1 2 3 4 5 6) -> ((1 2 3) (4 5 6))"
  (mapcar (lambda (x y z) (append (list x) (list y) (list z)))
	  (loop for val in lst by #'cdddr collect val)
	  (loop for val in (cdr lst) by #'cdddr collect val)
	  (loop for val in (cddr lst) by #'cdddr collect val)))

(defun read-file (input)
  "read ppm named INPUT into a string"
  (let ((x ""))
    (with-open-file (f input)
      (do ((l (read-line f) (read-line f nil 'eof)))
          ((eql l 'eof) "done!")
	(setq x (concatenate 'string x l))))
    x))

 (defun make-ppm (size data &optional (format "P3") (max 255) (output "output.ppm")) ;;using ppms to import file information! (how dare i use side effects in lisp :O)
  "SIZE is a list of (width height). DATA is a list of (r g b) values. import DATA to create a ppm. bonus, you can rename the file with OUTPUT. has side effects." 
  (with-open-file (f output
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (concatenate 'string format "
") f) 
    (write-sequence (concatenate 'string (write-to-string (car size)) " " (write-to-string (cadr size)) "
") f)
    (write-sequence (concatenate 'string (write-to-string max) "
 ") f)
      (loop for val in data
	    do (write-sequence (apply #'concatenate (append (list 'string) (mapcar #'(lambda (x) (concatenate 'string x " ")) (mapcar #'write-to-string val)))) f))))


(defvar img (fit-into-threes-2 (string-to-list (read-file "25input.ppm")))) ;;importing our image

;;; kmeans work :D

(defparameter *km*
  (make-instance 'k-means :k 13)) ;; make a kmeans model. :k is for num of centroids
(defparameter *X-train*
  (matrix-from-data img)) ;; use all image pixel colors as training data
(defvar fit-lst (fit *km* *X-train* '())) ;; fit training data to k-means model
;(print (list "fit: " fit-lst))
(defvar centroids (get-centroids *km*)) ;; get centroids from kmeans model
;(print (list "centroids: " centroids))
(defvar round-centroids (mapcar (lambda (x) (mapcar (lambda (y) (round (- 255 y))) x)) centroids))
;; round centroids ^

;;; new stuff

(defvar new-lst nil) ;;make a new image
(setq new-lst
      (loop for val in fit-lst
	    collect (nth val round-centroids))) ;;actually adding new values to it

(make-ppm '(341 540) (append new-lst (last new-lst))) ;;exporting into image

;;; goals / notes:

;; use average color in cluster?
;; bonus: layer edge detection thingies?
        ; make a threasholding function that takes in two images of the same size, and when image b's intensity >= VAL, it uses the pixel from image B instead of A



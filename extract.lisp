;; Extract and decode the PNG pixel data from the calculator
;; screenshots.

;; Zoom size: 200%


(ql:quickload :pngload)

(defpackage "DECODE-PNG"
  (:use "CL" "PNGLOAD")
  (:export "PNG-TO-BYTES"
           "SAVE-BYTES"))
(in-package "DECODE-PNG")

(defun subrect (bitmap x y width height subsample bit-transform)
  (let* ((sh      (truncate height subsample))
         (sw      (truncate width  subsample))
         (subrect (make-array (list sh sw) :element-type 'bit)))
    (loop :for j :below sh
          :do (loop :for i :below sw
                    :do (setf (aref subrect j i)
                              (funcall bit-transform
                                       (aref bitmap
                                             (+ y (* j subsample))
                                             (+ x (* i subsample))
                                             0)))))
    subrect))

(defun decode-bytes (bitmap)
  (loop
    :with s := (make-array (/ (reduce (function *) (array-dimensions bitmap)) 8)
                           :element-type '(unsigned-byte 8))
    :with r := -1
    :for j :below (array-dimension bitmap 0)
    :do (loop :for i :below (array-dimension bitmap 1) :by 8
              :do (loop :for k :below 8
                        :for b := (aref bitmap j i)
                          :then (logior (ash b 1) (aref bitmap j (+ i k)))
                        :finally (setf (aref s (incf r)) b)))
    :finally (return s)))


(defun png-to-bytes (pathname)
  (let ((png (load-file pathname))
        (offset 2)
        (bit-size 4))
    (bit-depth png)
    (let* ((data (data png))
           (h (array-dimension data 0))
           (w (array-dimension data 1)))

      (decode-bytes (subrect data offset offset (- w (* 2 offset)) (- h (* 2 offset)) bit-size (lambda (bit) (if (> bit 0) 0 1)))))))

(defun save-bytes (bytes pathname)
  (with-open-file (out pathname :direction :output :if-does-not-exist :create :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence bytes out)))

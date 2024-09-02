;; utilities.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :obj-reader)

(defclass bounding-box ()
  ((min-x :type single-float :initform most-positive-single-float)
   (max-x :type single-float :initform most-negative-single-float)
   (min-y :type single-float :initform most-positive-single-float)
   (max-y :type single-float :initform most-negative-single-float)
   (min-z :type single-float :initform most-positive-single-float)
   (max-z :type single-float :initform most-negative-single-float)))

(defun bounding-box (obj)
  (declare (type obj-file obj)
           (optimize (speed 3)))
  (let ((bb (make-instance 'bounding-box)))
    (with-slots (min-x min-y min-z max-x max-y max-z) bb
      (loop
        :for obj :of-type obj-object :in (slot-value obj 'objects)
        :do
           (with-slots (vertices) obj
               (loop
                 :for idx fixnum
                   :below (/  (length vertices) 3)
                 :do
                    (setf min-x (min min-x (aref vertices (the fixnum (* 3 idx)))))
                    (setf max-x (max max-x (aref vertices (the fixnum (* 3 idx)))))
                    (setf min-y (min min-y (aref vertices (the fixnum (+ 1 (* 3 idx))))))
                    (setf max-y (max max-y (aref vertices (the fixnum (+ 1 (* 3 idx))))))
                    (setf min-z (min min-z (aref vertices (the fixnum (+ 2 (* 3 idx))))))
                    (setf max-z (max max-z (aref vertices (the fixnum (+ 2 (* 3 idx))))))))))
    bb))

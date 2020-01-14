;;;; material-reader.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


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

(in-package :wavefront-reader)


(defclass wavefront-material ()
  ((material-name :initarg :material-name :type string)
   (attributes :initform (make-hash-table :test 'equal))))

(defun get-attribute (mtl attrib-name)
  (with-slots (attributes) mtl
    (gethash attrib-name attributes)))

(defun set-attribute (mtl attrib-name attrib-value)
  (with-slots (attributes) mtl
    (setf (gethash attrib-name attributes) (if (> (length attrib-value) 1)
                                               attrib-value
                                               (car attrib-value)))))

(defun read-mtl (ins)
  "Read a WaveFront OBJ file into memory."
  (let ((all-mtls nil)
        (current-mtl nil))
    (loop
       for line = (read-line ins nil)
       while line
       do
         (let* ((parts (cl-ppcre:split "\\s" line))
                (operator (car parts))
                (operands (cdr parts)))
           (cond
             ((null operator)
              t)
             ;; Comment
             ((char= #\# (aref operator 0))
              t)

             ((string= "newmtl" operator)
              (when current-mtl
                (push current-mtl all-mtls))
              (setf current-mtl
                    (make-instance 'wavefront-material
                                   :material-name (format nil "~{~a~^ ~}" operands))))
             (operator
              (set-attribute current-mtl operator (mapcar #'read-from-string operands))))))

    (when current-mtl
      (push current-mtl all-mtls))
    all-mtls))

(defun read-mtl-from-file (file-name)
  (with-input-from-file (ins file-name)
    (read-mtl ins)))

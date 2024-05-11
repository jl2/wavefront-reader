;; material-reader.lisp
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

(defclass obj-material ()
  ((material-name :initarg :material-name :type string)
   (attributes :initform (make-hash-table :test 'equal)))
  (:documentation "A Wavefront Material file.
A name and hashtable of surface attributes."))

(defun get-attribute (mtl attrib-name)
  "Get a surface attribute of the material."
  (with-slots (attributes) mtl
    (gethash attrib-name attributes)))

(defun set-attribute (mtl attrib-name attrib-value)
  "Set a surface attribute of the material."
  (with-slots (attributes) mtl
    (setf (gethash attrib-name attributes)
          (if (> (length attrib-value) 1)
              attrib-value
              (car attrib-value)))))

(defun read-mtl (ins)
  "Read a Wavefront .mtl material file into memory."
  (let ((all-mtls nil)
        (current-mtl nil))
    (loop
      :for line = (read-line ins nil)
      :while line
      :for no-comment = (str:trim (subseq line
                                          0
                                          (search "#" line)))
      :for (operator operands) = (str:words no-comment :limit 2)
;;      :when (not (zerop (length no-comment)))

      ;; :for parts = (cl-ppcre:split "\\s" line)
      ;; :for operator = (first parts)
      ;; :for operands = (rest parts)
      :when operator
      :do
         (cond
           ((null operator)
            t)

           ((zerop (length operator))
            t)
           ;; Comment
           ((char= #\# (aref operator 0))
            t)

           ((string= "newmtl" operator)
            (when current-mtl
              (push current-mtl all-mtls))
            (setf current-mtl
                  (make-instance 'obj-material
                                 :material-name (format nil "~{~a~^ ~}" (ensure-list operands)))))
           (operator
            ;; (format t "~a: ~{~a~^ ~}" operator (str:words operands))
            (set-attribute current-mtl
                           operator
                           (mapcar #'read-from-string (str:words operands))))))

    (when current-mtl
      (push
       (cons (slot-value current-mtl 'material-name) current-mtl)
       all-mtls))
    all-mtls))

(defun read-mtl-from-file (file-name)
  "Read a material from the named file."
  (with-input-from-file (ins file-name)
    (read-mtl ins)))

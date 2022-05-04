;; obj-reader.lisp
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

(defclass obj-group ()
  ((group-name :initarg :group-name :type (or null string))
   (smoothing-group :initform nil :type (or null fixnum))
   (material :initform nil :type (or null string))

   (face-stride :initarg :face-stride :initform 0)
   (faces :initform (make-array 10000
                                :element-type 'single-float
                                :adjustable t
                                :fill-pointer 0))

   (line-stride :initarg :line-stride :initform 0)
   (lines :initform (make-array 1000
                                :element-type 'single-float
                                :adjustable t
                                :fill-pointer 0))

   (points :initform (make-array 1000
                                 :element-type 'single-float
                                 :adjustable t
                                 :fill-pointer 0)))

  (:documentation "An object group in a WaveFront OBJ file."))

(defclass obj-object ()
  ((object-name :initarg :object-name :type (or null string))
   (groups :initform (make-array 0
                                 :element-type 'obj-group
                                 :initial-contents '()
                                 :adjustable t
                                 :fill-pointer 0)))
  (:documentation "A WaveFront OBJ file."))

(defclass obj-file ()
  ((objects :initarg :objects)
   (materials :initarg :materials))
  (:documentation "Objects and materials from a Wavefront OBJ file."))

(defun map-index (buffer idx)
  (cond ((< 0 idx)
         (1- idx))
        ((> 0 idx)
         (+ (length buffer) idx))
        (t
         0)))

(defun read-obj-points (object group operands)
  (with-slots (vertices) object
    (with-slots (points) group
      (loop
        :for idx :in operands
        :for midx = (map-index vertices (or (parse-integer idx :junk-allowed t) 0))
        :do
           (dotimes (offset 3)
             (let ((this-idx (+ offset (* 3 midx))))
               (vector-push-extend (aref vertices this-idx) points 3)))))))

(defun decide-stride (first-entry)
  (let ((has-vert (not (string= "" (car first-entry))))
        (has-text (not (string= "" (cadr first-entry))))
        (has-norm (not (string= "" (caddr first-entry)))))
    (cond ((and has-vert has-text has-norm) 8)
          ((and has-vert has-text) 5)
          ((and has-vert has-norm) 6)
          ((and has-vert) 3))))

(defun read-obj-line (object group operands)
  (when operands
    (with-slots (vertices tex-coords) object
      (with-slots (line-stride lines) group

        (loop
        :for oper :in operands
        :for parts = (str:split "/" oper :omit-nulls nil)
        :when (= 0 line-stride)
          :do
             (setf line-stride (decide-stride parts))
        :do
           (loop
             :for comp :in parts
             :for data in (list vertices tex-coords)
             :for midx = (map-index data (or (parse-integer comp :junk-allowed t) 0))
             :when (not (string= comp ""))
               :do
                  (dotimes (offset 3)
                    (let ((this-idx (+ offset (* 3 midx))))
                      (vector-push-extend (aref data this-idx) lines  (* 1000 line-stride (length operands)))))))))))


(defun read-obj-face (object group operands)
  (when operands
    (with-slots (vertices tex-coords normals) object
      (with-slots (face-stride faces) group

        (loop
        :for oper :in operands
        :for parts = (str:split "/" oper :omit-nulls nil)
        :when (= 0 face-stride)
          :do
             (setf face-stride (decide-stride parts))
        :do
           (loop
             :for comp :in parts
             :for data in (list vertices tex-coords normals)
             :for midx = (map-index data (or (parse-integer comp :junk-allowed t) 0))
             :when (not (string= comp ""))
               :do
                  (dotimes (offset 3)
                    (let ((this-idx (+ offset (* 3 midx))))
                      (vector-push-extend (aref data this-idx) faces  (* 1000 face-stride (length operands)))))))))))


(defun add-group (obj group)
  (with-slots (groups) obj
    (vector-push-extend group groups)))

(defun add-data (obj operator operands)
  (let ((slot-name (assoc-value '(("v" . vertices)
                                   ("vn" . normals)
                                   ("vt" . tex-coords)
                                   ("vp" . v-params))
                                 operator :test #'string=)))
    (dolist (vp operands)
      (vector-push-extend
       (coerce (read-from-string vp) 'single-float)
       (slot-value obj slot-name)))))


(defclass vertex-data ()
  ((vertices :initform (make-array 1000
                                   :element-type 'single-float
                                   :adjustable t
                                   :fill-pointer 0))
   (normals :initform (make-array 1000
                                  :element-type 'single-float
                                  :adjustable t
                                  :fill-pointer 0))
   (tex-coords :initform (make-array 1000
                                     :element-type 'single-float
                                     :adjustable t
                                     :fill-pointer 0))
   (v-params :initform (make-array 1000
                                   :element-type 'single-float
                                   :adjustable t
                                   :fill-pointer 0))))

(defun read-obj (ins)
  "Read a WaveFront OBJ file into memory."
  (let ((all-objects nil)
        (materials (make-hash-table :test 'equal))
        (current-group nil)
        (current-object nil)
        (temp-data (make-instance 'vertex-data)))
    (loop
       :for line = (read-line ins nil)
       :while line
       :do
         (let* ((parts (cl-ppcre:split "\\s" line))
                (operator (car parts))
                (operands (cdr parts)))
           (cond
             ;; Comment
             ((char= #\# (aref operator 0))
              t)

             ((string= "mtllib" operator)
              (let ((mats (read-mtl-from-file (format nil "~{~a~^ ~}" operands))))
                (loop :for material :in mats :do
                  (with-slots (material-name) material
                    (setf (gethash material-name materials) material)))))

             ;; object name - operands are words of the name
             ((string= "o" operator)
              ;; Close out any existing group or object
              (when current-group
                (add-group current-object current-group)
                (setf current-group nil))
              (when current-object
                (push current-object all-objects))
              (setf current-object
                    (make-instance 'obj-object
                                   :object-name (format nil "~{~a~^ ~}" operands))))

             ;; Group - operands are group names
             ((string= "g" operator)
              ;; Close out any existing group
              (when current-group
                (add-group current-object current-group))
              (setf current-group
                    (make-instance 'obj-group
                                   :group-name (format nil "~{~a~^ ~}" operands))))

             ((string= "s" operator)
              (with-slots (smoothing-group) current-group
                (setf smoothing-group (read-from-string (car operands)))))

             ((string= "usemtl" operator)
              (with-slots (material) current-group
                (setf material (format nil "~{~a~^ ~}" operands))))

             ;; Vertex - operands are x y z [w]
             ;; Normal - operands are  i j k
             ;; Texture coordinate - operands are  u [v [w]]
             ;; Vertex parameter - operands are u [v [w]]
             ((or (string= "v" operator)
                  (string= "vn" operator)
                  (string= "vt" operator)
                  (string= "vp" operator))
              (add-data temp-data operator operands))

             ;; Points - operands are vertex indices
             ;; p 1 2 3
             ((string= "p" operator)
              (read-obj-points temp-data current-group operands))

             ;; Lines - operands are vertex and optional texture parameter indices?
             ;; l 1/1 2/2 3/3
             ((string= "l" operator)
              (read-obj-line temp-data current-group operands))

             ;; Faces - operands are vertex, optional texture parameter, and optional normal indices
             ;; f 1//1 2//2 3//3
             ;; f 1/1/1 2/2/2 3/3/3
             ;; f 1/1 2/2 3/3
             ((string= "f" operator)
              (read-obj-face temp-data current-group operands))

             (t
              (format t "Unhandled operator ~a~%" operator)))))
    (when current-group
      (add-group current-object current-group))
    (when current-object
      (push current-object all-objects))
    (make-instance 'obj-file :objects all-objects :materials (alexandria:flatten materials))))

(defun read-obj-from-file (file-name)
  (uiop/filesystem:with-current-directory ((directory-namestring file-name))
    (with-input-from-file (ins file-name)
      (read-obj ins))))

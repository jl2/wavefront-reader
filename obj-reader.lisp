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

(defparameter *obj-real-type* 'single-float)

(defclass obj-file ()
  ((objects :initarg :objects)
   (materials :initarg :materials))
  (:documentation "Objects and materials from a Wavefront OBJ file."))

(defclass obj-object ()
  ((object-name :initarg :object-name :type (or null string))
   (vertices :initform (make-array 0
                                   :element-type *obj-real-type*
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (normals :initform (make-array 0
                                  :element-type *obj-real-type*
                                  :initial-contents '()
                                  :adjustable t
                                  :fill-pointer 0))
   (tex-coords :initform (make-array 0
                                     :element-type *obj-real-type*
                                     :initial-contents '()
                                     :adjustable t
                                     :fill-pointer 0))
   (v-params :initform (make-array 0
                                   :element-type *obj-real-type*
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (groups :initform (make-array 0
                                 :element-type 'obj-group
                                 :initial-contents '()
                                 :adjustable t
                                 :fill-pointer 0)))
  (:documentation "A collection of geometry groups and vertex data (including vertices, normals, and texture parameters) that makes up an OBJ file.
Geometric groups collections of faces, lines, and points that index into the vertex data."))

(defclass obj-group ()
  ((group-name :initarg :group-name
               :type (or null string))
   (smoothing-group :initform nil
                    :type (or null fixnum))
   (material :initform nil
             :type (or null string))
   (faces :initform (make-array 0
                                :element-type 'obj-face
                                :initial-contents '()
                                :adjustable t
                                :fill-pointer 0))
   (lines :initform (make-array 0
                                :initial-contents '()
                                :element-type 'obj-line
                                :adjustable t
                                :fill-pointer 0))
   (points :initform (make-array 0
                                 :element-type 'fixnum
                                 :initial-contents '()
                                 :adjustable t
                                 :fill-pointer 0)))

  (:documentation "An obj-group is a single geometric entity with a name and material.
The group is made up of faces, lines, and points whose vertex data indexes into the vertex data of the parent object."))


(deftype obj-index ()
  `(and (simple-array fixnum 1)))



(defclass obj-geometry ()
  ((idx-format :initarg :idx-format)
   (indices :initarg :indices
            :initform (make-array 0
                                  :initial-contents '()
                                  :element-type 'obj-index
                                  :adjustable t
                                  :fill-pointer 0)))
  (:documentation "A Wavefront OBJ face or line.
idx-format indicates which data is referenced (:vertex, :vertex-normal, :vertex-tex-normal, etc.) and determines the stride,
and indices contains the indices themselves."))

;; Face format is one of:
;; :vertex - 1 or 1/ or 1//
;; :vertex-normal - 1//1
;; :vertex-tex - 1/1 or 1/1/
;; :vertex-tex-normal - 1/1/1
(defclass obj-face (obj-geometry)
  ()
  (:documentation "A Wavefront OBJ face."))

;; Line format is:
;; :vertex - 1 or 1/
;; :vertex-tex - 1/1
(defclass obj-line (obj-geometry)
  ()
  (:documentation "A Wavefront OBJ line."))

(defun stride (obj-geometry)
  (format-stride (slot-value obj-geometry 'idx-format)))

(defun fixup-negative-indices (file type idx)
  "OBJ indices are 1 based, and allow negative values that index from the end of the array.
fixup-negative-indices converts these index values into positive, 0 based indices."
  (declare (type fixnum idx)
           (type obj-object file))
  (cond
    ((< 0 idx)
     (1- idx))

    ((> 0 idx)
     (+ (length (slot-value file type))
        idx))
    (t
     (error 'invalid-obj-index :index idx))))

(defun add-point (object group operands)
  "Add a point to the given group."
  (dolist (idx (str:words operands))
    ;; Points can only have a single vertex, and are indexed by single integers
    (with-slots (points) group
      (vector-push-extend
       (fixup-negative-indices object 'vertices (the fixnum (read-from-string idx)))
       points))))


(defun read-obj-line (object operand-string)
  "Convert an input line of text into a new obj-line"


  (let* (;; Split "1/1 2/2 3/3" into ("1/1", "2/2", "3/3")
         (operands (str:words operand-string))

         ;; Split "1/1" into ("1" "1")
         (first-index (str:split "/" (car operands) :omit-nulls nil))

         ;; Count the elements
         (index-len (length first-index))

         ;; And that's how many are expected each time
         (expected-indices index-len)

         ;; Decide what data format the line uses
         (idx-format (cond ((= 1 index-len)
                            :vertex)
                           ((and (= 2 index-len)
                                 (cadr first-index)
                                 (> 0 (length (cadr first-index))))
                            :vertex-texture)
                           ((and (= 2 index-len)
                                 (cadr first-index)
                                 (= 0 (length (cadr first-index))))
                            :vertex)))
         (indices (loop
                    :for oper :in operands
                    :for i :from 0
                    :for components = (str:split "/" oper :omit-nulls nil)
                    :for len-components = (length components)
                    :when (/= len-components expected-indices)
                      :do (error 'invalid-line-index :index oper)
                    :collect (make-array len-components
                                         :element-type 'fixnum
                                         :initial-contents
                                         (mapcar (lambda (idx)
                                                   (fixup-negative-indices object
                                                                           'vertices
                                                                           (the fixnum (read-from-string idx))))
                                                 components)))))


    (make-instance 'obj-line
                   :idx-format idx-format
                   :indices (make-array (length operands)
                                        :initial-contents indices))))

(defun add-line (object group operands)
  "Add a new line to object group."
  (with-slots (lines) group
    (vector-push-extend (read-obj-line object operands) lines)))


(defun decide-face-format (first-entry)
  "Determine the face format based on which indices are given."
  (let ((has-vert (and (car first-entry)
                       (string/= "" (car first-entry))))
        (has-text (and (cadr first-entry)
                       (string/= "" (cadr first-entry))))
        (has-norm (and (caddr first-entry)
                       (string/= "" (caddr first-entry))))
        (has-vdata (and (cadddr first-entry)
                        (string/= "" (cadddr first-entry)))))

    (cond ((and has-vert has-text has-norm has-vdata)
           :vertex-texture-normal-vdata)

          ((and has-vert has-text has-norm)
           :vertex-texture-normal)

          ((and has-vert has-text has-vdata)
           :vertex-texture-vdata)

          ((and has-vert has-norm has-vdata)
           :vertex-normal-vdata)

          ((and has-vert has-text)
           :vertex-texture)
          ((and has-vert has-vdata)
           :vertex-vdata)
          ((and has-vert has-norm)
           :vertex-normal)

          ((and has-vert)
           :vertex)

          ((and has-text has-norm has-vdata)
           :texture-normal-vdata)

          ((and has-text has-norm)
           :texture-normal)
          ((and has-text has-vdata)
           :texture-vdata)

          ((and has-text)
           :texture)

          ((and has-norm has-vdata)
           :normal-vdata)
          ((and has-norm)
           :normal)

          ((and has-vdata)
           :vdata)
          (t
           (error "Invalid index count for a face!")))))

(defun format-stride (format)
  "Lookup the stride (number of indices) used by a specific data format."
  (assoc-value '((:vertex-texture-normal . 3)
                 (:vertex-texture . 2)
                 (:vertex-normal . 2)
                 (:vertex . 1))
               format))

(defun read-obj-face (object operand-string)
  "Read a new face whose index data refers to object."

  (let* (;; Split "1/1 2/2 3/3" into ("1/1", "2/2", "3/3")
         (operands (str:words operand-string))

         ;; Split "1/1" into (1 1)
         (first-index (str:split "/" (car operands) :omit-nulls nil))

         ;; Decide the format
         (idx-format (decide-face-format first-index))

         (stride (format-stride idx-format))

         ;; Allocate enough space for them all
         (indices (loop
                    :for oper :in operands
                    :for components :of-type list = (str:split "/" oper :omit-nulls t)
                    :for len-components fixnum = (length components)
                    :when (/= len-components stride)
                      :do
                         (error 'invalid-face-index :index oper
                                                    :stride stride
                                                    :part-count len-components)
                    :collect
                    (make-array len-components
                                :element-type 'fixnum
                                :initial-contents
                                (mapcar (lambda (idx)
                                          (fixup-negative-indices object 'vertices (the fixnum (read-from-string idx))))
                                        components)))))
    (declare (type fixnum stride))

    ;; Read the face data into memory

    ;; Create the face
    (make-instance 'obj-face
                   :idx-format idx-format
                   :indices (make-array (length operands)
                                        :element-type 'obj-index
                                        :initial-contents indices
                                        ))))

(defun add-face (object group operands)
  "Read a new face from operands and add it to group."
  (with-slots (faces) group
    (vector-push-extend (read-obj-face object operands) faces)))

(defun add-group (obj group)
  "Add a new group to obj."
  (with-slots (groups) obj
    (vector-push-extend group groups)))

(defun add-vertex-data (obj operator operands)
  "Add a new vertex, normal, texture coordinate or parameter to obj."
  (let ((slot-name (assoc-value '(("v" . vertices)
                                  ("vn" . normals)
                                  ("vt" . tex-coords)
                                  ("vp" . v-params))
                                operator :test #'string=)))
    (dolist (vp (str:words operands))
      (vector-push-extend
       (coerce (read-from-string vp) *obj-real-type*)
       (slot-value obj slot-name)))))


(define-condition invalid-obj-index (error)
  ((index :initarg :index :reader index)))

(define-condition invalid-line-index (error)
  ((index :initarg :index :reader index)))

(define-condition invalid-face-index (error)
  ((index :initarg :index :reader index)
   (part-count :initarg :part-count :reader part-count)
   (stride :initarg :stride)))

(defun read-obj-from-file (file-name)
  "Read Wavefront OBJ data from file-name."
  ;; Use with-current-directory so .mtl files are found in the correct location.
  (uiop/filesystem:with-current-directory ((directory-namestring file-name))
    (with-input-from-file (ins file-name)
      (read-obj ins))))

(defun read-obj (ins)
  "Read a WaveFront OBJ file into memory."
  (let ((all-objects nil)
        (materials (make-hash-table :test 'equal))
        (current-group nil)
        (current-object (make-instance 'obj-object :object-name "default")))

    (loop
      :for line = (read-line ins nil)
      :while line

      :for no-comment = (str:trim (subseq line
                                          0
                                          (search "#" line)))
      :for (operator operands) = (str:words no-comment :limit 2)
      :when (> (length no-comment) 0)
        :do
         (cond
           ((string= "mtllib" operator)
            (loop :for (material-name . material) :in (read-mtl-from-file operands) :do
              (setf (gethash material-name materials) material)))

           ;; object name - operands are words of the name
           ((string= "o" operator)
            (cond ((and current-object
                        (zerop (length (slot-value current-object 'vertices))))
                   (setf (slot-value current-object 'object-name) operands))
                  (current-object
                   (push current-object all-objects)
                   (setf current-object
                  (make-instance 'obj-object :object-name operands)))))

             ((string= "g" operator)
              (when current-group
                (add-group current-object current-group))
              (setf current-group
                    (make-instance 'obj-group :group-name operands)))

             ((string= "s" operator)
              (with-slots (smoothing-group) current-group
                (setf smoothing-group (read-from-string operands))))

             ((string= "usemtl" operator)
              (with-slots (material) current-group
                (setf material operands)))

             ;; Vertex - operands are x y z [w]
             ;; Normal - operands are  i j k
             ;; Texture coordinate - operands are  u [v [w]]
             ;; Vertex parameter - operands are u [v [w]]
             ((or (string= "v" operator)
                  (string= "vn" operator)
                  (string= "vt" operator)
                  (string= "vp" operator))
              (add-vertex-data current-object operator operands))

             ;; Points - operands are vertex indices
             ;; p 1 2 3
             ((string= "p" operator)
              (add-point current-object current-group operands))

             ;; Lines - operands are vertex and optional texture parameter indices?
             ;; l 1/1 2/2 3/3
             ((string= "l" operator)
              (add-line current-object current-group operands))

             ;; Faces - operands are vertex, optional texture parameter, and optional normal indices
             ;; f 1//1 2//2 3//3
             ;; f 1/1/1 2/2/2 3/3/3
             ;; f 1/1 2/2 3/3
             ((string= "f" operator)
              (add-face current-object current-group operands))

             (t
              (format t "Unhandled operator ~s~%" operator))))
    (when current-group
      (add-group current-object current-group))
    (when current-object
      (push current-object all-objects))
    (make-instance 'obj-file :objects all-objects :materials materials)))

;; (defun read-obj-from-file (file-name)
;;   (uiop/filesystem:with-current-directory ((directory-namestring file-name))
;;     (with-input-from-file (ins file-name)
;;       (read-obj ins))))

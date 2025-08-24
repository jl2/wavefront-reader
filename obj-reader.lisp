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

(defclass obj-material ()
  ((material-name :initarg :material-name
                  :type string
                  :accessor material-name
                  )

   (attributes :initform (make-hash-table :test 'equal)
               :type hashtable
               :accessor attributes))
  (:documentation "A Wavefront Material file.
A name and hashtable of surface attributes."))

(defun create-empty-material ()
  (make-instance 'obj-material
                 :material-name "default"))

(defun add-attribute (material att-name value &optional replace)
  (with-slots (attributes material-name ) material
    (multiple-value-bind (val present-p) (gethash att-name attributes)

      (when (and present-p (null replace))
        (error "Attribute ~a already exists with value ~a in material ~a"
               att-name val material-name)))
    (setf (gethash att-name attributes) value)))

(defun default-material ()
  (let ((mat (create-empty-material)))
    (add-attribute mat "Kd" (vec3 0.0 0.9 0.0))
    (add-attribute mat "Ka" (vec3 0.1 0.1 0.1))
    (add-attribute mat "Ks" (vec3 1.0 1.0 1.0))
    (add-attribute mat "roughness" 80.0)
    mat))

(defun get-attribute (material name)
  (declare (type obj-material material)
           (type string name))
  (with-slots (attributes) material
    (gethash name attributes)))

(defclass obj-object ()
  ((object-name :initarg :object-name :type (or null string)
                :accessor object-name)
   (vertices :initform (make-array 0
                                   :element-type 'vec3
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0)
             :accessor vertices
             :type (vector vec3)
             )
   (normals :initform (make-array 0
                                  :element-type 'vec3
                                  :initial-contents '()
                                  :adjustable t
                                  :fill-pointer 0)
            :accessor normals
            :type (vector vec3)
            )
   (tex-coords :initform (make-array 0
                                     :element-type 'vec2
                                     :initial-contents '()
                                     :adjustable t
                                     :fill-pointer 0)
               :accessor tex-coords
               :type (vector vec2)
               )
   (v-params :initform (make-array 0
                                   :element-type *obj-real-type*
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0)
             :type (vector single-float)
             :accessor v-params
             )
   (groups :initform (make-array 0
                                 :element-type 'obj-group
                                 :initial-contents '()
                                 :adjustable t
                                 :fill-pointer 0)
           :accessor groups
           ;;:type (vector obj-group)
           ))
  (:documentation "A collection of geometry groups and vertex data (including vertices, normals, and texture parameters) that makes up an OBJ file.
Geometric groups collections of faces, lines, and points that index into the vertex data."))

(defclass obj-file ()
  ((objects :initarg :objects
            :type (vector obj-object)
            :accessor objects)
   (materials :initarg :materials
              :type hashtable
              :accessor materials))
  (:documentation "Objects and materials from a Wavefront OBJ file."))


(defclass obj-group ()
  ((group-name :initarg :group-name
               :type (or null string))
   (smoothing-group :initform nil
                    :type (or null fixnum))
   (material :initform nil
             :accessor material
             :type (or null string))
   (faces :initform (make-array 0
                                :element-type 'obj-face
                                :initial-contents '()
                                :adjustable t
                                :fill-pointer 0)
          :accessor faces)
   (lines :initform (make-array 0
                                :initial-contents '()
                                :element-type 'obj-line
                                :adjustable t
                                :fill-pointer 0)
          :accessor lines)
   (points :initform (make-array 0
                                 :element-type 'fixnum
                                 :initial-contents '()
                                 :adjustable t
                                 :fill-pointer 0)
           :accessor points))

  (:documentation "An obj-group is a single geometric entity with a name and ,material.
The group is made up of faces, lines, and points whose vertex data indexes into the vertex data of the parent object."))

(defgeneric get-material (which obj))
(defmethod get-material ((name string) (obj obj-file))
  (with-slots (materials) obj
    (gethash name materials)))

(defmethod get-material ((group obj-group)  (obj obj-file))
  (with-slots (materials) obj
    (with-slots (material) group
      (gethash material materials))))

(deftype obj-index ()
  `(and (simple-array fixnum 1)))

(defclass obj-geometry ()
  ((idx-format :initarg :idx-format
               :accessor idx-format)
   (indices :initarg :indices
            :accessor indices
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

(declaim (inline decide-face-format
                 add-point add-line add-face add-group
                 stride fixup-negative-indices))

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
  (declare (type string operands)
           (type obj-object object)
           (type obj-group group))
  (dolist (idx (str:words operands))
    ;; Points can only have a single vertex, and are indexed by single integers
    (with-slots (points) group
      (vector-push-extend
       (fixup-negative-indices object 'vertices (the fixnum (read-from-string idx)))
       points))))


(defun read-obj-line (object operand-string)
  "Convert an input line of text into a new obj-line"

  (declare (type obj-object object)
           (type string operand-string))

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
                    :for oper :of-type string :in operands
                    :for i fixnum :from 0
                    :for components :of-type list = (str:split "/" oper :omit-nulls nil)
                    :for len-components fixnum = (length components)
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
  (declare (type obj-object object)
           (type string operands)
           (type obj-group group))
  (with-slots (lines) group
    (vector-push-extend (read-obj-line object operands) lines)))


(defun decide-face-format (first-entry)
  "Determine the face format based on which indices are given."
  (declare (type list first-entry))

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
(defun vertex-index (format)
  (case format
    (:vertex-texture-normal-vdata 0)
    (:vertex-texture-normal 0)
    (:vertex-texture-vdata 0)
    (:vertex-normal-vdata 0)
    (:vertex-texture 0)
    (:vertex-vdata 0)
    (:vertex-normal 0)
    (:vertex 0)
    (:texture-normal-vdata nil)
    (:texture-normal nil)
    (:texture-vdata nil)
    (:texture nil)
    (:normal-vdata nil)
    (:normal nil)
    (:vdata nil)
    (t (error "Unknown format!"))))
(defun normal-index (format)
  (case format
    (:vertex-texture-normal-vdata 2)
    (:vertex-texture-normal 2)
    (:vertex-texture-vdata nil)
    (:vertex-normal-vdata 1)
    (:vertex-texture nil)
    (:vertex-vdata nil)
    (:vertex-normal 1)
    (:vertex nil)
    (:texture-normal-vdata 1)
    (:texture-normal 1)
    (:texture-vdata nil)
    (:texture nil)
    (:normal-vdata 0)
    (:normal 0)
    (:vdata nil)
    (t (error "Unknown format!"))))

(defun tex-index (format)
  (case format
    (:vertex-texture-normal-vdata 1)
    (:vertex-texture-normal 1)
    (:vertex-texture-vdata 1)
    (:vertex-normal-vdata nil)
    (:vertex-texture 1)
    (:vertex-vdata nil)
    (:vertex-normal nil)
    (:vertex nil)
    (:texture-normal-vdata 0)
    (:texture-normal 0)
    (:texture-vdata 0)
    (:texture 0)
    (:normal-vdata nil)
    (:normal nil)
    (:vdata nil)
    (t (error "Unknown format!"))))
(defun vdata-index (format)
  (case format
    (:vertex-texture-normal-vdata 3)
    (:vertex-texture-normal nil)
    (:vertex-texture-vdata 2)
    (:vertex-normal-vdata2)
    (:vertex-texture nil)
    (:vertex-vdata 1)
    (:vertex-normal nil)
    (:vertex nil)
    (:texture-normal-vdata 2)
    (:texture-normal nil)
    (:texture-vdata 1)
    (:texture nil)
    (:normal-vdata 1)
    (:normal nil)
    (:vdata 0)
    (t (error "Unknown format!"))))

(defun format-stride (format)
  "Lookup the stride (number of indices) used by a specific data format."
  (assoc-value '((:vertex-texture-normal . 3)
                 (:vertex-texture . 2)
                 (:vertex-normal . 2)
                 (:vertex . 1))
               format))

(defun read-obj-face (object operand-string)
  "Read a new face whose index data refers to object."
  (declare (type obj-object object)
           (type string operand-string))

  (let* (;; Split "1/1 2/2 3/3" into ("1/1", "2/2", "3/3")
         (operands (str:words operand-string))

         ;; Split "1/1" into (1 1)
         (first-index (str:split "/" (car operands) :omit-nulls nil))

         ;; Decide the format
         (idx-format (decide-face-format first-index))

         (stride (format-stride idx-format))

         ;; Allocate enough space for them all
         (indices (loop
                    :for oper :of-type string :in operands
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
                                        :initial-contents indices))))

(defun add-face (object group operands)
  "Read a new face from operands and add it to group."
  (declare (type obj-object object)
           (type string operands)
           (type obj-group group))

  (with-slots (faces) group
    (vector-push-extend (read-obj-face object operands) faces)))

(defun add-group (obj group)
  "Add a new group to obj."
  (declare (type obj-object obj)
           (type obj-group group))
  (with-slots (groups) obj
    (vector-push-extend group groups)))

(defun add-vertex-data (obj operator operands)
  "Add a new vertex, normal, texture coordinate or parameter to obj."
  (declare (type obj-object obj)
           (type string operator operands))
  (cond ((string= "v" operator)
         (vector-push-extend  (apply #'vec3 (mapcar (lambda (str)
                                                      (coerce (read-from-string str)
                                                              *obj-real-type*))
                                                    (subseq  (str:words operands) 0 3)))
                              (slot-value obj 'vertices)))
        ((string= "vn" operator)
         (vector-push-extend  (apply #'vec3 (mapcar (lambda (str)
                                                      (coerce (read-from-string str)
                                                              *obj-real-type*))
                                                    (subseq (str:words operands) 0 3)))
                              (slot-value obj 'normals)))

        ((string= "vt" operator)
         (vector-push-extend  (apply #'vec2 (mapcar (lambda (str)
                                                      (coerce (read-from-string str)
                                                              *obj-real-type*))
                                                    (subseq (str:words operands) 0 2)))
                              (slot-value obj 'tex-coords)))
        ((string= "vp" operator)
         (vector-push-extend  (apply #'car (mapcar (lambda (str)
                                                      (coerce (read-from-string str)
                                                              *obj-real-type*))
                                                    (str:words operands)))
                              (slot-value obj 'v-params)))))


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
        (current-object (make-instance 'obj-object :object-name "default"))
        (anon-group-count 0)
        (anon-obj-count 0))

    (flet ((ensure-group (&optional name)
             (when (null current-group)
               (setf current-group (make-instance 'obj-group :group-name (if name
                                                                             name
                                                                             (format nil "anonymous-group-~a" anon-group-count))))
               (incf anon-group-count))
             current-group)
           (ensure-object (&optional name)
             (when (null current-object)
               (setf current-object (make-instance 'obj-object :object-name (if name
                                                                                name
                                                                                (format nil "anonymous-object-~a" anon-obj-count))))
               (incf anon-obj-count))
             current-object))
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
                  (add-group (ensure-object) current-group))
                (setf current-group
                      (make-instance 'obj-group :group-name operands)))

               ((string= "s" operator)
                (when current-group
                  (with-slots (smoothing-group) (ensure-group)
                    (setf smoothing-group (read-from-string operands)))))

               ((string= "usemtl" operator)
                (when current-group
                  (with-slots (material)  (ensure-group)
                    (setf material operands))))

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
                (add-point  (ensure-object)  (ensure-group) operands))

               ;; Lines - operands are vertex and optional texture parameter indices?
               ;; l 1/1 2/2 3/3
               ((string= "l" operator)
                (add-line  (ensure-object)  (ensure-group) operands))

               ;; Faces - operands are vertex, optional texture parameter, and optional normal indices
               ;; f 1//1 2//2 3//3
               ;; f 1/1/1 2/2/2 3/3/3
               ;; f 1/1 2/2 3/3
               ((string= "f" operator)
                (add-face  (ensure-object)  (ensure-group)  operands))

               (t
                (format t "Unhandled operator ~s~%" operator))))
      (when current-group
        (add-group (ensure-object) current-group))
      (when current-object
        (push current-object all-objects))
      (when (zerop (hash-table-count materials))
        (setf (gethash "default" materials) (default-material)))
      (make-instance 'obj-file :objects all-objects :materials materials))))

;; (defun read-obj-from-file (file-name)
;;   (uiop/filesystem:with-current-directory ((directory-namestring file-name))
;;     (with-input-from-file (ins file-name)
;;       (read-obj ins))))


(defmacro with-each-object ((obj-var obj-file)
                            &body body)
  `(loop :for ,obj-var :in (objects ,obj-file)
         :do
            (progn
              ,@body)))

(defmacro with-each-group ((group-var obj-object)
                            &body body)
  `(loop :for ,group-var :across (groups ,obj-object)
         :do
            (progn
              ,@body)))

(defmacro with-each-face ((face-var group)
                            &body body)
  `(loop :for ,face-var :across (faces ,group)
         :do
            (progn
              ,@body)))

(defmacro with-each-line ((line-var group)
                          &body body)
  `(loop :for ,line-var :across (lines ,group)
         :do
            (progn
              ,@body)))

(defmacro with-each-point ((pt-var group)
                          &body body)
  `(loop :for ,pt-var :across (points ,group)
         :do
            (progn
              ,@body)))


;; (defmacro with-each-point ((index vertex normal tex vp) face-var group obj
;;                            &body body)
;;   `(loop :for ,index = (indices ,face-var)
;;          :for ,vertex = ()))


;; (defun face-data (obj-file group face))
;; (defun line-data (obj-file group line))
;; (defun point-data (obj-file group point))

;; (defun vertex (obj-file group face)
;;   (with-slots (groups vertices)) obj-file
;;   (with-slots (faces )) (aref groups group))
;; (defun normal (obj-file group face))
;; (defun tex-coord (obj-file group face))
;; (defun v-param (obj-file group face))

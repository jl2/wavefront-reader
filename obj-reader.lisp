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

(defmethod print-object ((object obj-material) stream)
  (format stream "(material ~a)" (material-name object)))

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

(defun maybe-read-string (str)
  (if (string= "" str)
      nil
      (read-from-string str nil)))

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

(defclass bounding-box ()
  ((max-corner :type vec3
               :initform (vec3 most-negative-single-float
                               most-negative-single-float
                               most-negative-single-float)
               :initarg :max-corner
               :accessor max-corner)
   (min-corner :type vec3
               :initform (vec3 most-positive-single-float
                               most-positive-single-float
                               most-positive-single-float)
               :initarg :min-corner
               :accessor min-corner)))

(defun bb-center (bb)
  (declare (type bounding-box bb))
  (with-slots (min-corner max-corner) bb
    (v* 0.5 (v+ min-corner max-corner))))

(defmethod print-object ((object bounding-box) stream)
  (with-slots (min-corner max-corner) object
    (format stream "(bounding-box ~a ~a)" min-corner max-corner)))

(defclass obj-geometry ()
  ((vertices :initarg :indices
             :accessor vertices
             :initform (make-array 0
                                   :initial-contents '()
                                   :element-type 'fixnum
                                   :adjustable t
                                   :fill-pointer 0))

   (tex-coords :initarg :tex-coords
               :accessor tex-coords
               :initform (make-array 0
                                     :initial-contents '()
                                     :element-type 'fixnum
                                     :adjustable t
                                     :fill-pointer 0)))
  (:documentation "A Wavefront OBJ face or line.
idx-format indicates which data is referenced (:vertex, :vertex-normal, :vertex-tex-normal, etc.) and determines the stride,
and indices contains the indices themselves."))

(defclass obj-face (obj-geometry)
  ((normals :initarg :normals
            :accessor normals
            :initform (make-array 0
                                  :initial-contents '()
                                  :element-type 'fixnum
                                  :adjustable t
                                  :fill-pointer 0)))
  (:documentation "A Wavefront OBJ face."))

(defclass obj-line (obj-geometry)
  ()
  (:documentation "A Wavefront OBJ line."))

(defclass obj-group ()
  ((group-name :initarg :group-name
               :accessor group-name
               :type (or null string))
   (smoothing-group :initform nil
                    :accessor smoothing-group
                    :type (or null fixnum))
   (material :initform nil
             :accessor material
             :type (or null string))

   (bounding-box :initform (make-instance 'bounding-box)
                 :type bounding-box
                 :accessor bounding-box)

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

(defclass obj-object ()
  ((object-name :initarg :object-name :type string
                :accessor object-name)
   (groups :initform (make-hash-table :test 'equal)
           :accessor groups)
   (bounding-box :initform (make-instance 'bounding-box)
                 :type bounding-box
                 :accessor bounding-box))
  (:documentation "A collection of geometry groups and vertex data (including vertices, normals, and texture parameters) that makes up an OBJ file.
Geometric groups collections of faces, lines, and points that index into the vertex data."))

(declaim (inline get-vertex get-normal get-tex-coord))
(defun get-vertex (obj-file idx)
  (aref (slot-value obj-file 'vertices) idx))
(defun get-normal (obj-file idx)
  (aref (slot-value obj-file 'normals) idx))
(defun get-tex-coord (obj-file idx)
  (aref (slot-value obj-file 'tex-coords) idx))


(defun get-vertices (obj-file geo)
  (map 'vector
       (curry #'get-vertex obj-file)
       (slot-value geo 'vertices)))

(defun get-normals (obj-file geo)
  (map 'vector
       (curry #'get-normal obj-file)
       (slot-value geo 'normals)))

(defun get-tex-coords (obj-file geo)
  (map 'vector
       (curry #'get-tex-coord obj-file)
       (slot-value geo 'tex-coords)))


(defclass obj-file ()
  ((objects :initform (make-hash-table :test 'equal)
            :type hashtable
            :accessor objects)
   (materials :initform (make-hash-table :test 'equal)
              :type hashtable
              :accessor materials)
   (path :initarg :path
         :type pathname
         :accessor path)

   (bounding-box :initform (make-instance 'bounding-box)
                 :type bounding-box
                 :accessor bounding-box)
   (vertices :initform (make-array 0
                                   :element-type '(or vec3 vec4)
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0)
             :accessor vertices
             :type (vector (or vec3 vec4)))
   (normals :initform (make-array 0
                                  :element-type 'vec3
                                  :initial-contents '()
                                  :adjustable t
                                  :fill-pointer 0)
            :accessor normals
            :type (vector vec3))
   (tex-coords :initform (make-array 0
                                     :element-type '(or real vec2 vec3)
                                     :initial-contents '()
                                     :adjustable t
                                     :fill-pointer 0)
               :accessor tex-coords
               :type (vector (or real vec2 vec3)))

   (parameters :initform (make-array 0
                                     :element-type '(or real vec2 vec3)
                                     :initial-contents '()
                                     :adjustable t
                                     :fill-pointer 0)
               :type (vector (or real vec2 vec3))
               :accessor v-params))

  (:documentation "Objects and materials from a Wavefront OBJ file."))


(defmethod print-object ((object obj-object) stream)
  (format stream "(obj-object ~a)" (object-name object)))



(defmethod print-object ((object obj-file) stream)
  (format stream "(obj-file ~a)" (path object)))


(defmethod print-object ((object obj-group) stream)
  (format stream "(group ~a)" (group-name object)))

(defmethod print-object ((object obj-face) stream)
  (format stream "(face ~a  to ~a)"
           (aref (vertices object) 0) (aref (vertices object) (1- (length (vertices object))))))

(defgeneric get-material (which obj))
(defmethod get-material ((name string) (obj obj-file))
  (with-slots (materials) obj
    (gethash name materials)))

(defmethod get-material ((group obj-group)  (obj obj-file))
  (with-slots (materials) obj
    (with-slots (material) group
      (gethash material materials))))


;; (defun has-normals (geo)
;;   (with-slots (normals) geo
;;     (not (emptyp normals))))

;; (defun has-tex-coords (geo)
;;   (with-slots (tex-coords) geo
;;     (not (emptyp tex-coords))))

(defun vertex-count (geo)
  (if (has-vertices geo)
    (length (vertices geo))
    0))

(defun normal-count (geo)
  (if (has-normals geo)
    (length (normals geo))
    0))

(defun tex-coord-count (geo)
  (if (has-tex-coords geo)
    (length (tex-coords geo))
    0))


(defun fixup-negative-index (obj-file i-type idx)
  "OBJ indices are 1 based, and allow negative values that index from the end of the array.
fixup-negative-indices converts these index values into positive, 0 based indices."

  (when idx
    (let ((count (length (slot-value obj-file i-type))))
      (+ idx
         (if (< idx 0)
             count
             -1)))))


(defun add-point (obj-file group operands)
  "Add a point to the given group."
  (declare (type string operands)
           (type obj-file obj-file)
           (type obj-group group))
  (with-slots (bounding-box points) group
    (loop :for raw-idx :in (str:words operands)
          :for fixed-idx = (fixup-negative-index obj-file 'vertices (maybe-read-string raw-idx))
          :for vert = (aref points fixed-idx)
          :do
             (expand bounding-box vert)
             (vector-push-extend fixed-idx points))))

(defun maybe-add (obj-file what value vector)
  (when-let (idx (fixup-negative-index obj-file what value))
    (vector-push-extend idx vector)))

(defun read-obj-line (obj-file operand-string)
  "Convert an input line of text into a new obj-line"

  (declare (type obj-file obj-file)
           (type string operand-string))

  (let* ((the-line (make-instance 'obj-line)))
    (with-slots (vertices tex-coords) the-line
      (loop
        :for oper :of-type string :in (str:words operand-string)
        :for i :of-type fixnum :from 0
        :for parts = (mapcar #'maybe-read-string (str:split "/" oper :omit-nulls nil))
        :do
           (ecase (length parts)
             (1
              (maybe-add obj-file 'vertices (first parts) vertices))
             (2
              (maybe-add obj-file 'vertices (first parts) vertices)
              (maybe-add obj-file 'tex-coords (second parts) vertices)))))
    the-line))

(defun add-line (obj-file group operands)
  "Add a new line to object group."
  (declare (type obj-file obj-file)
           (type string operands)
           (type obj-group group))
  (with-slots (lines bounding-box) group
    (let ((the-line (read-obj-line obj-file operands)))
      (expand bounding-box (get-vertices obj-file the-line))
      (vector-push-extend the-line lines))))



(defun read-obj-face (obj-file operand-string)
  "Read a new face whose index data refers to object."
  (declare (type obj-file obj-file)
           (type string operand-string))
  (let* ((the-face (make-instance 'obj-face)))
    (with-slots (vertices normals tex-coords) the-face
      (loop
        :for oper :of-type string :in (str:words operand-string)
        :for i :of-type fixnum :from 0
        :for parts = (mapcar #'maybe-read-string (str:split "/" oper :omit-nulls nil))
        :do
           (ecase (length parts)
             (1
              (maybe-add obj-file 'vertices (first parts) vertices)
              )
             (2
              (maybe-add obj-file 'vertices (first parts) vertices)
              (maybe-add obj-file 'tex-coords (second parts) tex-coords))
             (3
              (maybe-add obj-file 'vertices (first parts) vertices)

              (when (not (null (second parts)))
                (maybe-add obj-file 'normals (second parts) normals))
              (maybe-add obj-file 'normals (third parts) normals)))))
    the-face))

(defun add-face (obj-file group operands)
  "Read a new face from operands and add it to group."
  (declare (type obj-file obj-file)
           (type string operands)
           (type obj-group group))

  (with-slots (faces bounding-box) group
    (let ((the-face (read-obj-face obj-file operands)))
      (expand bounding-box (get-vertices obj-file the-face))
      (vector-push-extend the-face faces))))

(defun add-group (obj group)
  "Add a new group to obj."
  (declare (type obj-object obj)
           (type obj-group group))
  (with-slots (groups) obj
    (setf (gethash (group-name group) groups) group)))

(defun add-object (obj-file obj)
  "Add a new group to obj."
  (declare (type obj-file obj-file)
           (type obj-object obj))
  (with-slots (objects) obj-file
    (setf (gethash (object-name obj) objects) obj)))

(defun read-value (operands)
  (let ((numbers (mapcar #'read-from-string operands))
        (converters (make-array 4 :initial-contents (list #'identity #'vec2 #'vec3 #'vec4))))
    (when (<= 4 (length operands))
      (error "Too many values - expected 1-4! ~a" operands))
    (apply (aref converters (- (length numbers) 1)) numbers)))


(defgeneric expand (bb thing))

(defmethod expand (bb (new-pt vec3))
  (with-slots (max-corner min-corner) bb
    (setf (vx min-corner) (min (vx min-corner) (vx new-pt)))
    (setf (vx max-corner) (max (vx max-corner) (vx new-pt)))
    (setf (vy min-corner) (min (vy min-corner) (vy new-pt)))
    (setf (vy max-corner) (max (vy max-corner) (vy new-pt)))
    (setf (vz min-corner) (min (vz min-corner) (vz new-pt)))
    (setf (vz max-corner) (max (vz max-corner) (vz new-pt)))
    ))

(defmethod expand (bb (seq sequence))
  (map nil (lambda (pt) (expand bb pt)) seq))

(defun add-vertex-data (obj operator operands)
  "Add a new vertex, normal, texture coordinate or parameter to obj."
  (declare (type obj-file obj)
           (type string operator operands))
  (let ((parts (str:split " " operands)))
    (cond ((string= "v" operator)
           (with-slots (vertices bounding-box) obj
             (let ((vert (read-value parts)))
               (vector-push-extend vert vertices)
               (expand bounding-box vert))))

          ((string= "vn" operator)
           (vector-push-extend (read-value parts)
                               (slot-value obj 'normals)))

          ((string= "vt" operator)
           (vector-push-extend (read-value parts)
                               (slot-value obj 'tex-coords))))))


(defun read-obj-from-file (file-name)
  "Read Wavefront OBJ data from file-name."
  ;; Use with-current-directory so .mtl files are found in the correct location.
  (uiop/filesystem:with-current-directory ((directory-namestring file-name))
    (with-input-from-file (ins file-name)
      (read-obj ins file-name))))

(defun read-obj (ins &optional (path (pathname "object.obj")))
  "Read a WaveFront OBJ file into memory."
  (let* ((materials (make-hash-table :test 'equal))
         (current-group nil)
         (current-object nil)
         (anon-group-count 0)
         (anon-obj-count 0)

         (return-value (make-instance 'obj-file
                                      :path path)))

    (flet ((ensure-group (&optional name)
             (when (null current-group)
               (setf current-group (make-instance 'obj-group
                                                  :group-name (if name
                                                                  name
                                                                  (format nil "anonymous-group-~a" anon-group-count))))
               (incf anon-group-count))
             current-group)

           (ensure-object (&optional name)
             (when (null current-object)
               (setf current-object (make-instance 'obj-object
                                                   :object-name (if name
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
        :when (string= "mtllib" operator)
          :do
             (loop :for (material-name . material) :in (read-mtl-from-file operands) :do
               (setf (gethash material-name (materials return-value)) material))


             ;; object name - operands are words of the name
        :when (string= "o" operator)
          :do
             (when current-object
               (when current-group
                 (add-group current-object current-group))
               (add-object return-value current-object))
             (setf current-group nil)
             (setf current-object (make-instance 'obj-object :object-name operands))

        :when (string= "g" operator)
          :do
             (when current-group
               (add-group (ensure-object) current-group))
             (setf current-group (make-instance 'obj-group :group-name operands))

        :when (string= "s" operator)
          :do
             (with-slots (smoothing-group) (ensure-group)
               (setf smoothing-group operands))

        :when (string= "usemtl" operator)
          :do
             (with-slots (material) (ensure-group)
               (setf material operands))

             ;; Vertex - operands are x y z [w]
             ;; Normal - operands are i j k
             ;; Texture coordinate - operands are  u [v [w]]
             ;; Vertex parameter - operands are u [v [w]]
        :when (or (string= "v" operator)
                  (string= "vn" operator)
                  (string= "vt" operator)
                  (string= "vp" operator))
          :do
             (add-vertex-data return-value operator operands)

             ;; Points - operands are vertex indices
             ;; p 1 2 3
        :when (string= "p" operator)
          :do
             (add-point return-value (ensure-group) operands)

             ;; Lines - operands are vertex and optional texture parameter indices?
             ;; l 1/1 2/2 3/3
        :when (string= "l" operator)
          :do
             (add-line return-value (ensure-group) operands)

             ;; Faces - operands are vertex, optional texture parameter, and optional normal indices
             ;; f 1//1 2//2 3//3
             ;; f 1/1/1 2/2/2 3/3/3
             ;; f 1/1 2/2 3/3
        :when (string= "f" operator)
          :do
             (add-face return-value (ensure-group) operands))

      (when current-group
        (add-group (ensure-object) current-group))
      (when current-object
        (add-object return-value current-object))
      (when (zerop (hash-table-count materials))
        (setf (gethash "default" materials) (default-material)))
      return-value)))

(defmacro with-each-object ((obj-var obj-file)
                            &body body)
  `(loop :for ,obj-var
           :being :the hash-values
             :of (objects ,obj-file)
         :do
            (progn
              ,@body)))

(defmacro with-each-group ((group-var obj-object)
                            &body body)
  `(loop :for ,group-var
           :being :the hash-values
             :of (groups ,obj-object)
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

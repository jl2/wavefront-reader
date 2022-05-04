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

(defclass obj-geometry ()
  ((idx-format :initarg :idx-format)
   (indices :initarg :indices
            :initform (make-array 0
                                  :initial-contents '()
                                  :element-type 'fixnum
                                  :adjustable t
                                  :fill-pointer 0)))
  (:documentation "A Wavefront OBJ face."))

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


(defclass obj-group ()
  ((group-name :initarg :group-name :type (or null string))
   (smoothing-group :initform nil :type (or null fixnum))
   (material :initform nil :type (or null string))
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

  (:documentation "An object in a WaveFront OBJ file."))

(defclass obj-file ()
  ((objects :initarg :objects)
   (materials :initarg :materials))
  (:documentation "Objects and materials from a Wavefront OBJ file."))

(defun add-point (object group operands)
  (dolist (idx operands)
    ;; Points are indexed by single integers
    (with-slots (points) group
      (vector-push-extend
       (map-index object 'vertices (the fixnum (read-from-string idx)))
       points))))

(defun map-index (file type idx)
  (cond ((< 0 idx)
         (1- idx))
        ((> 0 idx)
         (+ (length (slot-value file type)) idx))
        (t
         (error 'invalid-obj-index :index idx))))

(defun slash-p (char)
 (char= #\/ char))

(defun read-obj-line (object operands)
  (when operands
    (let* ((first-index (str:split "/" (car operands) :omit-nulls nil))
           (index-len (length first-index))
           (expected-indices (length first-index))
           (indices (make-array (* (length operands) (length first-index))
                                :element-type 'fixnum))
           (idx-format nil))
      (cond ((= 1 index-len)
             (setf idx-format :vertex))
            ((and (= 2 index-len)
                  (cadr first-index)
                  (> 0 (length (cadr first-index))))
             (setf idx-format :vertex-texture))
            ((and (= 2 index-len)
                  (cadr first-index)
                  (= 0 (length (cadr first-index))))
             (setf idx-format :vertex)))
      (loop
         :for i = 0 :then (+ i len-parts)
         :for oper :in operands
         :for parts = (str:split "/" oper :omit-nulls nil)
         :for len-parts = (length parts)
         :do
           (cond ((/= len-parts expected-indices)
                  (error 'invalid-line-index :index oper))

                 ((or (= 1 len-parts)
                      (= 2 len-parts))
                  (loop
                     :for offset :from 0
                     :for idx :in parts
                     :do (setf (aref indices (+ offset i))
                              (map-index object 'vertices (the fixnum (read-from-string idx))))))
                 (t
                  (error 'invalid-line-index :index oper))))
      (make-instance 'obj-line :idx-format idx-format :indices indices))))

(defun add-line (object group operands)
  (with-slots (lines) group
    (vector-push-extend (read-obj-line object operands) lines)))


(defun decide-format (first-entry)
  (let ((has-vert (not (string= "" (car first-entry))))
        (has-text (not (string= "" (cadr first-entry))))
        (has-norm (not (string= "" (caddr first-entry)))))
    (cond ((and has-vert has-text has-norm)
           :vertex-texture-normal)
          ((and has-vert has-text)
           :vertex-texture)
          ((and has-vert has-norm)
           :vertex-normal)
          ((and has-vert)
           :vertex))))

(defun format-stride (format)
  (assoc-value '((:vertex-texture-normal . 3)
                 (:vertex-texture . 2)
                 (:vertex-normal . 2)
                 (:vertex . 1))
               format))

(defun read-obj-face (object operands)
  (when operands
    (let* ((first-index (str:split "/" (car operands) :omit-nulls nil))
           (idx-format (decide-format first-index))
           (stride (format-stride idx-format))
           (indices (make-array (* (length operands) stride)
                                :element-type 'fixnum)))
      (loop
         :for i = 0 :then (+ i len-parts)
         :for oper :in operands
         :for parts = (str:split "/" oper :omit-nulls t)
         :for len-parts = (length parts)
         do
           (cond ((/= len-parts stride)
                  (error 'invalid-face-index :index oper
                         :stride stride
                         :part-count len-parts))
                 ((or (= 1 len-parts)
                      (= 2 len-parts)
                      (= 3 len-parts))
                  (loop
                     :for offset :from 0
                     :for idx :in parts
                     :do (setf (aref indices (+ offset i))
                              (map-index object 'vertices (the fixnum (read-from-string idx))))))
                 ;; (t
                 ;;  (error 'invalid-line-index :index oper))
                 ))
      (make-instance 'obj-face :idx-format idx-format :indices indices))))

(defun add-face (object group operands)
  (with-slots (faces) group
    (vector-push-extend (read-obj-face object operands) faces)))

(defclass obj-object ()
  ((object-name :initarg :object-name :type (or null string))
   (vertices :initform (make-array 0
                                   :element-type 'single-float
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (normals :initform (make-array 0
                                  :element-type 'single-float
                                  :initial-contents '()
                                  :adjustable t
                                  :fill-pointer 0))
   (tex-coords :initform (make-array 0
                                     :element-type 'single-float
                                     :initial-contents '()
                                     :adjustable t
                                     :fill-pointer 0))
   (v-params :initform (make-array 0
                                   :element-type 'single-float
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (groups :initform (make-array 0
                                   :element-type 'obj-group
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0)))
  (:documentation "A WaveFront OBJ file."))

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


(define-condition invalid-obj-index (error)
  ((index :initarg :index :reader index)))

(define-condition invalid-line-index (error)
  ((index :initarg :index :reader index)))

(define-condition invalid-face-index (error)
  ((index :initarg :index :reader index)
   (part-count :initarg :part-count :reader part-count)
   (stride :initarg :stride :reader stride)))


(defun read-obj (ins)
  "Read a WaveFront OBJ file into memory."
  (let ((all-objects nil)
        (materials (make-hash-table :test 'equal))
        (current-group nil)
        (current-object nil))
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
              (when current-object
                (push current-object all-objects))
              (setf current-object
                    (make-instance 'obj-object
                                   :object-name (format nil "~{~a~^ ~}" operands))))

             ;; Group - operands are group names
             ((string= "g" operator)
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
              (add-data current-object operator operands))

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

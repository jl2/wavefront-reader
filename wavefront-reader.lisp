;;;; wavefront-reader.lisp 
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

(defclass wavefront-geometry ()
  ((format :initarg :format)
   (indices :initarg :indices
            :initform (make-array 0
                                  :initial-contents '()
                                  :element-type 'fixnum
                                  :adjustable t
                                  :fill-pointer 0)))
  (:documentation "A Wavefront OBJ face."))

;; Face format is one of: 
;; :vertex-only - 1 or 1/ or 1//
;; :vertex-normal - 1//1
;; :vertex-tex - 1/1 or 1/1/
;; :vertex-tex-normal - 1/1/1 
(defclass wavefront-face (wavefront-geometry)
  ()
  (:documentation "A Wavefront OBJ face."))

;; Line format is:
;; :vertex-only - 1 or 1/
;; :vertex-tex - 1/1
(defclass wavefront-line (wavefront-geometry)
  ()
  (:documentation "A Wavefront OBJ line."))


(defclass wavefront-group ()
  ((group-name :initarg :group-name :type (or null string))
   (faces :initform (make-array 0
                                :element-type 'wavefront-face
                                :initial-contents '()
                                :adjustable t
                                :fill-pointer 0))
   (lines :initform (make-array 0
                                :initial-contents '()
                                :element-type 'wavefront-line
                                :adjustable t
                                :fill-pointer 0))
   (points :initform (make-array 0
                                 :element-type 'fixnum
                                 :initial-contents '()
                                 :adjustable t
                                 :fill-pointer 0)))

  (:documentation "An object in a WaveFront OBJ file."))

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
           (expected-indices (length first-index))
           (indices (make-array (* (length operands) (length first-index))
                                :element-type 'fixnum)))
      (loop
         for i = 0 then (+ i len-parts)
         for oper in operands
         for parts = (str:split "/" oper :omit-nulls nil)
         for len-parts = (length parts)
         do
           (cond ((/= len-parts expected-indices)
                  (error 'invalid-line-index :index oper))

                 ((or (= 1 len-parts)
                      (= 2 len-parts))
                  (loop
                     for offset from 0
                     for idx in parts
                     do (setf (aref indices (+ offset i))
                              (map-index object 'vertices (the fixnum (read-from-string idx))))))
                 (t
                  (error 'invalid-line-index :index oper))))
      indices)))

(defun add-line (object group operands)
  (with-slots (lines) group
    (vector-push-extend (read-obj-line object operands) lines)))


(defun read-obj-face (object operands)
  (when operands
    (let* ((first-index (str:split "/" (car operands) :omit-nulls nil))
           (index-len (length first-index))
           ;; (ignore-it (progn
           ;;              (format t "first-index: ~a, index-len ~a~%" first-index index-len)
           ;;              (format t "(cadr first-index) ~a~%" (cadr first-index))
           ;;              (format t "(length (cadr first-index)) ~a~%" (length (cadr first-index)))
           ;;              (format t "(caddr first-index) ~a~%" (caddr first-index))
           ;;              (format t "(length (caddr first-index)) ~a~%" (length (caddr first-index)))))
           (stride (cond ((= 1 index-len)
                          1)
                         
                         ((and (= 2 index-len)
                               (cadr first-index)
                               (> 0 (length (cadr first-index))))
                          2)
                         ((and (= 2 index-len)
                               (cadr first-index)
                               (= 0 (length (cadr first-index))))
                          1)
                         ((and (= 3 index-len)
                               (cadr first-index)
                               (= 0 (length (cadr first-index)))
                               (caddr first-index)
                               (= 0 (length (caddr first-index))))
                          1)
                         ((and (= 3 index-len)
                               (cadr first-index)
                               (< 0 (length (cadr first-index)))
                               (caddr first-index)
                               (= 0 (length (caddr first-index))))
                          2)
                         ((and (= 3 index-len)
                               (cadr first-index)
                               (= 0 (length (cadr first-index)))
                               (caddr first-index)
                               (< 0 (length (caddr first-index))))
                          2)
                         ((and (= 3 index-len)
                               (cadr first-index)
                               (< 0 (length (cadr first-index)))
                               (caddr first-index)
                               (< 0 (length (caddr first-index))))
                          3)
                         (t
                          (format t "Unknown stride!~%")
                          3)
                         ))
           (indices (make-array (* (length operands) stride)
                                :element-type 'fixnum)))
      (loop
         for i = 0 then (+ i len-parts)
         for oper in operands
         for parts = (str:split "/" oper :omit-nulls t)
         for len-parts = (length parts)
         do
           (cond ((/= len-parts stride)
                  (error 'invalid-face-index :index oper
                         :stride stride
                         :part-count len-parts))
                 ((or (= 1 len-parts)
                      (= 2 len-parts)
                      (= 3 len-parts))
                  (loop
                     for offset from 0
                     for idx in parts
                     do (setf (aref indices (+ offset i))
                              (map-index object 'vertices (the fixnum (read-from-string idx))))))
                 (t
                  (error 'invalid-line-index :index oper))))
      indices)))

(defun add-face (object group operands)
  (with-slots (faces) group
    (vector-push-extend (read-obj-face object operands) faces)))

(defclass wavefront-object ()
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
                                   :element-type 'wavefront-group
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
        (current-group nil)
        (current-object nil))
    (loop
       for line = (read-line ins nil)
       while line
       do
         (let* ((parts (cl-ppcre:split "\\s" line))
                (operator (car parts))
                (operands (cdr parts)))
           ;; (format t "~a~%" operator)
           (cond
             ;; Comment
             ((char= #\# (aref operator 0))
              t)

             ;; Group - operands are group names
             ((string= "g" operator)
              (when current-group
                (add-group current-object current-group))
              (setf current-group (make-instance 'wavefront-group :group-name (format nil "~{~a~^ ~}" operands))))

             ;; object name - operands are words of the name
             ((string= "o" operator)
              (when current-object
                (push current-object all-objects))
              (setf current-object (make-instance 'wavefront-object :object-name (format nil "~{~a~^ ~}" operands))))

             ;; Vertex - operands are x y z [w]
             ;; Vertex - operands are  i j k
             ;; Texture coordinate - operands are  u [v [w]]
             ;; Vertex parameter - operands are u [v [w]]
             ((or (string= "v" operator)
                  (string= "vn" operator)
                  (string= "vt" operator)
                  (string= "vp" operator))
              (add-data current-object operator operands))
              ;; Use operator to look up correct slot name


             ;; Points - operands are vertex indices
             ;; p 1 2 3
             ((string= "p" operator)
              (add-point current-object current-group operands))

             ;; Lines - operands are vertex and optional texture parameter indices
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
    all-objects))

(defun read-obj-from-file (file-name)
  (with-input-from-file (ins file-name)
    (read-obj ins)))


(defun to-open-gl (obj format)
  "Return arrays of vertex and index data, suitable for rendering with OpenGL."
  (declare (ignorable obj format)))


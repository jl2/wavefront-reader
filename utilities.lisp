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

(defun expand (bb new-pt)
  (with-slots (max-corner min-corner) bb
    (setf (vx min-corner) (min (vx min-corner) (vx new-pt)))
    (setf (vx max-corner) (max (vx max-corner) (vx new-pt)))
    (setf (vy min-corner) (min (vy min-corner) (vy new-pt)))
    (setf (vy max-corner) (max (vy max-corner) (vy new-pt)))
    (setf (vz min-corner) (min (vz min-corner) (vz new-pt)))
    (setf (vz max-corner) (max (vz max-corner) (vz new-pt)))
    ))

(defun bounding-box (obj)
  (declare (type obj-file obj)
           (optimize (speed 3)))
  (let ((bb (make-instance 'bounding-box)))
    (with-slots ((vx min-corner) min-y min-z max-x max-y max-z) bb
      (loop
        :for pt :of-type (or vec3 vec4) :across (vertices obj)
        :do
           (expand bb pt)))
    bb))

(defun get-vertex (obj-file idx)
  (aref (slot-value obj-file 'vertices) idx))
(defun get-normal (obj-file idx)
  (aref (slot-value obj-file 'normals) idx))
(defun get-tex-coord (obj-file idx)
  (aref (slot-value obj-file 'tex-coords) idx))


(defun get-vertices (obj-file face)
  (map 'vector
       (curry #'get-vertex obj-file)
       (slot-value face 'vertices)))
(defun get-normals (obj-file face)
  (map 'vector
       (curry #'get-normal obj-file)
       (slot-value face 'normals))
  )
(defun get-tex-coords (obj-file face)
  (map 'vector
       (curry #'get-tex-coord obj-file)
       (slot-value face 'tex-coords)))




(defclass geometry ()
  ((vertices :initarg :vertices
             :accessor vertices
             :type (vector vec3))
   (normals :initarg :normals
            :accessor normals
            :type (vector vec3))
   (tex-coords :initarg :tex-coords
               :accessor tex-coords
               :type (vector vec2))
   (material :initarg :material
             :accessor material
             :type obj-material)))

(defun has-vertices (triangle)
  (and (slot-boundp triangle 'vertices)
       (not (emptyp (slot-value triangle 'vertices)))))

(defun has-normals (triangle)
  (and (slot-boundp triangle 'normals)
       (not (emptyp (slot-value triangle 'normals)))))

(defun has-tex-coords (triangle)
  (and (slot-boundp triangle 'tex-coords)
       (not (emptyp (slot-value triangle 'tex-coords)))))
(defun has-material (triangle)
  (slot-boundp triangle 'material))


(defun map-geometry (obj-file handler)
  "Triangulate obj-file and call handler for each triangle."
  (with-each-object (obj obj-file)
    (with-slots (vertices normals tex-coords v-params) obj
      (with-each-group (group obj)
        (let ((the-material (if (material group)
                                (get-material (material group) obj-file)
                                (get-material "default" obj-file))))

          (with-each-face (face group)
            (funcall handler (make-instance 'geometry
                                     :material the-material
                                     :vertices (get-vertices obj-file face)
                                     :normals (get-normals obj-file face)
                                     :tex-coords (get-tex-coords obj-file face)))))))))


(defun estimate-triangle-count (obj-file)
  (let ((estimate 0))
    (with-each-object (obj obj-file)
      (with-each-group (group obj)
        (incf estimate (length (faces group)))))
    estimate))

(defun estimate-line-count (obj-file)
  (let ((estimate 0))
    (with-each-object (obj obj-file)
      (with-each-group (group obj)
        (incf estimate (length (lines group)))))
    estimate))

(defun estimate-point-count (obj-file)
  (let ((estimate 0))
    (with-each-object (obj obj-file)
      (with-each-group (group obj)
        (incf estimate (length (points group)))))
    estimate))


(defun collect-geometry (obj-file)
  "Triangulate obj-file and call handler for each triangle."
  (let* ((estimated-size (+ (estimate-triangle-count obj-file)
                            (estimate-line-count obj-file)
                            (estimate-point-count obj-file)))
         (triangles
           (make-array estimated-size
                       :initial-element nil
                       :adjustable t
                       :fill-pointer 0
                       :element-type '(or null geometry))))
    (with-each-object (obj obj-file)
      (with-slots (vertices normals tex-coords v-params) obj
        (with-each-group (group obj)
          (format t "Processing group: ~a~%" (group-name group))
          (let ((the-material (if (material group)
                                  (get-material (material group) obj-file)
                                  (get-material "default" obj-file))))

            (with-each-face (face group)
              (vector-push-extend (make-instance 'geometry
                                                 :material the-material
                                                 :vertices (get-vertices obj-file face)
                                                 :normals (get-normals obj-file face)
                                                 :tex-coords (get-tex-coords obj-file face))
                                  triangles))
            (with-each-line (line group)
              (vector-push-extend (make-instance 'geometry
                                                 :material the-material
                                                 :vertices (get-vertices obj-file line)
                                                 :tex-coords (get-tex-coords obj-file line))
                                  triangles))
            (with-each-point (point group)
              (vector-push-extend (make-instance 'geometry
                                                 :material the-material
                                                 :vertices (get-vertices obj-file point))
                                  triangles))))))
    triangles))

(defun sort-geometry (triangles point &key (predicate #'>))
  "Sort triangles by their distance from the specified point."
  (sort triangles predicate
        :key (lambda (val)
               (vdistance (v* (/ 1.0 (length (vertices val))) (apply #'v+ (coerce (vertices val) 'list)))
                          point))))

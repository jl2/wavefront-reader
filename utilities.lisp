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

(defun compute-bounding-box (obj)
  (declare (type obj-file obj)
           (optimize (speed 3)))
  (let ((bb (make-instance 'bounding-box)))
    (with-slots ((vx min-corner) min-y min-z max-x max-y max-z) bb
      (loop
        :for pt :of-type (or vec3 vec4) :across (vertices obj)
        :do
           (expand bb pt)))
    bb))


(defclass geometry ()
  ((geo-type :initarg :geo-type :accessor geo-type
             :type (or :face :line :points))
   (vertices :initarg :vertices
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
                                                 :geo-type :face
                                                 :material the-material
                                                 :vertices (get-vertices obj-file face)
                                                 :normals (get-normals obj-file face)
                                                 :tex-coords (get-tex-coords obj-file face))
                                  triangles))
            (with-each-line (line group)
              (vector-push-extend (make-instance 'geometry
                                                 :geo-type :line
                                                 :material the-material
                                                 :vertices (get-vertices obj-file line)
                                                 :tex-coords (get-tex-coords obj-file line))
                                  triangles))
            (with-each-point (point group)
              (vector-push-extend (make-instance 'geometry
                                                 :geo-type :points
                                                 :material the-material
                                                 :vertices (get-vertices obj-file point))
                                  triangles))))))
    triangles))

(defun center-point (verts)
  (v* (/ 1.0 (length verts))
      (apply #'v+ (coerce verts 'list))))

(defun sort-geometry (triangles point &key (predicate #'>))
  "Sort triangles by their distance from the specified point."
  (sort triangles predicate
        :key (lambda (val)
               (vdistance (center-point (vertices val))
                          point))))

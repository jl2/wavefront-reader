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
               :initform (vec3 most-negative-single-float most-negative-single-float most-negative-single-float)
               :initarg :max-corner
               :accessor max-corner)
   (min-corner :type vec3
               :initform (vec3 most-positive-single-float most-positive-single-float most-positive-single-float)
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
        :for obj :of-type obj-object :in (slot-value obj 'objects)
        :do
           (loop
             :for vert :of-type vec3 :across (vertices obj)
             :do (expand bb vert))))
    bb))


(defclass geometry ()

  ((vertices :initarg :vertices
          :accessor vertices
          :type (vector vec3 3))
   (normals :initarg :normals
            :accessor normals
            :type (vector vec3 3))
   (tex-coords :initarg :tex-coords
               :accessor tex-coords
               :type (vector vec2 3))
   (vdata :initarg :vdata
             :accessor vdata
             :type (vector double-float 3))
   (material :initarg :material
             :accessor material
             :type obj-material)))

(defun has-vertices (triangle)
  (and (slot-boundp triangle 'vertices)
       (not (zerop (length (slot-value triangle 'vertices))))))

(defun has-normals (triangle)
  (and (slot-boundp triangle 'normals)
       (not (zerop (length (slot-value triangle 'normals))))))

(defun has-tex-coords (triangle)
  (and (slot-boundp triangle 'tex-coords)
       (not (zerop (length (slot-value triangle 'tex-coords))))))
(defun has-vdata (triangle)
  (and (slot-boundp triangle 'vdata)
       (not (zerop (length (slot-value triangle 'vdata))))))
(defun has-material (triangle)
  (slot-boundp triangle 'material))


(defun map-geometry (obj-file handler)
  "Triangulate obj-file and call handler for each triangle."
  (with-each-object (obj obj-file)
    (with-slots (vertices normals tex-coords v-params) obj
      (with-each-group (group obj)
        (let ((the-material (if (material group)
                                (get-material (material group) obj-file)
                                (get-material "default" obj-file)))
              )

          (with-each-face (face group)
            (let* ((idx-format (idx-format face))
                   (vi (vertex-index idx-format))
                   (ni (normal-index idx-format))
                   (ti (tex-index idx-format))
                   (vdi (vdata-index idx-format)))
              (with-slots (indices) face
                (multiple-value-bind (these-verts these-normals these-texs these-vdatas)
                    (loop :with these-verts = (when vi (make-array (length indices) :initial-element (vec3 0 0 0) :element-type 'vec3))
                          :with these-norms = (when ni (make-array (length indices) :initial-element (vec3 0 0 0) :element-type 'vec3))
                          :with these-texs = (when ti (make-array (length indices) :initial-element (vec2 0 0) :element-type 'vec2))
                          :with these-vdatas = (when vdi (make-array (length indices) :element-type 'single-float))
                          :for i :from 0
                          :for idx :across indices
                          :when vi
                            :do
                               (setf  (aref these-verts i)
                                      (aref vertices (aref idx vi)))
                          :when ni
                            :do
                               (setf (aref these-norms i)
                                     (aref normals (aref idx  ni)))
                          :when ti
                            :do
                               (setf (aref these-texs i)
                                     (aref tex-coords (aref idx ti)))
                          :when vdi
                            :do
                               (setf (aref these-vdatas i)
                                     (aref v-params (aref idx vdi)))
                          :finally (return (values these-verts
                                                   these-norms
                                                   these-texs
                                                   these-vdatas)))

                  (funcall handler (make-instance 'geometry
                                                  :material the-material
                                                  :vertices these-verts
                                                  :normals these-normals
                                                  :tex-coords these-texs
                                                  :vdata these-vdatas)))))))))))


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


(defun collect-geometry (obj-file)
  "Triangulate obj-file and call handler for each triangle."
  (let ((triangles
          (make-array (estimate-triangle-count obj-file) :initial-element nil :adjustable t :fill-pointer 0 :element-type '(or null geometry))))
    (with-each-object (obj obj-file)
      (with-slots (vertices normals tex-coords v-params) obj
        (with-each-group (group obj)
          (let ((the-material (if (material group)
                                  (get-material (material group) obj-file)
                                  (get-material "default" obj-file))))

            (with-each-face (face group)
              (let* ((idx-format (idx-format face))
                     (vi (vertex-index idx-format))
                     (ni (normal-index idx-format))
                     (ti (tex-index idx-format))
                     (vdi (vdata-index idx-format)))
                (with-slots (indices) face
                  (multiple-value-bind (these-verts these-normals these-texs these-vdatas)
                      (loop :with these-verts = (when vi (make-array (length indices) :initial-element (vec3 0 0 0) :element-type 'vec3))
                            :with these-norms = (when ni (make-array (length indices) :initial-element (vec3 0 0 0) :element-type 'vec3))
                            :with these-texs = (when ti (make-array (length indices) :initial-element (vec2 0 0) :element-type 'vec2))
                            :with these-vdatas = (when vdi (make-array (length indices) :element-type 'single-float))
                            :for i :from 0
                            :for idx :across indices
                            :when vi
                              :do
                                 (setf  (aref these-verts i)
                                        (aref vertices (aref idx vi)))
                            :when ni
                              :do
                                 (setf (aref these-norms i)
                                       (aref normals (aref idx  ni)))
                            :when ti
                              :do
                                 (setf (aref these-texs i)
                                       (aref tex-coords (aref idx ti)))
                            :when vdi
                              :do
                                 (setf (aref these-vdatas i)
                                       (aref v-params (aref idx vdi)))
                            :finally (return (values these-verts
                                                     these-norms
                                                     these-texs
                                                     these-vdatas)))

                    (vector-push-extend (make-instance 'geometry
                                                       :material the-material
                                                       :vertices these-verts
                                                       :normals these-normals
                                                       :tex-coords these-texs
                                                       :vdata these-vdatas)
                                        triangles)))))))))
    triangles))

(defun sort-geometry (triangles point &key (predicate #'<))
  "Sort triangles by their distance from the specified point."
  (sort triangles predicate
        :key (lambda (val)
               (vdistance (v* (/ 1.0 3.0) (apply #'v+ (coerce (vertices val) 'list)))
                          point))))

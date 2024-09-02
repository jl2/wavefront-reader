;; package.lisp
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

(defpackage :obj-reader
  (:use #:cl #:alexandria)
  (:nicknames #:obj)
  (:export #:*obj-real-type*

           #:read-obj
           #:read-mtl
           #:read-obj-from-file
           #:read-mtl-from-file

           #:obj-file

           #:objects
           #:materials

           
           #:obj-object
           
           #:object-name
           #:vertices
           #:normals
           #:tex-coords
           #:v-params
           #:groups

           #:obj-group
           #:group-name
           #:smoothing-group
           #:material
           #:attributes
           #:faces
           #:lines
           #:points

           #:obj-geometry
           #:idx-format
           #:indices
           #:format-stride
           #:stride
           #:obj-face
           #:obj-line
           #:format-stride
           #:bounding-box
           #:min-x
           #:min-y
           #:min-z
           #:max-x
           #:max-y
           #:max-z
           ))

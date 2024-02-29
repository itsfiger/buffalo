;; Copyright (c) 2015 by Frank Huttner

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; after ideas from http://research.swtch.com/sparse by Russ Cox

(defpackage :SparseSet
  (:use :cl)
  (:export #:sparseset #:make-SparseSet #:spinit #:spin-p #:spadd #:spdel
	   #:spreset #:spiter #:spcompress #:spcompress! #:sp-len #:sp-len-tot
	   #:spref))

(in-package :SparseSet)

(declaim (optimize (speed 3) (space 0) (debug 0))
	 (inline spin-p spadd spdel spref))

(defstruct SparseSet
  (n 0 :type fixnum)
  (w (make-array 0 :element-type 'fixnum :initial-element 0) :type (simple-array fixnum (*)))
  (stack (make-array 0 :element-type 'fixnum :initial-element 0) :type (simple-array fixnum (*))))

(defun spinit (n)
  "Create the sparse and dense working sets. Required before any use."
  (declare (type fixnum n))
  (let ((sparseset (make-sparseset)))
    (setf (sparseset-w sparseset) (make-array n :element-type 'fixnum :initial-element 0)
	  (sparseset-stack sparseset) (make-array n :element-type 'fixnum :initial-element 0))
    sparseset))

(defun spin-p (sparseset i)
  "True if i is in collection, false otherwise."
  (declare (type sparseset sparseset)
	   (type fixnum i))
  (let ((n (sparseset-n sparseset))
	(w (sparseset-w sparseset))
	(stack (sparseset-stack sparseset)))
    (and
     (< (aref w i) n)
     (= (aref stack (aref w i)) i))))

(defun spadd (sparseset i)
  "Add i to collection. Returns nil if i is already present, otherwise i."
  (declare (type sparseset sparseset)
	   (type fixnum i))
  (let ((n (sparseset-n sparseset))
	(w (sparseset-w sparseset))
	(stack (sparseset-stack sparseset)))
    (unless (spin-p sparseset i)
      (setf (aref stack n) i
	    (aref w i) n)
;      (incf (sparseset-n sparseset))
      (setf (sparseset-n sparseset) (logand (1+ (sparseset-n sparseset)) most-positive-fixnum))
      i)))

(defun spdel (sparseset i)
  "Removes i from the collection. Returns nil if i is not present, otherwise i."
  (declare (type sparseset sparseset)
	   (type fixnum i))
  (when (spin-p sparseset i)
;    (let* ((n (decf (sparseset-n sparseset)))
    (let* ((n (setf (sparseset-n sparseset) (logand (1- (sparseset-n sparseset)) most-positive-fixnum)))
	   (w (sparseset-w sparseset))
	   (stack (sparseset-stack sparseset))
	   (di (aref w i)))
      (setf (aref stack di) (aref stack n)
	    (aref w (aref stack n)) di)
      i)))

(defun spreset (sparseset)
  "Reset the collection to zero elements."
  (declare (type sparseset sparseset))
  (setf (sparseset-n sparseset) 0))

(defun spref (sparseset i)
  "Return the i-th elment of the collection"
  (declare (type sparseset sparseset)
	   (type fixnum i))
  (unless (< i (sparseset-n sparseset))
    (error "Element ~d is outside collection of size ~d" i (sparseset-n sparseset)))
  (aref (sparseset-stack sparseset) i))

(defun spiter (sparseset)
  "Returns and 'array for iteration', i.e. a displaced-to array to the internal stack."
  (declare (type sparseset sparseset))
  (let ((n (sparseset-n sparseset))
	(stack (sparseset-stack sparseset)))
    (make-array n
		:element-type 'fixnum
		:displaced-to stack)))

(defun spcompress (sparseset)
  "Returns a copy of the internal stack."
  (declare (type sparseset sparseset))
  (let ((n (sparseset-n sparseset))
	(stack (sparseset-stack sparseset)))
    (map-into
	    (make-array n :element-type 'fixnum)
	    #'identity
	    stack)))

(defun spcompress! (sparseset)
  "Returns a copy of the internal stack and resets the collection."
  (declare (type sparseset sparseset))
  (prog1 (spcompress sparseset)
    (spreset sparseset)))

(defun sp-len (sparseset)
  "Returns the length of the internal stack."
  (declare (type sparseset sparseset))
  (sparseset-n sparseset))

(defun sp-len-tot (sparseset)
  "Returns the maximum length of the set."
  (declare (type sparseset sparseset))
  (length (sparseset-w sparseset)))

(defun test ()
  (declare (optimize (speed 0) (space 0) (debug 3)))
  (let ((s (spinit 10)))
    (mapc
     (lambda (x) (spadd s x)) '(0 5 1 2 9 2 1))
    (mapc
     (lambda (x) (spdel s x)) '(2 6 3))
    (print (spiter s))
    (print (spcompress s))
    (print (spin-p s 5))
    (print (spiter s))
    (spin-p s 3)
    (spreset s)
    (mapc
     (lambda (x) (spadd s x)) '(2 1))
    (loop for v across (spiter s) do (print v))))

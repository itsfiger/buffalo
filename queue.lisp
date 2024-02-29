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


(defpackage :queue
  (:use :cl)
  (:export #:queue #:make-queue #:qu-init #:qu-in-p #:qu-push #:qu-pop))

(in-package :queue)

(declaim (optimize (speed 3) (space 0) (debug 0))
 	 (inline spin-p spadd spdel))

(defstruct queue
  (in 0 :type fixnum)
  (out 0 :type fixnum)
  (stack (make-array 0 :element-type 'fixnum :initial-element 0) :type (simple-array fixnum (*))))

(defun qu-init (n)
  "Create the working set. Required before any use."
  (declare (type fixnum n))
  (let ((queue (make-queue)))
    (setf (queue-stack queue) (make-array n :element-type 'fixnum :initial-element 0))
    queue))

(defun qu-in-p (queue)
  "True if there are more elements in the queue, false otherwise."
  (declare (type queue queue))
  (/= (queue-out queue) (queue-in queue)))

(defun qu-push (queue i)
  "Add i to collection."
  (declare (type queue queue)
	   (type fixnum i))
  (let ((in (queue-in queue))
	(stack (queue-stack queue)))
    (setf (aref stack in) i)
    (when (=
	   (incf (queue-in queue))
	   (length stack))
      (setf (queue-in queue) 0))
    i))

(defun qu-pop (queue)
  "Pops one element from the collection."
  (declare (type queue queue))
  (let ((out (queue-out queue))
	(stack (queue-stack queue)))
    (prog1 (aref stack out)
      (when (=
	     (incf (queue-out queue))
	     (length stack))
	(setf (queue-out queue) 0)))))


(defun test ()
  (let ((q (qu-init 10)))
    (mapc
     (lambda (x) (qu-push q x)) '(1 3 5))
    (mapc
     (lambda (x) (qu-push q x)) '(0 2 4 6 8))
    (loop repeat 5
       while (qu-in-p q) do
	 (print (qu-pop q)))
    (print "!")
    (mapc
     (lambda (x) (qu-push q x)) '(-1 -2 -3 -4 -5 -6))
    (loop
       while (qu-in-p q) do
	 (print (qu-pop q)))))

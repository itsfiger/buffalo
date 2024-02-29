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

(defpackage #:bisect
  (:use #:common-lisp)
  (:export #:create-trie #:lookup #:insert))

(in-package #:bisect)


(defstruct (item
	     (:constructor make-item (key val)))
  (key 0 :type fixnum :read-only t)
  (val -1 :type t :read-only t))


(defun create-trie (n)
  (make-array n :initial-element (make-item -1 nil) :element-type 'item :fill-pointer 0))


(defun lookup (key trie)
  (declare (type fixnum key)
	   (type (array item *) trie)
	   (optimize (speed 3) (space 0) (debug 0)))
  (let ((start 0)
	(end (1- (length trie))))
    (declare (type fixnum start end))
    (loop
       for i = (+ start (ash (- end start) -1))
       for k = (item-key (aref trie i))
       if (> start end) do (error "Not found.")
       until (= k key)
       if (< k key) do
	 (setf start (incf i))
       else do
	 (setf end (decf i))
       finally (return (item-val (aref trie i))))))


(defun insert (key val trie)
  (declare (type fixnum key)
	   (type (array item *) trie))
  (vector-push (make-item key val) trie))




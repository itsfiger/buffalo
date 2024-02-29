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


;;; implements concepts from the book Algorithms in C++ by Roberte Sedgewick
;;; but not on a bit vector but on a bit vector represented as index vector,
;;; i.e. as indices of '1's
;;; The trie holds its own SparseSet as working set while the keys are stored
;;; as supplied (fixnum arrays). The SparseSet is one element larger than requested
;;; so that a 'guard' bit can be used as nth element (to represent zero vectors
;;; if they occur)
;;; The indices in the supplied vector can be in any order.
;;; The test bits are also unordered.

(defpackage #:patricia
  (:use #:common-lisp #:sparseset)
  (:export #:create-trie #:lookup #:insert #:show))

(in-package #:patricia)


(defstruct (item
	     (:constructor make-item (key val)))
  (key #() :type (simple-array *) :read-only t)
  (val -1 :type t :read-only t))

(defstruct (node (:print-function print-node))
  (bit -1 :type fixnum)
  (l nil :type t)
  (r nil :type t))

(defun print-node (k s d)
  (declare (type node k) (stream s) (ignore d))
  (print-unreadable-object (k s :type t)
    (format s "bit=~a left=~a right=~a" (node-bit k) (node-l k) (node-r k))))


(defstruct (trie)
  (head (make-node) :type node)
  sp)


(defun create-trie (n)
  "The main entrance point to creating the trie."
  (let ((trie (make-trie :head (make-node))))
    (etypecase n
      (fixnum (setf (trie-sp trie) (spinit (1+ n))))
      (sparseset (setf (trie-sp trie) n)))
    trie))


(defun spify (sp key)
  "Puts the indices in vector key into the SparseSet sp."
  (declare (type sparseset sp)
  	   (type (simple-array fixnum (*)) key)
  	   (optimize (speed 3) (space 0) (debug 0)))
  (spreset sp)
  (loop for a across key do (spadd sp a))
  sp)

(defun key-equal-p (sp v)
  "The SparseSet sp and vector v are equal if they have the same length
   and every element of v is in sp."
  (declare (type sparseset sp)
  	   (type (simple-array fixnum (*)) v)
  	   (optimize (speed 3) (space 0) (debug 0)))
  (when (= (the fixnum (sp-len sp)) (length v))
    (loop for i across v always (spin-p sp i))))

(defun key-unequal-p (sp v)
  "The SparseSet sp and vector v are equal if they have the same length
   and every element of v is in sp."
  (declare (type sparseset sp)
  	   (type (simple-array fixnum (*)) v)
  	   (optimize (speed 3) (space 0) (debug 0)))
  (cond
    ((/= (the fixnum (sp-len sp)) (length v))
     t)
    (t
     (loop for i across v
	unless (spin-p sp i) return i
	finally (return nil)))))


(defun searchR (h sp)
  (declare (type sparseset sp)
  	   (optimize (speed 3) (space 0) (debug 0)))
  (let ((h h))
    (declare (type (or node item null) h))
    (loop
       do
       ;; h can be an item, nil, or another node
	 (typecase h
	   (item (return-from searchR h))
	   (null (return-from searchR nil)))
       if (not (spin-p sp (node-bit h))) do
	 (setf h (node-l h))
       else do
	 (setf h (node-r h)))))

(defun lookup (key trie)
  "Lookup key in trie."
  (let* ((head (trie-head trie))
	 (sp (spify (trie-sp trie) key))
	 (w (searchR (node-l head) sp)))
    (if (and w (not (key-unequal-p sp (item-key w))))
	(values (item-val w) (item-key w))
	(values nil nil))))


;; the "first" index is the first that is found, not necessarily the
;; smallest number
(defun find-first-differing-index (sp v)
  (declare (type sparseset sp)
  	   (type (simple-array fixnum (*)) v)
  	   (optimize (speed 3) (space 0) (debug 0)))
  (cond
    ((= 0 (length v))
     (return-from find-first-differing-index (spref sp 0)))
    ((< (the fixnum (sp-len sp)) (length v))
     (loop for a across v
	unless (spin-p sp a) do (return a)))
    (t
     (loop for a across v
		  for i from 0 upto (length v)
		  when (not (spin-p sp a)) do (return a)
		  else
		  do (spdel sp a)
	finally
	  (return (spref sp 0))))))


(defun insert (key val trie)
  (declare (type (simple-array fixnum (*)) key)
  	   (type trie trie)
  	   (optimize (speed 3) (space 0) (debug 0)))
  (let* ((p (trie-head trie))
	 (h (node-l p))
	 (sp (spify (trie-sp trie) key))
	 (s nil)
	 (d 0)
	 (place (function (setf node-l)))) 
    (declare (type (or node item null) p h s)
	     (type sparseset sp)
	     (type fixnum d)
	     (type function place))
    (labels ((ins+ret ()
	       (funcall place s p)
	       (return-from insert (values nil nil)))
	     (ins-node ()
	       (let* ((x (make-item key val))
		      (left h)
		      (right x))
		 (if (not (spin-p sp d))
		     (rotatef left right))
		 (setf s (make-node :bit d)
		       (node-l s) left
		       (node-r s) right)
		 (ins+ret)))
	     (ins-item ()
	       (setf s (make-item key val))
	       (ins+ret)))
      (loop
	 do
	   (typecase h
	     (item
	      (let ((e (key-unequal-p sp (item-key h))))
		(typecase e
		  (null
		   (return-from insert (values (item-val h) (item-key h))))
		  (fixnum
		   (setf d e)
		   (ins-node))
		  (t
		   (setf d (find-first-differing-index sp (item-key h)))
		   (ins-node)))))
	     (symbol
	      (ins-item)))
	 if (not (spin-p sp (node-bit h))) do
	   (shiftf p h (node-l h))
	   (setf place #'(setf node-l))
	 else do
	   (shiftf p h (node-r h))
	   (setf place #'(setf node-r))))))


(defun showR (h head f)
  (let ((h h))
    (declare (type (or node item null) h))
    (typecase h
      (item (unless (eq h head) (funcall f h)))
      (null (values))
      (node
       (showR (node-l h) head f)
       (showR (node-r h) head f)))))

(defun show (trie f)
  (showR (node-l (trie-head trie)) (trie-head trie) f))


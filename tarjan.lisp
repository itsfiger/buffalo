;; Copyright (c) 2015 by Frank Huttner
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; The software contains code from cl-yacc Copyright (c) 2005-2009 by Juliusz Chroboczek
;; under the same licence.


(in-package #:buffalo)

;; Algorithm for computing strongly connected components (scc) a la Tarjan
;; The function children is expected to return a vector of child nodes w when
;; called with v; all other functions allow the caller of the algorithm to perform
;; actions, the return values are discarded
;; init is called when a node is visited the first time
;; unite is called on parent and child once the child and its children have been traversed
;; finish is called on parent and children vector once all children and their
;; children have been traversed
;; pop is called when a scc is popped from the stack; note that each node is at least
;; in a scc containing only itself
;; without k, all n nodes are traversed, with k being a number traversal starts only
;; from this node, or k is a vector of starting nodes
(defun scc (n children init unite finish pop &optional (k nil))
  (declare (type (or null function) init unite finish pop)
	   (type function children))
  (let ((i 0)
	(stack '())
	(index (make-array n :initial-element -1))
	(lowlink (make-array n :initial-element -1))
	(onstack (make-array n :initial-element nil :element-type 'boolean)))
    (labels ((scc (v)
	       (setf (aref index v) i)
	       (and init (funcall init v))
	       (setf (aref lowlink v) i)
	       (incf i)
	       (push v stack)
	       (setf (aref onstack v) t)
	       (let ((children (funcall children v)))
		 (loop for w across children
		    if (< (aref index w) 0) do
		      (scc w)
		      (setf (aref lowlink v) (min (aref lowlink v) (aref lowlink w)))
		    else do
		      (if (aref onstack w)
			  (setf (aref lowlink v) (min (aref lowlink v) (aref index w))))
		    end
		    do
		      (and unite (funcall unite v w))
		    finally
		      (and finish (funcall finish v children))))
	       (when (= (aref lowlink v) (aref index v))
		   (loop for w = (pop stack) do
			(setf (aref onstack w) nil)
			(and pop (funcall pop v w))
		      until (= w v)))))
      (if (not k)
	  (loop for v from 0 below n do
	       (if (< (aref index v) 0) (scc v)))
	  (etypecase k
	    (fixnum (scc k))
	    (vector
	     (loop for v across k do
		  (if (< (aref index v) 0) (scc v)))))))))


;; Fix point iteration
;; The actual computations are done on data structures held by f
;; f is expected to return t if a computation has changed a value
(defun fpi (stack f)
  (loop until
       (loop for v across stack
	  with changed-p = nil
	  when (funcall f v) do (setf changed-p t)
	  finally (return (not changed-p)))))


;; maps the sequence a into a newly created fixnum array, possibly applying f
(defun map2a (a &optional (f #'identity))
  (map-into (make-array (length a) :initial-element 0 :element-type 'fixnum) f a))


;; caution: each closure has to be followed by a closure-reset
(let (closure)
  (defun closure-init (n)
    (setf closure (spinit n)))
  (defun closure (children k)
    (declare (type function children))
    (labels ((scc (v)
	       (loop for w across (funcall children v)
		  if (spadd closure w) do (scc w))))
      (etypecase k
	(fixnum (scc k))
	(vector
	 (loop for v across k do
	      (if (spadd closure v) (scc v)))))
      (spiter closure)))
  (defun closure-reset ()
    (spreset closure)))

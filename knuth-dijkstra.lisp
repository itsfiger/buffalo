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

;; implements Knuth, D E "A generalization of Dijkstra's algorithm."
;; Information Processing Letters 6.1 (1977): 1-5.

(in-package #:buffalo)


(defun knuth-dijkstra (no-n no-t rr rr-back rev-nt &optional print-nonterminal-derivations)
  (let* ((m (make-array no-n :initial-element most-positive-fixnum :element-type 'fixnum))
	 (d (make-array no-n :initial-element 0 :element-type '(or list fixnum)))
	 (rp (make-array no-n :initial-element '()))
	 (no-p (- (length rr) no-t no-n))
	 (no-s (+ no-t no-n))
	 (pl (make-array no-p :initial-element 0))
	 (heap (make-instance 'fib-heap:fib-heap))
	 (nodes (make-array no-n :initial-element nil))
	 (sp (spinit no-n))
	 (rhs (make-array no-p :displaced-to rr :displaced-index-offset no-s))
	 (lhs (make-array no-p :displaced-to rr-back :displaced-index-offset no-s)))

    ;; step 1
    (loop for p from 0 below no-p
       do
	 (spreset sp)
	 (loop for sym across (aref rhs p)
	    when (< sym no-t) count t into n-t ; Terminal
	    else do
	      (spadd sp (- sym no-t))
	    finally
	      (let ((i (spiter sp))
		    (n (sp-len sp)))
		(setf (aref pl p) n) ; number of nonterminals on rhs
		(loop for sym across i do (push p (aref rp sym)))
		(when (= n 0) ; when there are no nonterminals
		  (let* ((head (- (aref lhs p) no-t))
			 (min* (aref m head)))
		    (when (< n-t min*) ; when the current sequence is the shortest
		      (setf (aref m head) n-t
			    (aref d head) p)))))))
    (loop for p across rp for i from 0 do (setf (aref rp i) (nreverse p)))
    
    ;; Enqueue all lhs with empty or terminal-only rhs
    (loop for i across m
       for sym from 0
       when (< i most-positive-fixnum) do
	 (setf (aref nodes sym) (fib-heap:insert heap i sym)))
    
    ;; The main loop
    (loop while (not (fib-heap:empty-p heap))
       do
       ;; step 4
	 (let ((y (fib-heap:extract-min heap)))
	   ;; replace the production-no in d with the actual terminal
	   ;; derivation. Remember, all nonterminal symbols for this
	   ;; production have already been computed
	   (setf (aref d y) (loop for sym across (aref rhs (aref d y))
			       when (< sym no-t) append (list sym)
			       else append (aref d (- sym no-t))))
	   ;; step3
	   (loop for p in (aref rp y)
	      for head = (- (aref lhs p) no-t)
	      unless (typep (aref d head) 'list) ; not already in D?
	      do
		(decf (aref pl p))
		(when (= 0 (aref pl p)) ; can rhs be computed?
		  (let ((min* (aref m head))
			(val (loop for sym across (aref rhs p)
				with val = 0
				when (< sym no-t)
				do
				  (incf val)
				else do
				  (incf val (aref m (- sym no-t)))
				finally
				  (return val))))
		    (cond
		      ;; case 1: first computation -> add to queue
		      ((= min* most-positive-fixnum)
		       (setf (aref nodes head) (fib-heap:insert heap val head)
			     (aref m head) val
			     (aref d head) p))
		      ;; case 2: decrease
		      ((< val min*)
		       (fib-heap:decrease-key heap (aref nodes head) val)
		       (setf (aref m head) val
			     (aref d head) p))))))))

    ;; coerce derivations in d to list (for unused productions)
    (loop for p across d for i from 0
       unless (typep (aref d i) 'list)
       do (setf (aref d i) '()
		(aref m i) 0))
    
    (when print-nonterminal-derivations
      (format t "Nonterminal derivations:~%")
      (loop for i from 0 repeat no-n
	 do
	   (format t "~a (Length: ~d): "
		   (aref rev-nt (+ no-t i))
		   (aref m i))
	   (loop for sym in (aref d i)
	      do (format t "\"~a\" " (aref rev-nt sym)))
	   (terpri))
      (terpri))
    d))

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

(in-package #:buffalo)


(defstruct state1
  (id 0 :type fixnum)
  (goto '() :type list)
  (action '() :type list))


(defun number-grammar-lr1%
      (grammar muffle-conflicts muffle-warnings print-derives-epsilon print-states print-first-terminals force-check print-nonterminal-derivations print-state-sentential-forms)
  (declare (type grammar grammar))
  (let* ((no-t 0)		   ; Number of terminals
	 (no-n 1)		   ; Number of nonterminals
	 (no-s 1)		   ; Number of symbols
	 (no-p 1)		   ; Number of productions
	 (no-nn 1)		   ; Number of symbols + productions
	 (i 1)			   ; Counter, i=0 reserved for epsilon
	 (r (make-array no-s :initial-element '() :element-type 'list))
	 ;; work array
	 (rr (make-array (+ no-s 0) :initial-element (make-array (+ i 0) :initial-element 0 :element-type 'fixnum) :element-type '(simple-array fixnum)))
	 ;; cross-reference: length = no-t no-n no-p
	 ;; each element is a vector refering to the productions starting with
	 ;; this symbol (no-t is therefore empty) or in case of a production
	 ;; refering to the symbols it derives
	 (rr-back (make-array (+ no-s 0) :initial-element 0 :element-type 'fixnum))
	 ;; back-reference: length = no-t no-s no-productions
	 ;; in case of productions no-s of the lhs (production-head)
	 (rr-production (make-array (+ no-s 0) :initial-element (make-production nil nil) :element-type 'production))
	 ;; grammar reference: length = no-t + no-n + no-p
	 ;; in case of productions to the structure (grammar-production grammar)
	 (l-graph (make-array no-s :initial-element 0 :element-type 'fixnum))
	 ;; concept introduced by Park et al.:
	 ;; A L B <-> A -> B alpha is production in grammar
	 (leps-graph (make-array no-s :initial-element 0 :element-type 'fixnum))
	 (rev-nt (make-array no-s :initial-element nil :element-type 'symbol))
	 ;; back-reference from symbol numbers to symbols
	 (duplicate-terminals '())
	 ;; for grammar-checks
	 (prec (make-array no-s :initial-element #() :element-type '(simple-array t *)))
	 ;; Precedence levels: length = no-t no-s no-productions
	 (nitems 0)
	 ;; No of Items
	 (items-derives (make-array nitems :initial-element 0))
	 ;; Symbol after dot in item
	 (items-next (make-array nitems :initial-element -1))
	 ;; ID of next item
	 (items-back (make-array nitems :initial-element 0))
	 ;; ID of the production from which the item derives
	 (items-nullrest (make-array nitems :initial-element 0))
	 ;; Flag: 1 = rest behind dot derives epsilon
	 (items-first (make-array nitems))
	 (items-epst (make-array nitems :initial-element #()))
	 ;; Epsilon transition of items = Children in the closure
	 (items-revtopc (make-array nitems :initial-element '()))
	 ;; Children in the reverse topologigal sort of nullable items
	 (de (make-array no-nn :initial-element 0))
	 (first (make-array no-nn :initial-element 0))
	 ;; Flag for derives-epsilon for symbols and productions
	 (s-prime 0)
	 ;; Symbol-no of S' in added prodcution S' -> S
	 (accept-state 0)
	 (states '())
	 (states-stack '())
	 ;; No of transitions of the form (p,A) with p State, A Non-Terminal
	 (text (make-string-output-stream))
	 (start-symbol t)
	 sp-mark sp-t
	 (emptvec (map2a #())))
    (declare (special nt-table))
    (labels ((print-laset (trie)
	       (loop for s across trie do
		    (format t " ~s" (aref rev-nt s))))
	     (print-item1 (item &optional laset)
	       (let* ((rule (aref items-back item))
		      (head (aref rr-back rule))
		      (find-next (lambda (v)
				   (unless (> 0 (aref items-next v))
				     (aref items-next v))))
		      (mark (lambda (v)
			      (let ((end (> 0 (aref items-next v)))
				    (v (aref items-derives v)))
				(if (not end)
				    (if (< v no-t)
					(format t "\"~a\" " (aref rev-nt v))
					(format t "~a " (aref rev-nt v))))
				end))))
		 (format t "~a -> " (aref rev-nt head))
		 (loop
		    for pos = (- rule no-s) then (funcall find-next pos)
		    if (= pos item) do (format t ". ") end
		    until (funcall mark pos))
		 (format t "  ~a nullable" (if (= 0 (aref items-nullrest item)) "is" "not"))
		 (when laset
		   (format t ", LA: ")
		   (print-laset laset)
		   (terpri))))
	     
	     (check-grammar ()
	       (let ((s (geth start-symbol)))
		 ;; 1.: Start symbol
		 (cond
		   ((not s)
		    (format text "Start symbol ~A does not start a production.~%" start-symbol))
		   ((< s no-t)
		    (format text "Start symbol ~A also defined as terminal.~%" start-symbol)))
		 ;; 2.: Terminals
		 (loop for production in (cdr (grammar-productions grammar))
		    if (< (geth (production-symbol production)) no-t) do
		      (format text "Terminal cannot define a rule as ~A on production ~a.~%" (production-symbol production) production))
		 (if duplicate-terminals
		     (format text "The terminals ~a are defined more than once.~%" duplicate-terminals))
		 ;; 3. Productions
		 (loop for sym from no-t below no-s
		    do
		      (loop for rule0 across (aref rr sym)
			 for r0 from 0
			 do
			   (loop for rule1 across (aref rr sym)
			      for r1 from 0
			      when (and (< r0 r1) (equalp (aref rr rule0) (aref rr rule1)))
			      do
				(format text "Identical productions: ~a.~%" (aref rr-production rule0)))))
		 (let ((text (get-output-stream-string text)))
		   (unless (equal text "")
		     (error (make-condition 'buffalo-grammar-error
					    :text text)))))))
      
      (loop for terminal in (cons '-eof- (grammar-terminals grammar))
	 when (not (geth terminal)) do
	   (setf (geth terminal) i)
	   (incf i)
	 else
	 do (push terminal duplicate-terminals))
      (setf no-t i)
      (setf start-symbol (car (production-derives (car (grammar-productions grammar)))))
      (loop for production in (grammar-productions grammar)
	 for head = (production-symbol production)
	 for np from 1
	 unless (geth head) do
	   (setf (geth head) i)
	   (incf i)
	 finally (setf no-p np))
      (setf no-s i
	    no-n (- no-s no-t)
	    no-nn (+ no-s no-p)
	    r (make-array no-s :initial-element '())
	    rr (make-array no-nn :initial-element (make-array 0 :initial-element 0 :element-type 'fixnum))
	    rr-back (make-array no-nn :initial-element '())
	    rr-production (make-array no-nn :initial-element '())
	    l-graph (make-array no-n :initial-element (make-array 0))
	    leps-graph (make-array no-n :initial-element (make-array 0))
	    rev-nt (make-array no-s)
	    prec (make-array no-nn :initial-element #(0)))
      (mapc #'(lambda (sym)
		   (setf (aref rev-nt (geth sym)) sym)) nt-table)
      (loop for production in (grammar-productions grammar)
	 for head = (geth (production-symbol production))
	 for derives = (map2a (production-derives production)
			      (lambda (s)
				(let ((sym (geth s)))
				  (cond
				    ((not sym)
				     (format text "Unknown symbol ~A is production ~s.~%" s production)
				     0)
				    (sym sym)))))
	 for n from no-s
	 do
	   (push n (aref r head))
	   (setf (aref rr n) derives)
	   (setf (aref rr-back n) head)
	   (setf (aref rr-production n) production)
	 finally
	   (loop for rules across r
	      for k from 0
	      do (setf (aref rr k) (map-into (make-array (length rules) :initial-element 0 :element-type 'fixnum) #'identity rules))))
      (setf s-prime (geth 's-prime))

      ;; ---- Precedence ---
      (loop for precedence in (reverse (grammar-precedence grammar))
	 for level from 1
	 do
	   (loop for sym in (cdr precedence)
	      for s = (geth sym)
	      do
		(cond
		  ((not s)
		   (format text "Precedence symbol ~A is undefined in grammar.~%" sym))
		  ((>= s no-t)
		   (format text "Precedence symbol ~A must be a terminal but is a non-terminal.~%" sym))
		  (t (setf (aref prec s) (vector level (car precedence)))))))
      (loop for rule from no-s below no-nn
	 for production in (grammar-productions grammar)
	 when (production-prec production)
	 do
	   (let ((sym (geth (production-prec production))))
	     (if (or (not sym) (>= sym no-t))
		 (format text "Precedence symbol ~A must be a terminal.~%" (production-prec production))
		 (setf (aref prec rule) (vector (aref (aref prec sym) 0)))))
	 else
	 do
	   (loop for sym across (aref rr rule)
	      for precedence = (aref (aref prec sym) 0)
	      when (< sym no-t) ; Terminal
	      do
		(setf (aref prec rule) (vector precedence))))

      ;; ---- Grammar Checks ---
      (check-grammar)

      (setf (grammar-no-terminals grammar) (1- no-t)
	    (grammar-no-nonterminals grammar) no-n)

      ;; ------- Nullable and nonempty symbols ------------
      (let ((sp-n (spinit no-nn))
	    (lhs (make-array no-p :displaced-to rr-back :displaced-index-offset no-s))
	    (rhs (make-array no-p :displaced-to rr :displaced-index-offset no-s))
	    (nonterminal-names (make-array no-n :displaced-to rev-nt :displaced-index-offset no-t)))
	
	(initial-guess-at-used-symbols sp-n no-t rr s-prime grammar)
	
	(setf de (make-array no-nn :initial-element nil))
	(let ((de (make-array no-n :displaced-to de :displaced-index-offset no-t)))
	  (nullable-and-empty-symbols nil no-t no-n no-p sp-n lhs rhs de)
	  (multiple-value-bind (n empty)
	      (collect-empty/nullable-symbols nil de nonterminal-names)
	    (when empty
	       (let ((text (format nil "~D symbols derive the empty language:~%~S~%~%" n empty)))
		 (if (member start-symbol empty)
		     (error (make-condition
			     'buffalo-grammar-error
			     :text text))
		     (unless muffle-warnings
		       (warn (make-condition
			      'buffalo-muffable-warning
			      :text text))))))))
	(setf de (make-array no-nn :initial-element nil))
	(let ((de (make-array no-n :displaced-to de :displaced-index-offset no-t)))
	  (nullable-and-empty-symbols t no-t no-n no-p sp-n lhs rhs de)
	  (multiple-value-bind (n nullable)
	      (collect-empty/nullable-symbols t de nonterminal-names)
	    (when print-derives-epsilon
	      (format t "~D symbols derive epsilon:~%~S~%~%" n nullable)))))

      ;; ------- Print shortest terminal derivations when requested  ------------
      (when print-nonterminal-derivations
	(knuth-dijkstra no-n no-t rr rr-back rev-nt t))

      ;; ------- FIRST ------------
      (setf first (make-array no-n :initial-element emptvec))
      (setf sp-mark (spinit no-s))
      (setf sp-t (spinit no-t))
      (let ((rf (make-array no-n :initial-element '())))
	(loop for n from no-t below no-s
	   for j from 0
	   do
	     (loop for rule across (aref rr n) do
		  (loop for sym across (aref rr rule)
		     if (< sym no-t) ; terminal
		     do (setf (aref first j) (sp-merge! sp-t (aref first j) (vector sym)))
		     else
		     do (spadd sp-mark (- sym no-t)) ; nonterminal
		     end
		     while (aref de sym))
		finally
		  (setf (aref rf j) (spcompress! sp-mark))))
	(let* ((children (lambda (v) (aref rf v)))
	       (init nil)
	       (unite nil)
	       (finish (lambda (v w)
			 (sp-merge1 sp-mark (aref first v))
			 (loop for u across w do
			      (sp-merge2 sp-mark (aref first u)))
			 (setf (aref first v) (spcompress! sp-mark))))
	       (pop (lambda (v w) (setf (aref first w) (aref first v)))))
	  (scc no-n children init unite finish pop))

	;; Print first terminals
	(when print-first-terminals
	  (loop for sym from no-t below no-s
	     for j from 0
	     do
	       (format t "Nonterminal ~S derives:" (aref rev-nt sym))
	       (print-laset (aref first j))
	       (terpri))
	  (terpri))

      ;; --------- Items -----------
      (setf nitems (loop for rule from no-s below no-nn
		      sum (1+ (length (aref rr rule))))
	    items-derives (make-array nitems :initial-element 0)
	    items-next (make-array nitems :initial-element -1)
	    items-back (make-array nitems :initial-element 0)
	    items-nullrest (make-array nitems :initial-element 0)
	    items-first (make-array nitems :initial-element emptvec)
	    items-revtopc (make-array nitems :initial-element '())
	    items-epst (make-array nitems :initial-element '()))
      (loop for m from 0 below no-p
	 with h = no-p
	 for rule from no-s
	 for rule-derives = (aref rr rule)
	 do
	   (loop
	      for now = m then (1- h)
	      for next = h
	      for dot across rule-derives
	      do
		(setf (aref items-next now) next)
		(setf (aref items-derives now) dot)
		(setf (aref items-back now) rule)
		(setf (aref items-nullrest next) now)
		(incf h)
	      finally
		(setf (aref items-back now) rule)
		(loop repeat (length rule-derives)
		   for act = (shiftf (aref items-nullrest now) 0) then new
		   for new = (aref items-nullrest act)
		   for derives = (aref items-derives act)
		   for epsilon? = (aref de derives) then (and epsilon? (aref de derives))
		   do
		     (setf (aref items-nullrest act)
			   (if epsilon? 0 1)))))

      ;; --------- Items-First ----------
      (spreset sp-mark)
      (loop
	 for i from 0 below nitems
	 do
	 ;; then loop over the rest of the item as long as there are symbols
	 ;; and they derive epsilon
	   (loop
	      for k = i then (aref items-next k)
	      for derives = (aref items-derives k)
	      while (> derives 0)
	      when (< derives no-t)
	      ;; a terminal is directly added
	      do (spadd sp-mark derives)
	      else do
		(sp-merge sp-mark (aref first (- derives no-t)))
	      while (aref de derives)
	      finally
		(setf (aref items-first i) (spcompress! sp-mark))))

      (when print-first-terminals
	(loop
	   for i from 0 below nitems
	   do (print-item1 i (aref items-first i))))

      ;; -------- Epsilon transitions of items graph ------
      (loop for item from 0 below nitems
	 do
	   (let ((s (aref items-derives item)))
	     ;; items-derives nt?
	     (setf (aref items-epst item)
		   (if (>= s no-t)
		       (map2a (aref rr s) (lambda (p) (- p no-s)))
		       emptvec))))

      ;; -------- Reverse nullable items graph ------
      ;; In this graph a item is a child of its parent item if
      ;; there is an epsilon transition form the child to the parent
      ;; and the rest of the transition item of the child is nullable:
      ;; I1: A -> alpha . B beta (child, beta nullable)
      ;; I2; B > . gamma (parent)
      ;; -> I1 is child of I2
      (loop for item from 0 below nitems
	 when (and
	       (>= (aref items-derives item) no-t) ; nonterminal
	       (= 0 (aref items-nullrest (aref items-next item))))
	 do
	   (loop for p across (aref rr (aref items-derives item))
	      do (push item (aref items-revtopc (- p no-s)))))
      (loop for item from 0 below nitems
	 do (setf (aref items-revtopc item) (map2a (aref items-revtopc item))))
	   

      ;; --------- Leps-Graph -----------
      ;; The relation Leps between two non-terminal symbols A and B, A Leps B,
      ;; exists iff there is a production A -> B alpha in the grammar and
      ;; alpha is nullable
      (let ((sp-lepsgraph (spinit no-n)))
	(loop for head from no-t below no-s
	   for head-i from 0
	   do
	     (loop for rule across (aref rr head)
		for rhs = (aref rr rule)
		;do (print rhs)
		when (/= 0 (length rhs)) do
		  (let ((b (aref rhs 0)))
		    (when (and (>= b no-t) (/= head b)) ; Rule: A -> B beta
		      (let ((item (- rule no-s)))
			;; Item: A -> . B beta; nextItem: A -> B . beta
			(if (= 0 (aref items-nullrest (aref items-next item)))
			    (spadd sp-lepsgraph (- b no-t))))))
		finally
		  (setf	(aref leps-graph head-i) (spcompress! sp-lepsgraph)))))

      ;; The leps-graph should contain no cycles: by a conjecture by
      ;; DeRemer/Pennello the grammar would then be infinitely ambiguous
      (let* ((stack '())
	     (children (lambda (v) (aref leps-graph v)))
	     (init nil)
	     (unite nil)
	     (finish nil)
	     (pop (lambda (v w)
		    (if (= v w)
			(when stack
			  (push (aref rev-nt (+ w no-t)) stack)
			  (error (make-condition 'buffalo-grammar-error
						   :text (format nil "Ambiguous grammar with infinite cycle with symbols:~%~S~%~%" stack))))
			(push (aref rev-nt (+ w no-t)) stack)))))
	(scc no-n children init unite finish pop))

      ;; --------- States -----------
      (let* ((la (make-array nitems :initial-element emptvec))
	     (goto-next (make-array no-s :initial-element '()))
	     (la-next (make-array no-s :initial-element '()))
	     (work (make-array nitems))
	     (parents (make-array nitems :initial-element '()))
	     (move (make-array no-t :initial-element '()))
	     (done (make-array no-s))
	     (fin (map2a #(0)))
	     (sp-i (spinit nitems))
	     (sp-closure (spinit nitems))
	     (sp-closure2 (spinit nitems))
	     (sp-t (spinit no-t))
	     (sp-goto (spinit no-s))
	     (co (make-conflict :muffle-conflicts muffle-conflicts))
	     (action '()) (goto '()))
	(labels ((init (v w)
		   (when (>= (aref items-next v) 0)
		     (push (aref items-next v) (aref parents w))))
		 (unite1 (v w)
		   (declare (ignore v w))
		   (values))
		 (init2 (v w)
		   (declare (ignore v w))
		   (values))
		 (unite2 (v w)
		   (when (loop for u across w thereis (spin-p sp-closure u))
		     (sp-merge1 sp-t (aref la v))
		     (loop for u across w do
			  (when (spin-p sp-closure u) ; are we still in the state?
			    (sp-merge2 sp-t (aref la u)))
			finally
			  (setf (aref la v) (spcompress! sp-t)))))
		 (children (v)
		   (aref items-epst v))
		 (null-children (v)
		   (if (spin-p sp-closure v)
		       (aref items-revtopc v)
		       emptvec)))

	  (loop for i from 0 below no-s
	     do
	       (setf (aref done i) (patricia:create-trie sp-i)))

      ;; Start with the start state
	  (push (list (map2a (vector (geth '-eof-)))) states-stack)
	  (push
	   (map2a (vector (- (aref (aref rr s-prime) 0) no-s))) states-stack)
	  (push 0 states-stack)
	  
	  (loop
	     with stateno = 0
	     for state = (pop states-stack)
	     for core-items = (pop states-stack)
	     for closure =
	       (let ((las (pop states-stack)))
		 (loop for l in las
		    for item across core-items
		    do
		      (setf (aref la item) l))
		 (spreset sp-closure)
		 (closure2 sp-closure #'children #'init #'unite1 core-items))
	     do
	       (loop for item across closure
		  when (aref parents item) do
		    (setf (aref la item)
			  (cond
			    ((cdr (aref parents item))
			     (progn
			       (sp-merge1 sp-t (aref items-first (first (aref parents item))))
			       (loop for u in (cdr (aref parents item)) do
				    (sp-merge2 sp-t (aref items-first u))
				  finally
				    (return (spcompress! sp-t)))))
			    (t
			     (aref items-first (first (aref parents item)))))
			  (aref parents item) '()))
	       (spreset sp-closure2)
	       (closure2 sp-closure2 #'null-children #'init2 #'unite2 closure)
	       (spreset sp-t)
	       (spreset sp-mark)
	       
	       (when print-states
		 (format t "Stateno: ~a~%" state)
		 (loop for item across closure do
		      (print-item1 item (aref la item))))
	       
	       (loop for item across closure
		  do
		    (let* ((derives (aref items-derives item))
			   (next (aref items-next item)))
		      (when (> derives 0)
			(spadd sp-goto derives)
			(push next (aref goto-next derives))
			(push (aref la item) (aref la-next derives)))))

	       (loop for sym across (spiter sp-goto)
		  for kernel-items = (map2a (shiftf (aref goto-next sym) '()))
		  for las = (shiftf (aref la-next sym) '())
		  for find = (find-lr1state kernel-items (aref done sym) las work sp-t)
		  for next =
		    (let  ((num (patricia:lookup fin find)))
		      (if num
			  num
			  (progn
			    (push las states-stack)
			    (push kernel-items states-stack)
			    (patricia:insert fin (incf stateno) find)
			    (push stateno states-stack)
			    stateno)))

		  ;; the goto's are straightforward and without conflict
		  if (>= sym no-t) do
		    (push (cons (aref rev-nt sym) next) goto)
		  else do ; terminal transition = shift
		  ;; the "action" is the shift-state
		    (spadd sp-mark sym)
		    (setf (aref move sym) next)
		  end)

	       (loop for item across closure
		  do
		    (let* ((derives (aref items-derives item)))
		      (when (= derives 0) ; Final item
			  (loop for sym across (aref la item)
			   do
			     (let ((s-a (aref move sym))
				 (rule (- (aref items-back item) no-s)))
			     ;; Final item and S-Prime is head -> final state
			     (if (= s-prime (aref rr-back (aref items-back item)))
				 (setf accept-state state))
			     (spadd sp-mark sym)
			     (cond
			       (s-a ; there is already an action saved for this symbol
				;; we have a conflict
				(resolve-conflict co rule s-a sym closure state rr-production no-s items-derives items-back prec rev-nt)
				(setf (aref move sym) (conflict-action co)))
			       (t (setf (aref move sym) (- rule))))))))) ;; the "action" is the rule

	       (spreset sp-goto)

	     ;; now the actions can be set and the work array be cleared for the next state
	       (loop for sym across (spiter sp-mark)
		  do
		    (push (cons (aref rev-nt sym) (shiftf (aref move sym) nil)) action)
		  finally
		    (spreset sp-mark))
	       
	       (push (make-state1
		      :id state
		      :action (shiftf action '())
		      :goto (shiftf goto '())) states)
	       
	     while states-stack)

	  (let* ((nstates (length states))
		 (action-array (make-array nstates))
		 (goto-array (make-array nstates)))
	    (loop for state in states
	       for i = (state1-id state)
	       do
		 (setf (aref action-array i) (state1-action state))
		 (setf (aref goto-array i) (state1-goto state)))
	      (warnings-and-unused-productions
	       co muffle-warnings force-check
	       no-t no-n no-s no-p nstates
	       rr-back rr de rev-nt
	       action-array goto-array
	       grammar
	       print-state-sentential-forms)
	    (prepare-and-return
	     no-p no-s no-nn rr
	     action-array goto-array accept-state
	     grammar co))))))))



;; first find the lr0 state
;; if it exists, its value will be a patricia trie of an la set which
;; in turn points to the next patricia trie
;; the items are ordered be the key if it exists
(defun find-lr1state (normalized trie las work sp-t)
  (let ((cache (patricia:create-trie sp-t)))
    (multiple-value-bind (start key) (patricia:insert normalized cache trie)
      (unless start
	(setf key normalized)
	(shiftf start cache (patricia:create-trie sp-t)))
      (loop for la in las
	 for item across normalized
	 do
	   (setf (aref work item) la))
      (loop for item across key
	 for pat = start then new-pat
	 for next-pat = (patricia:insert (aref work item) cache pat)
	 for new-pat =  (if next-pat
			    next-pat
			    (shiftf cache (patricia:create-trie sp-t)))
	 finally
	   (return new-pat)))))
	       

(defun sp-merge! (sp v w)
  "Adds the shorter vector of v and w to the other one.
   Returns v, if w is subset, or the compressed new one
   if w is no subset."
  (spreset sp)
  (let ((v v)
	(w w))
    (if (< (length v) (length w))
	(rotatef v w))
    (when (= 0 (length w))
      (return-from sp-merge! v))
    (loop for u across v do (spadd sp u))
    (let ((ever nil))
      (loop for u across w
	 for a = (spadd sp u)
	 do
	   (setf ever (or ever a)))
      (if (not ever)
	  v
	  (spcompress! sp)))))

(defun sp-merge1 (sp v)
  (spreset sp)
  (loop for u across v do (spadd sp u)))

(defun sp-merge2 (sp w)
  (loop for u across w do
       (spadd sp u)))

(defun sp-merge (sp v)
  (loop for u across v do (spadd sp u)))


(defun closure2 (sp children init finish k)
  (declare (type function children init finish))
  (labels ((scc (v)
	     (let ((children (funcall children v)))
	       (loop for w across children
		  do (funcall init v w)
		  if (spadd sp w) do (scc w) end)
	       (funcall finish v children))))
    (etypecase k
      (fixnum (scc k))
      (vector
       (loop for v across k
	  when (spadd sp v) do (scc v))))
    (spiter sp)))


(defun closure3 (sp children init finish k)
  (declare (type function children init finish))
  (labels ((scc (v)
	     (funcall init v)
	     (let ((children (funcall children v)))
	       (loop for w across children
		  if (spadd sp w) do (scc w) end)
	       (funcall finish v children))))
    (etypecase k
      (fixnum (scc k))
      (vector
       (loop for v across k
	  when (spadd sp v) do (scc v))))
    (spiter sp)))


(defun collect-empty/nullable-symbols (true-p de nonterminal-names)
  (loop
     for a across de
     for s across nonterminal-names
     when (eq a true-p)
     collect s into de
     and count t into n
     finally
       (return (values n de))))
  
(defun nullable-and-empty-symbols (null-p no-t no-n no-p sp-n lhs rhs de)
  "Implements Algorithm 4.14 from Sippu: Parsing Theory, that is linear in |G|"
  ;; Nullable and nonempty symbols are computed using the same algorithm.
  ;; Only the base case is different: Terminals are not nullable (null-p t),
  ;; so these productions do not have to be considered, while only the
  ;; productions where only terminals or terminal-only deriving nonterminals
  ;; are on the rhs are actually non-empty
  (let ((w '())
	(pl (make-array no-p :initial-element 0))
	(rp (make-array no-n :initial-element '())))
    (loop for p from 0 below no-p
       do
	 (spreset sp-n)
	 (loop for sym across (aref rhs p)
	    when (< sym no-t) do (when null-p (return)) ; Terminal
	    else do
	      (spadd sp-n (- sym no-t))
	    finally
	      (let ((i (spiter sp-n))
		    (n (sp-len sp-n)))
		(setf (aref pl p) n) ; number of nonterminals on rhs
		(loop for sym across i do (push p (aref rp sym)))
		(when (= n 0)
		  (push (- (aref lhs p) no-t) w)))))
    (spreset sp-n)
    (loop for sym in w do (spadd sp-n sym)) ;w0
    (loop
       for w+1 = (spcompress! sp-n)
       when (= 0 (length w+1)) do (return)
       do
	 (loop for sym across w+1
	    do
	      (setf (aref de sym) t)
	      (loop for p in (aref rp sym)
		 do
		   (decf (aref pl p))
		 when (and (= 0 (aref pl p))
			   (not (aref de (- (aref lhs p) no-t)))) do (spadd sp-n (- (aref lhs p) no-t)))))))

;;; This is just an initial guess because nonterminals/production might not
;;; be reached due to conflicts and their resolution
(defun initial-guess-at-used-symbols (sp no-t rr s-prime grammar)
  (let ((used-terminal 0)
	(used-nonterminal 0))
    (flet ((init (sym)
	     (if (< sym no-t)
		 (incf used-terminal)
		 (incf used-nonterminal)))
	   (children (sym)
	     (let ((n '()))
	       (loop for p across (aref rr sym)
		  do
		    (loop for sym across (aref rr p)
		       do
			 (push sym n)))
	       (map2a n))))
      (closure3 sp #'children #'init #'values s-prime)
      (setf (grammar-used-terminals grammar) used-terminal)
      (setf (grammar-used-nonterminals grammar) used-nonterminal))))


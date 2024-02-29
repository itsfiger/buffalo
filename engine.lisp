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


(defstruct (state (:print-function print-state))
  (id 0 :type fixnum)
  (items #() :type vector)
  (trans #() :type vector)
  (all-items #() :type vector))

(defun print-state (k s d)
  (declare (type state k) (stream s) (ignore d))
  (print-unreadable-object (k s :type t)
    (format s " id=~D" (state-id k))))


(defstruct sitem
  (item 0 :type fixnum)
  (items #() :type vector)
  (preds '() :type list)
  (next nil))


(defstruct conflict
  (exist-p nil :type boolean)
  (muffle-conflicts nil :type (or boolean list))
  (sr 0 :type fixnum)
  (rr 0 :type fixnum)
  (action 0 :type (or null fixnum))
  (total 0 :type fixnum)
  (used-productions 0 :type fixnum)
  (conflict-states '() :type list))


;; geth replaces the gethash function; the result from indexing (numbering)
;; the grammar is stored directly on the symbol instead of maintaining a hash table;
;; the plist ist cleared by an unwind-protect
(defun geth (sym)
  (declare (special mark))
  (get sym mark))

(defun (setf geth) (val sym)
  (declare (special nt-table mark))
  (unless (get sym mark)
    (push sym nt-table)
    (setf (get sym mark) val)))

(defun number-grammar (grammar
		       &key (muffle-conflicts nil) (muffle-warnings t)
			 (print-derives-epsilon nil) 
			 (print-states nil)
			 (print-first-terminals nil)
			 (print-includes nil)
			 (canonical-lr1 nil)
			 (force-check nil)
			 (print-nonterminal-derivations nil)
			 (print-state-sentential-forms nil))
  (declare (type grammar grammar))
  (let ((nt-table '())
	(mark (gensym)))
    (declare (special nt-table mark))
    (unwind-protect
	 (if canonical-lr1
	     (number-grammar-lr1% grammar muffle-conflicts muffle-warnings print-derives-epsilon print-states print-first-terminals force-check print-nonterminal-derivations print-state-sentential-forms)
	     (number-grammar% grammar muffle-conflicts muffle-warnings print-derives-epsilon print-states print-includes force-check print-nonterminal-derivations print-state-sentential-forms))
      (loop for sym in nt-table do (remprop sym mark)))))

  
(defun number-grammar%
      (grammar muffle-conflicts muffle-warnings print-derives-epsilon print-states print-includes force-check print-nonterminal-derivations print-state-sentential-forms)
  (declare (type grammar grammar))
  (let* ((no-t 0)		   ; Number of terminals
	 (no-n 1)		   ; Number of nonterminals
	 (no-s 1)		   ; Number of symbols
	 (no-p 1)		   ; Number of productions
	 (no-nn 1)		   ; Number of symbols + productions
	 (i 1)			   ; Counter, i=0 reserved for epsilon
	 (no-used-p 0)             ; No of used production
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
	 (items-disjoint (make-array nitems :initial-element -1))
	 (items-seed (make-array no-s :initial-element -1))
	 (de (make-array no-nn :initial-element 0))
	 ;; Flag for derives-epsilon for symbols and productions
	 (s-prime 0)
	 ;; Symbol-no of S' in added prodcution S' -> S
	 (accept-state 0)
	 (states '())
	 (states-stack '())
	 (ntrans 0)
	 ;; No of transitions of the form (p,A) with p State, A Non-Terminal
	 (states-vec (make-array 0))
	 (text (make-string-output-stream))
	 (start-symbol t)
	 sp-mark sp-goto)
    (declare (special nt-table))
    (labels ((print-item! (item &optional preds)
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
		 (when (and (>= item no-p) preds)
		   (apply #'format t ", lookback to states:~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]." (mapcar #'state-id preds)))
		 (terpri)))
	     
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
				     (format text "Unknown symbol ~A in production ~s.~%" s production)
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

      ;; --------- L-Graph -----------
      (let ((sp-lgraph (spinit no-n)))
	(loop for head from no-t below no-s
	   for head-i from 0
	   do
	     (loop for deriv across (aref rr head)
		when (/= 0 (length (aref rr deriv))) do
		  (let ((b (aref (aref rr deriv) 0)))
		    (if (and (>= b no-t) (/= head b)) ; Rule: A -> B beta
			(spadd sp-lgraph (- b no-t))))
		finally
		  (setf (aref l-graph head-i) (spcompress! sp-lgraph)))))
      
      ;; --------- Items -----------
      (setf nitems (loop for rule from no-s below no-nn
		      sum (1+ (length (aref rr rule))))
	    items-derives (make-array nitems :initial-element 0)
	    items-next (make-array nitems :initial-element -1)
	    items-back (make-array nitems :initial-element 0)
	    items-nullrest (make-array nitems :initial-element 0))
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

      ;; --------- Items-Sets -----------
      ;; Items form disjoint union of "per marked symbol" sets
      ;; The items in these sets are numbered
      (setf items-disjoint (make-array nitems :initial-element -1))
      (setf items-seed (make-array no-s :initial-element 0))
      (loop for m from 0 below nitems
	 for derives across items-derives
	 do
	   (setf (aref items-disjoint m) (aref items-seed derives))
	   (incf (aref items-seed derives)) ;(setf (aref seed derives) (* 2 (aref seed derives)))
	   )

      ;; --------- States -----------
      ;; States are found by a list search on kernel items where a separate list is
      ;; maintained for every accessing symbol (array done)
      (let* ((item-list (make-array nitems :fill-pointer 0 :element-type 'sitem))
	     (goto-next (make-array no-s :initial-element '()))
	     (work (make-array nitems))
	     (shift-item (lambda (sitem)
			   (vector-push sitem item-list)
					;(sp-set item sp-items)
			   (let ((item (sitem-item sitem)))
			     (let ((items-derives (aref items-derives item)))
			       (when (> items-derives 0)
				 (spadd sp-goto items-derives)
				 (push sitem (aref goto-next items-derives))
				 items-derives)))))
	     (children (lambda (v) (aref l-graph v)))
	     (init (lambda (v)
		     (loop for rule across (aref rr (+ no-t v)) do
			  (funcall shift-item (make-sitem :item (- rule no-s))))))
	     (done (make-array no-s)))
	(loop for i from 0
	   for max across items-seed
	   do
	     (setf (aref done i) (patricia:create-trie max)))
	(push
	 (make-state
	  :items (vector (make-sitem :item (- (aref (aref rr s-prime) 0) no-s)))
	  :id 0) states-stack)
	(setf sp-mark (spinit no-s))
	(setf sp-goto (spinit no-s))
	(closure-init no-n)
	(loop
	   for state = (pop states-stack)
	   with stateno = 0
	   do
	     (push state states)
	     (let (state-trans)
	       (loop for k across
		    (closure children
			     (loop for kernel-item across (state-items state) ; Shift the kernel items
				for mark = (funcall shift-item kernel-item)
				when (and mark (>= mark no-t)) do (spadd sp-mark (- mark no-t))
				finally (return (spiter sp-mark))))
		  do (funcall init k))	; now shift the closure
	       (spreset sp-mark)
	       (closure-reset)
	       (setf (state-all-items state) (copy-seq item-list)
		     (fill-pointer item-list) 0)
	       (setf state-trans (spiter sp-goto))
	       ;; state-trans are all the symbols by wich the state can be left
	       ;; (state-trans state) is a vector of elements of the form: #(sym, goto-state)
	       (setf (state-trans state)
		     (map-into (make-array (length state-trans) :initial-element (vector))
			       (lambda () (vector -1 -1))))
	       (loop for sym across state-trans
		  for trans across (state-trans state)
		  ;; from-state-items are a bit of a misnomer, because they form the kernel
		  ;; of the new state, but only when shifted
		  for from-state-items = (coerce (shiftf (aref goto-next sym) '()) 'vector)
		  for normalized = (map2a
				    from-state-items
				    (lambda (item)
				      (aref items-disjoint (sitem-item item))))
		  for place = (cons nil nil)
		  for findcons = (patricia:insert normalized place (aref done sym))
		  ;for findcons = (patricia:lookup normalized (aref done sym))
		  for find = (car findcons)
		  for newstate = (cdr findcons)
		  when (>= sym no-t) do (incf ntrans) ; -> Non-Terminal transitions
		  if (not find) do		      ; new state
		    (let ((state (make-state
				  :items (map 'vector
					      (lambda (item)
						(let ((new-item (make-sitem :item (aref items-next (sitem-item item)))))
						  (setf (sitem-next item) new-item)
						  new-item))
					      from-state-items)
				  :id (incf stateno))))
		      (push state states-stack)
		      (setf (car place) stateno (cdr place) state)
		      ;(patricia:insert normalized (cons stateno state) (aref done sym))
		      (setf (aref trans 0) sym
			    (aref trans 1) stateno))
		  else do
		    (setf (aref trans 0) sym
			  (aref trans 1) find)
		  ;; Item sets are not necessarily in the same order, therefore they
		  ;; have to be put into the dereferenced work array
		    (loop for item across (state-items newstate)
		       do (setf (aref work (sitem-item item)) item))
		    (loop for item across from-state-items
		       do
			 (setf (sitem-next item) (aref work (aref items-next (sitem-item item))))))
	       (spreset sp-goto))
	   while states-stack
	   finally
	   ;; when all states are generated they can be put into a vector
	     (setf states-vec (make-array (1+ stateno)))
	     (loop for state in states do
		  (setf (aref states-vec (state-id state)) state))))
    
      ;; --------- Lookback Edges --------
      (let* (from-state
      	     (follow-item (lambda (item)
      			    (loop
      			       for act-item = item then (sitem-next act-item)
			       while act-item
      			       do
				 (push from-state (sitem-preds act-item))))))
      	;; Lookaheads are computed for all final items, not only for the ones in
      	;; conflicting states; therefore the state machine is run forward for
      	;; all non-kernel items in all states
      	(loop for state across states-vec
      	   do
      	     (setf from-state state)
      	     (loop for sitem across (state-all-items state)
      		when (< (sitem-item sitem) no-p) ;; dot is at leftmost position = non-kernel item
      		do (funcall follow-item sitem))))

      (when print-states
	(loop for state across states-vec
	   do
	     (format t "State ~d:~%" (state-id state))
	     (loop for sitem across (state-all-items state)
		for item = (sitem-item sitem)
		for preds = (sitem-preds sitem)
		do
		  (print-item! item preds))
	     (terpri)))

					;      (print (length states-vec))

      ;; ------- Print Includes -----
      (when print-includes
	(loop for state across states-vec do
	     (loop for sitem across (state-all-items state)
		for item = (sitem-item sitem)
		for preds = (sitem-preds sitem)
		for derives = (aref items-derives item)
		when (>= derives no-t)	; Nonterminal
		when (= 0 (aref items-nullrest (aref items-next item))) do
		  (format t "(State ~d,~S) includes"
			  (state-id state) (aref rev-nt derives))
		  (loop for pred in preds
		     for id = (state-id pred) do
		       (format t " (State ~d,~S)" 
			       (state-id pred) (aref rev-nt (aref rr-back (aref items-back item)))))
		  (terpri))))
					;
      (let* ((nstates (length states-vec))
	     (trans-vec (make-array ntrans))
	     (states-ntcount (make-array no-n :initial-element 0 :element-type 'fixnum))
	     (states-mash (make-array no-n :initial-element #())))
	;; In order to run the scc we need a plain vector
	;; The forward reference is easy: just store as each element a
	;; vector of #(current state, Nonterminal-Transition, Goto-State)
	;; For the reverse case a mash is set up that works like a hash:
	;; state-id is key, the position in trans-vec the value
	;; Because we loop over states in the outer loop the
	;; state-mash vectors are sorted
	(loop for state across states-vec
	   for state-id = (state-id state) do
	     (loop for trans across (state-trans state)
		for sym = (aref trans 0)
		when (>= sym no-t) do
		  (incf (aref states-ntcount (- sym no-t))))
	   finally
	     (loop for count across states-ntcount
		for k from 0
		do
		  (setf (aref states-mash k) (bisect:create-trie count))))
	(loop for state across states-vec with j = -1 for state-id = (state-id state) do
	     (loop for trans across (state-trans state)
		for sym = (aref trans 0)
		for r = (aref trans 1)
		when (>= sym no-t) do
		  (setf (aref trans-vec (incf j)) (vector state sym r))
		  (bisect:insert state-id j (aref states-mash (- sym no-t)))))

	;; Read
	(let* ((read (make-array ntrans :initial-element (vector) :element-type 'vector))
	       (init (lambda (v)
		       ;; on init initialize with the DR symbols, i.e. terminal transitions
		       ;; in the follow state
		       (let* ((par (aref trans-vec v))
			      (r (aref states-vec (aref par 2))))
			 (loop for trans across (state-trans r)
			    for sym = (aref trans 0)
			    when (< sym no-t)
			    do (spadd sp-goto sym)
			    finally
			      (setf (aref read v) (spcompress! sp-goto))))))
	       (children (lambda (v)
			   (let* ((par (aref trans-vec v))
				  (r (aref states-vec (aref par 2))))
			     ;; the "reads" relation
			     ;; for children first find which nonterminals are nullable in
			     ;; the follow state; then map them back to trans-vec
			     (loop for trans across (state-trans r)
				for sym = (aref trans 0)
			     	when (aref de sym) do (spadd sp-mark sym))
			     (let* ((syms (spiter sp-mark)))
			       (prog1 (map2a syms (lambda (v) (bisect:lookup (state-id r) (aref states-mash (- v no-t)))))
				 (spreset sp-mark))))))
	       (unite (lambda (v w)
			(loop for sym across (aref read v) do (spadd sp-goto sym))
			(loop for w across w do
			     (loop for sym across (aref read w) do (spadd sp-goto sym)))
			(setf (aref read v) (spcompress! sp-goto))))
	       ;; in DeRemer/Pennello whith v root of scc, w is popped:
	       ;; F(Top of S) <- Fx
	       (pop (lambda (v w)
		      (if (/= v w)
			  (setf (aref read w) (copy-seq (aref read v)))))))
	  (scc ntrans children init nil unite pop)

	  ;; Follow
	  (let* ((sp-ntrans (spinit ntrans))
		 (follow (make-array ntrans))
		 (init (lambda (v)
			 (setf (aref follow v) (aref read v)))) ; only reference
		 (children (lambda (v)
			     ;; the "includes" relation
			     ;; one transition (v) includes another when the following
			     ;; item (after transition on nonterminal) has a nullable rest
			     (let* ((par (aref trans-vec v))
				    (state (aref par 0))
				    (sym (aref par 1)))
			       (spreset sp-ntrans)
			       (loop for sitem across (state-all-items state)
				  for item = (sitem-item sitem)
				  for preds = (sitem-preds sitem)
				  for derives = (aref items-derives item)
				  when (= derives sym) ; sym is Nonterminal
				  when (= 0 (aref items-nullrest (aref items-next item))) do
				    (loop for pred in preds
				       for id = (state-id pred) do
				       ;; the second expression is complicated because there is no data structure relating
				       ;; item and lhs, so: go back to the production and then to the head
					 (spadd sp-ntrans (bisect:lookup (state-id pred) (aref states-mash (- (aref rr-back (aref items-back item)) no-t)))))
				  finally
				    (return (spcompress! sp-ntrans))))))
		 (unite (lambda (v w)
			  (loop for sym across (aref follow v) do (spadd sp-goto sym))
			  (loop for w across w do
			       (loop for sym across (aref follow w) do (spadd sp-goto sym)))
			  (setf (aref follow v) (spcompress! sp-goto))))
		 (pop (lambda (v w)
			(if (/= v w)
			    (setf (aref follow w) (copy-seq (aref follow v)))))))
	    (spreset sp-ntrans)
	    (scc ntrans children init nil unite pop)

	    ;; LA and Parsing Table
	    (let ((goto (make-array nstates :initial-element '()))
		  (action (make-array nstates :initial-element '()))
		  (work (make-array no-t :initial-element nil))
		  (co (make-conflict :muffle-conflicts muffle-conflicts)))
	      ;; the goto's are straightforward and without conflict
	      (loop for state across states-vec do
		   (loop for trans across (state-trans state)
		      for sym = (aref trans 0)
		      for r = (aref trans 1)
		      if (>= sym no-t) do
			(push (cons (aref rev-nt sym) r) (aref goto (state-id state)))))
					;	      (sp-init no-t)
	      (loop for state across states-vec
		 for id = (state-id state)
		 do
		   (loop for trans across (state-trans state)
		      for sym = (aref trans 0)
		      for r = (aref trans 1)
		      when (< sym no-t) do ; terminal transition = shift
			(spadd sp-mark sym)
		      ;; the "action" is the shift-state
			(setf (aref work sym) r))
		   (loop for sitem across (state-all-items state)
		      for item = (sitem-item sitem)
		      for preds = (sitem-preds sitem)
		      for next = (aref items-next item)
		      when (< next 0)	; Final Item
		      do
		      ;; lookahead are just the union over all lookbacks
			(loop for pred in preds
			   with s-prime = (- s-prime no-t)
			   for sym = (- (aref rr-back (aref items-back item)) no-t)
			   for trans = (if (/= sym s-prime)
					   (bisect:lookup (state-id pred) (aref states-mash sym))
					   -1)
			   for la = (if (/= sym s-prime)
					(loop for sym across (aref follow trans) do (spadd sp-goto sym))
					(values)) then (loop for sym across (aref follow trans) do (spadd sp-goto sym))
			   finally
			   ;; Final item and S-Prime is head -> final state
			     (when (= s-prime sym)
			       (setf accept-state id)
			       (push (cons '-eof- 0) (aref action id)))
			     (loop for sym across (spcompress! sp-goto)
				with rule = (- (aref items-back item) no-s)
				do
				  (let ((s-a (aref work sym)))
				    (spadd sp-mark sym)
				    (cond
				      (s-a ; there is already an action saved for this symbol
				       ;; we have a conflict
				       (resolve-conflict co rule s-a sym (state-all-items state) (state-id state) rr-production no-s items-derives items-back prec rev-nt)
				       (setf (aref work sym) (conflict-action co)))
				      (t (setf (aref work sym) (- rule)))))))) ; the "action" is the rule

		 ;; now the actions can be set and the work array be cleared for the next state
		   (loop for sym across (spiter sp-mark)
		      do
			(push (cons (aref rev-nt sym) (shiftf (aref work sym) nil)) (aref action id))
		      finally
			(spreset sp-mark)))

	      (setf (conflict-used-productions co) no-used-p)

	      (warnings-and-unused-productions
	       co muffle-warnings force-check
	       no-t no-n no-s no-p nstates
	       rr-back rr de rev-nt
	       action goto
	       grammar
	       print-state-sentential-forms)
	      (prepare-and-return
	       no-p no-s no-nn rr
	       action goto accept-state
	       grammar co))))))))



;; finally check for conflicts and required warnings
;; first come unreachable states and unused productions
;; they can only occur due to conflict resolution
;; It is computed by a fixed-point iteration over the closure of
;; states reachable be shifts or gotos on used non-terminals
(defun warnings-and-unused-productions (co muffle-warnings force-check
					no-t no-n no-s no-p nstates
					rr-back rr de rev-nt
					action goto
					grammar
					print-state-sentential-forms)
  (let ((conflict-p (conflict-exist-p co))
	(muffle-conflicts (conflict-muffle-conflicts co))
	(sr-conflicts (conflict-sr co))
	(rr-conflicts (conflict-rr co))
	(used-nons (make-array no-n :initial-element nil :element-type 'boolean))
	(used-ts (make-array no-t :initial-element nil :element-type 'boolean))
	(changed-p nil))
    (when (or (and conflict-p (not muffle-warnings))
	      force-check print-state-sentential-forms)
      (let* ((used-productions (make-array no-p :initial-element nil))
	     (used-states (make-array nstates :initial-element nil))
	     (transitions
	      (lambda (state)
		(concatenate
		 'vector
		 (loop for shift in (aref action state)
		    for sym = (geth (car shift))
		    do (setf (aref used-ts sym) t)
		    if (cdr shift)
		    if (> (cdr shift) 0) collect (cdr shift)
		    else do
		      (let* ((production (- (cdr shift)))
			     (nt (- (aref rr-back (+ no-s production)) no-t)))
			(setf (aref used-productions production) t)
			(unless (aref used-nons nt)
			  (setf (aref used-nons nt) t
				changed-p t))))
		 (loop for go in (aref goto state)
		    when (aref used-nons (- (geth (car go)) no-t))
		    collect (cdr go))))))
	(loop
	     for i from 1
	   do
	     (setf changed-p nil)
	     (closure-init nstates)
	     (loop for state across (closure transitions 0)
		do
		  (setf (aref used-states state) t))
	     while changed-p)
	(setf (aref used-nons 0) t) ; S-Prime
	(setf (grammar-used-terminals grammar) (loop for val across used-ts count val)
	      (grammar-used-nonterminals grammar) (loop for val across used-nons count val))
	(setf (aref used-states 0) t)	     ; Start state
	(setf (aref used-productions 0) t) ; S-Prime production
	(setf (conflict-used-productions co) (loop for val across used-productions count val))
	(let ((text (make-string-output-stream)))
	  (loop for used across used-productions
	     for production in (grammar-productions grammar)
	     count used into used-productions
	     unless used do
	       (format text "Unused production: ~a~%" production)
	     finally
	       (setf (conflict-used-productions co) used-productions))
	  (loop for used across used-states
	     for k from 0
	     unless used do
	       (format text "Unreachable state ~d~%" k))
	  (let ((text (get-output-stream-string text)))
	    (unless (equal text "")
	      (warn (make-condition
		     'buffalo-muffable-warning
		     :text text)))))
	(when (conflict-conflict-states co)
	  (let ((que (qu-init nstates))
		(path (make-array nstates :initial-element '() :element-type 'list))
		(text (make-string-output-stream))
		(derives (knuth-dijkstra no-n no-t rr rr-back rev-nt)))
	    (qu-push que 0)
	    (loop
	       for s = (and (qu-in-p que) (qu-pop que))
	       while s
	       do
		 (loop for (sym . nextstate) in (aref action s)
		    unless (or
			    (not nextstate)
			    (< nextstate 0)
			    (not (aref used-ts (geth sym)))
			    (aref path nextstate)) do
		      (setf (aref path nextstate) (cons sym (aref path s)))
		      (qu-push que nextstate))
		 (loop for (sym . nextstate) in (aref goto s)
		    unless (or
			    (not nextstate)
			    (not (aref used-nons (- (geth sym) no-t)))
			    (aref path nextstate)) do
		      (setf (aref path nextstate) (cons sym (aref path s)))
		      (qu-push que nextstate)))
	    (when (conflict-conflict-states co)
	      (format text "Conflict explanation:~%")
	      (loop for (state . sym) in (reverse (conflict-conflict-states co))
		 do
		   (format text "Derived sentence for state ~d:~%" state)
		   (format text "Sentential form:~%")
		   (loop for s in (reverse (aref path state))
		      with text-n = ""
		      with text-t = ""
		      with max = 0
		      when (< (geth s) no-t) do
			(setf text-t (format nil "\"~a\" " s))
			(format text "~a" text-t) and
		      collect text-t into list and
		      collect (length text-t) into len
		      else do
			(setf text-n (format nil "~a " s))
			(setf text-t (format nil "~{\"~a\" ~}"
					     (loop for sym in (aref derives (- (geth s) no-t))
						collect (aref rev-nt sym))))
			(setf max (max (length text-n) (length text-t)))
			(format text "~va" max text-n)
		      and collect text-t into list
		      and collect max into len
		      end
		      finally
			(format text ". ~s~%" sym)
			(format text "Terminal form:~%")
			(loop for ts in list
			   for l in len do
			     (format text "~va" l ts))
			(format text ". ~s~%~%" sym)))
	      (let ((text (get-output-stream-string text)))
		(warn (make-condition
		       'buffalo-muffable-warning
		       :text text))))
	    (when print-state-sentential-forms
	      (format t "Sentential forms~%")
	      (loop for state from 1 below nstates
		 when (aref used-states state)
		 do
		   (format t "Sentential form for state ~d: " state)
		   (loop for s in (reverse (aref path state)) do
			(if (< (geth s) no-t)
			    (format t "\"~a\" " s)
			    (format t "~a " s)))
		   (format t "~%")))))))
    ;; second come the '%expect'ed conflicts
    (when (null muffle-conflicts) (setq muffle-conflicts '(0 0)))
    (unless (or (eq t muffle-conflicts)
		(and (consp muffle-conflicts)
		     (= (car muffle-conflicts) sr-conflicts)
		     (= (cadr muffle-conflicts) rr-conflicts)))
      (warn (make-condition 'conflict-summary-warning
			    :shift-reduce sr-conflicts
			    :reduce-reduce rr-conflicts)))))


(defun prepare-and-return (no-p no-s no-nn rr
			   action goto accept-state
			   grammar co)
  (let ((rule-array (make-array no-p :initial-element -1))
	(head-array
	 (coerce (mapcar #'production-symbol (grammar-productions grammar)) 'vector)))
    (loop for k from no-s below no-nn
       for j from 0
       do
	 (setf (aref rule-array j) (length (aref rr k))))
    (values
     (list rule-array head-array action goto accept-state)
     `(:terminals ,(grammar-no-terminals grammar)
		  :used-terminals ,(grammar-used-terminals grammar)
		  :nonterminals ,(grammar-no-nonterminals grammar)
		  :used-nonterminals ,(grammar-used-nonterminals grammar)
		  :productions ,no-p
		  :used-productions ,(conflict-used-productions co)
		  :states ,(length action)
		  :resolved-conflicts ,(- (conflict-total co) (conflict-sr co) (conflict-rr co))
		  :shift-reduce-conflicts ,(conflict-sr co)
		  :reduce-reduce-conflicts ,(conflict-rr co)))))


;; a1 is by design a reduce action (still positive!)
;; (on rule 0 there can never be a conflict, so 0 is a state)
;; a2 is then either a reduction (<=0) or a shift action (>0)
(defun resolve-conflict (co a1 a2 sym state-items state-id rr-production no-s items-derives items-back prec rev-nt)
  (labels ((get-production (a)
	     (aref rr-production (+ no-s a)))
	   (find-production (s)
	     (loop for sitem across state-items
		for item = (typecase sitem
			     (sitem (sitem-item sitem))
			     (fixnum sitem))
		for derives = (aref items-derives item)
		when (= derives s)
		return 
		  (aref rr-production (aref items-back item))))
	   (main ()
	     (cond
	       ((<= a2 0)		; Reduce-reduce conflict
		;; productions are not sorted or removed, so a1 and a2 are the literal position
		;; in the grammar
		(cond
		  ;; ((> (aref (aref prec (+ no-s (- a2))) 0) (aref (aref prec (+ no-s a1)) 0))
		  ;;  (values a2 0 0 nil))
		  ;; ((< (aref (aref prec (+ no-s (- a2))) 0) (aref (aref prec (+ no-s a1)) 0))
		  ;;  (values a1 0 0 nil))
		  (t
		   (if (> a1 (- a2))
		       (values a2 0 1 `("Reduce/Reduce" ,(get-production a1) ,(get-production (- a2))))
		       (values (- a1) 0 1 `("Reduce/Reduce" ,(get-production a1) ,(get-production (- a2))))))))
	       (t			; a1 reduce, a2 shift
		(let* ((prec1v (aref prec (+ no-s a1)))
		       (prec1 (aref prec1v 0))
		       (prec2v (aref prec sym))
		       (prec2 (aref prec2v 0)))
		  (if (and (> prec1 0)
			   (> prec2 0))
		      ;; only if both action and terminal have precedence associated with them
		      ;; buffalo remains silent
		      (cond
			((> prec1 prec2)
			 (values (- a1) 0 0 nil))
			((> prec2 prec1)
			 (values a2 0 0 nil))
			(t
			 (ecase (aref prec2v 1)
			   ((:left) (values (- a1) 0 0 nil))
			   ((:right) (values a2 0 0 nil))
			   ((:nonassoc) (values nil 0 0 nil))
			   ((:precedence) (values a2 1 0 `("Shift/Reduce" ,(find-production sym) ,(get-production a1)))))))
		      ;; that's the default
		      (values a2 1 0 `("Shift/Reduce" ,(find-production sym) ,(get-production a1)))))))))
    (multiple-value-bind (new-action s-r r-r conflict-p) (main)
      (when (and conflict-p (not (conflict-muffle-conflicts co)))
	(warn (make-condition
	       'conflict-warning
	       :text
	       (format *error-output* "~A conflict on terminal \"~S\" in state ~A, ~_~?"
		       (first conflict-p) (aref rev-nt sym) state-id
		       "~S and ~S"
		       (list (second conflict-p) (third conflict-p)))))
	(push (cons state-id (aref rev-nt sym)) (conflict-conflict-states co)))
      (incf (conflict-sr co) s-r)
      (incf (conflict-rr co) r-r)
      (let ((conflict-p (if conflict-p t)))
	(setf (conflict-action co) new-action
	      (conflict-exist-p co) (or (conflict-exist-p co) conflict-p)))
      (incf (conflict-total co))
      co)))



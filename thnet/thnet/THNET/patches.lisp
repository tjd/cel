; uncomment next line for cm version
;(in-package '*lisp)

; FILE:       patches
; PURPOSE:    patch bugs in ARCS/ACME
; PROGRAMMER: Eric Melz
; CREATED:    6-18-89
; UPDATED:    12-12-90

;; These variables are used to print non-asymptoted units (see corresponding functions
;; below)

(defvar *print-asymp-last* nil)
(defvar *print-asymp-all* nil)

(defun print_asymp_last nil
  (my_print "Will print non-asymptoted units at end of run.")
  (setf *print-asymp-last* t))

(defun print_asymp_all nil
  (my_print "Will print out non-asymptoted units throughout the run.")
  (setf *print-asymp-all* t))


;; PUT patch (from misc.lisp)
;; For some reason, put sometimes returns the error
;; >>Error: PUT cannot be called with 3 arguments
;; I think there is probably a bug with compiled macros.

(defun put (atm prop val)
  (setf (get atm prop) val))


;; PRESUMED patch (from analogy.lisp)
;; This version of presumed includes a feature which allows non-existent units
;; to be presumed mappings.  If the source part of the new mapping exists but
;; the target doesn't, the new unit is "created", and inhibitory links are
;; set up between other mapping units which map from the source.
;; If the target part of the new mapping exists but the source doesn't,
;; the new unit is created, and inhibitory links are set up between the new
;; unit and other units which contain the target if the variable map_one_one
;; is T.  Of course, if the new unit doesn't contain an existing source
;; OR an existing target element, it is just thrown away.

(defun presumed (list_of_units)
   (create_pragmatic_unit)
   (do ((units list_of_units (cdr units)))
       ((null units) 'done)
       ; repeat:
     (let* ((unit (car units))
	    (src (source_map_from_unit unit))
	    (tar (target_map_from_unit unit)))
     (cond
       ((member unit all_units)  ;; Normal case (unit exists)
	  (unless silent_map? (my_print units '" is a presumed mapping."))
          (make_sym_link 'pragmatic unit prag_weight))
       ;; Second case: Source component exists
       ((or (member src (conc_from_struc *struc1*))
	    (member src (obj_from_struc *struc1*))
	    (member src all_propositions))
	(unless silent_map? (my_print unit '" is a presumed mapping (target component is new)."))
	;; Create the new unit
	(make_presumed_type_unit src tar)
	(if (atom_begins src #\?)  ; query
            (excit_queries src)
	    ;; otherwise:
            (make_inhib_links (no_queries (get src 'constraint_hyps))
			      inhib_weight))
	(make_sym_link 'pragmatic (car units) prag_weight))
       ;; Third case: Target component exists
       ((or (member tar (conc_from_struc *struc2*))
	    (member tar (obj_from_struc *struc2*))
	    (member tar all_propositions))
	(unless silent_map? (my_print unit '" is a presumed mapping (source component is new)."))
	;; Create the new unit
	(make_presumed_type_unit src tar)
	(if (atom_begins src #\?)  ; query
            (excit_queries src)
	    ;; otherwise:
	    (if map_one_one? 
		(make_inhib_links (no_queries (get tar 'constraint_hyps))
				  inhib_weight)))
	(make_sym_link 'pragmatic (car units) prag_weight))))))


;;; This function is used by presumed (above) to create a new unit if a non-existent
;;; one is specified by the presumed function.
;;; Most of this is borrowed from make_hyp_unit (in acme.lisp)

(defun make_presumed_type_unit (src tar)
  (let ((sem_sim? nil))
    (setq new_unit (catname src tar))
    ;; Select "type" of unit (conc, pred, or obj)
    (cond 
      ;; Concept unit or proposition unit
      ((or (member src (conc_from_struc *struc1*)) 
	   (member tar (conc_from_struc *struc2*))
	   (member src all_propositions)
	   (member tar all_propositions))
       ;; set up unit
       (put new_unit 'concerns (list src tar))
       ;; note creation of unit:
       (note_unit new_unit)
       ;; calculate semantic similarity of concepts if concept unit
       (if (or (member src (conc_from_struc *struc1*))
	       (member tar (conc_from_struc *struc2*)))
	   (progn
	     (setq sem_sim?
		   (if use_arcs_semantics?
		       ;; Get similarity from table if parallel
		       (if *starsearch-loaded* 
			   (progn
			     (cadr (assoc tar *sim-list*))
			     (my_print "getting parallel similarities"))
			   (sem_sim_arcs src tar))
		       (sem_similarity src tar)))
	     (if sem_sim?
		 (make_sym_link 'special new_unit  
				sem_sim?))))
       ;; record hypotheses about a concept:
       (record_hypothesis src new_unit)
       (record_hypothesis tar new_unit))
      ;; Object unit:
      (t (make_obj_unit src tar default_activation)))))



;; These 2 functions get what's to the left or right of the = for a mapping unit.


(defun source_map_from_unit (unit)
  (let ((s (princ-to-string unit)))
    (read-from-string (subseq s 0 (search "=" s)))))


(defun target_map_from_unit (unit)
  (let ((s (princ-to-string unit)))
    (read-from-string (subseq s (1+ (search "=" s))))))


;; This is a patch to make acme run on starlisp (string-append is undefined for this dialect)
;;    -Eric 11/29/88 

(defun string-append (&rest strings)
  "Append a groups of strings together "
  (prog ((num-of-letters 0)
        (result nil)
        (current-char 0)
	(new-strings (mapcar 'princ-to-string strings)))
    (dolist (string new-strings nil)
       (setq num-of-letters (+ num-of-letters (length string)))
    )
    ;Make a null string, then copy each string over it 
    (setq result (make-string num-of-letters :initial-element #\x))
    (dolist (string new-strings nil)
      (dotimes (i (length string))
         (setf (schar result current-char) (schar string i))
         (incf current-char)
    ) )
    (return result)))


;; run-program is undefined in starlisp

(defun run-program (&rest x) NIL)

;; for debugging parallel stuff, namely excitation/inhibition problem

(defun debug-hook nil
    (my_print "debug report on cycle " total_times ":")
    (cond 
      (*staracme-loaded* 
       (*all
       (dotimes (i (length all_units))
	 (when (equal (pref *unit i) 'band=plants)
	   (my_print "parallel")
	   (my_print "excit = " (pref *excit-net i))
	   (my_print "inhib = " (pref *inhib-net i))
	   (my_print "activation = " (pref *unit-activation i))))))
      (t (excit_and_inhib 'band=plants)
	 (my_print "serial")
	 (my_print "excit = " current_excit)
	 (my_print "inhib = " current_inhib)
	 (my_print "activation = " (get 'band=plants 'activation)))))


;;; The original BEST_MATCH procedure has a bug where if the concept or object
;;; is in the target, it will also consider these mappings (i.e. from target
;;; to source rather than from source to target).  This patch fixes that by
;;; adding an extra "filter" to the form (get conc_or_obj 'constraint_hyps),
;;; namely, it only retrieves units in which conc_or_obj is the first element
;;; of the mapping unit.

(defun get_firsts (conc_or_obj units)
  (let ((ret nil))
    (dolist (unit units)
      (when (eq (first (get unit 'concerns)) conc_or_obj)
	 (setf ret (cons unit ret))))
    ret))

;;; get_seconds is like get_firsts except it does it for the target
(defun get_seconds (conc_or_obj units)
  (let ((ret nil))
    (dolist (unit units)
      (when (eq (second (get unit 'concerns)) conc_or_obj)
	 (setf ret (cons unit ret))))
    ret))


; **********************************************************
; BEST_MATCH figures out what concept or object has been found to be
; the best match.

(defun best_match (conc_or_obj) 
  (let (bm best_unit other_good_units)
    (setq best_unit (highest-l (get_firsts conc_or_obj
					   (get conc_or_obj 'constraint_hyps))
			       'activation
                    )
    )
    (if (listp best_unit)
	(setq best_unit (car (setq other_good_units best_unit)))
    )
    (setq bm (other_from_pair conc_or_obj (get best_unit 'concerns)))
    (my_print "Best mapping of " conc_or_obj " is " bm ". " 
	      (get best_unit 'activation)
    )
    (if other_good_units
	(do ((units (cdr other_good_units) (cdr units)))
	    ((null units) nil)
	    (setq bm (other_from_pair conc_or_obj (get (car units) 'concerns)))
	    (my_print "                tied with " bm ".")
        )
    )

    (if show_others?
	(good_enough_s conc_or_obj 
		       (remove best_unit
			       (get_firsts conc_or_obj
					   (get conc_or_obj 'constraint_hyps))
		       )
        )
    )
    (setq best_matches (cons (list conc_or_obj bm) best_matches))
  )
)




;;; 7/17/89: This version of print_run prints out non-asymptoted units.

(defun print_run (mode &optional verbose)
  (my_print "patched print_run")
  (cond (silent_run?)
	(t 
	 ;; Update "serial" activations if in parallel mode
	 (when *staracme-loaded*                
	     (get_parallel_act))
	     
	 (my_print '"Test: " testnum 
		   '"   Total times: " total_times
		   )
	 (unix "date")
	 (my_print experiment)  ; description
	 (my_print '"Units not yet reached asymptote: "
		   (length (set-difference all_units
					   asymptoted_units 
					   )
			   )
		   )
           (cond ((eq mode 'echo)  ; explanation
		  (print_cohere))
		 ((eq mode 'acme) (print_good) (best_analogy *struc1*))
		 ((eq mode 'arcs) (print_good) (best_retrieval *struc1* verbose))
           )
	   (if (and *transfer-fields* *transfer-type*) (transfer))
	   (when (or (and *print-asymp-last* (eq total_times (last when_to_print)))
		     *print-asymp-all*)
		 (my_print "These units have not reached asymptote: ")
		 (mapcar #'my_print (set-difference all_units asymptoted_units))))))



;;; 7/25/89: This version also calculates the analogical mappings for propositions
(defun best_analogy (prob)
   (unless silent_run?
	   (my_print '"Calculating the best mappings after "
		     total_times '" cycles."
           )
   )
   (setq best_matches nil)
   (mapcar #'best_match (union (conc_from_struc prob)
			       (union (propns_from_struc prob)
				      (obj_from_struc prob))
                        )
   )
   (if stop_when_matched?
       (cond ( (null (set-difference desired_matches best_matches 
                                     :test #'equal
                     )
               )
               (setq stop_run? t)
	       (unless silent_run?
		       (my_print '"Desired match accomplished at cycle "
				 total_times
                       )
               )
             )
       )
    )
)


(defun debug_units (units)
  (let ((unit_num_list nil))
    (when *staracme-loaded*
	  (*all
	   (*when *unit-p
		  (do-for-selected-processors (i)
		     (when (member (pref *unit i) units)
			(setf unit_num_list (cons i unit_num_list))))))
	  (my_print "unit num list: " unit_num_list))
    (defun debug_run () 
      (cond 
       (*staracme-loaded*
	(dolist (un unit_num_list)
		(my_print total_times '"  Activation of " (pref *unit un) '" is " 
                 (pref *unit-activation un))))
       (t
	(do ((unts units (cdr unts)))
	    ((null unts) 'done)
	    (my_print total_times '"  Activation of " (car unts) '" is " 
		      (get (car unts) 'activation))))))))

;;; This debug_els uses the get_firsts routine to filter irrelevant units

(defun debug_els (elements)
   (debug_units (union_map #'get_constraints elements))
)

(defun get_constraints (el) 
   (get_firsts el (get el 'constraint_hyps))
)

;;; Routines to printout excitatory and inhibitory groups for a unit 

(defun test1 (x y) (> (cadr x) (cadr y)))

(defun test2 (x y) (< (cadr x) (cadr y)))

;;; excit filter
(defun e (x) (if (> (cdr x) 0) x))

;;; inhib filter
(defun i (x) (if (< (cdr x) 0) x))

;;; Get weighted activation and filter units under threshold
(defun m (x) (if (> (get (car x) 'activation) output_threshold)
		 (list (car x) (* (get (car x) 'activation)) (cdr x))))

;;; Print excitatory group for a unit
(defun pre (unit)
  (format t "Excitatory group for ~a~%~%" unit)
  (let ((elist (sort (remove nil 
			     (mapcar 'm
				     (remove nil
					     (mapcar 'e (get unit 'links_from)))))
		     'test1)))
    (dolist (x elist)
      (format t "Weighted activation of ~25A is ~6,4f, weight ~5,3,f~%" 
	      (car x) (cadr x) (caddr x)))))

;;; Print inhibitory group for a unit
(defun pri (unit)
  (format t "Inhibitory group for ~a~%~%" unit)
  (let ((ilist (sort (remove nil 
			     (mapcar 'm
				     (remove nil
					     (mapcar 'i (get unit 'links_from)))))
		     'test2)))
    (dolist (x ilist)
      (format t "Weighted activation of ~25A is ~6,4f, weight ~5,3,f~%" (car x) 
	      (cadr x) (caddr x)))))

;;; This fixes a bug which mixes up source and target components when considering
;;; inhibitory connections.  For example, num5=num3 is inhibited by num6=num5.

;(defun inhibit_multiple_mappings (list_of_objs_or_concs weight)
;   (do ((lst (remove_nil_dup list_of_objs_or_concs) (cdr lst)))
;        ; exit:
;       ((null lst) total_links)
;       (if (atom_begins (car lst) #\?)  ; internal-structure query
;           (excit_queries (car lst))
;	   (progn                       ; not a query
;	    (make_inhib_links 
;	      (no_queries 
;		(get_firsts (car lst) (get (car lst) 'constraint_hyps)))
;	      weight)
;	    (make_inhib_links
;	      (no_queries
;		(get_seconds (car lst) (get (car lst) 'constraint_hyps)))
;	      weight))
;	  )
;    )
;)

; IS_QUERY
; added the atom_ends clause for cross-structure queries

(defun is_query (atm)
   (or (atom_ends atm #\?) (atom_begins atm #\?))
)


(defun inhibit_multiple_mappings (list_of_objs_or_concs weight)
   (do ((lst (remove_nil_dup list_of_objs_or_concs) (cdr lst)))
        ; exit:
       ((null lst) total_links)
       (if (atom_begins (car lst) #\?)  ; internal query
           (excit_queries (car lst))
           ; otherwise:
	 (if (atom_ends (car lst) #\?)  ; cross structure query
	     (progn
	       (make_inhib_links 
		(get_firsts (car lst) (get (car lst) 'constraint_hyps))
		weight)
	       (make_inhib_links 
		(get_seconds (car lst) (get (car lst) 'constraint_hyps))
		weight))
	   (progn                       ; regular (no query) atom
	     (make_inhib_links 
	      (no_queries 
	       (get_firsts (car lst) (get (car lst) 'constraint_hyps)))
	      weight)
	     (make_inhib_links
	      (no_queries
	       (get_seconds (car lst) (get (car lst) 'constraint_hyps)))
	      weight))))))


;; This checks to see if new_propn_unit already exists.

(defun make_hyp_unit (msg1 msg2)
  (let ((conc1 (car msg1))
	(conc2 (car msg2))
	(propn1 (last_element msg1))
	(propn2 (last_element msg2))
	(args1 (get_args msg1))
	(args2 (get_args msg2))
	(new_conc_unit nil)
	(new_propn_unit nil)
	(result nil)
	(object_units nil)
	(sem_sim? nil)
       )
       ; if same # args:
    (cond ((and (= (length args1) (length args2)) ; cond 1
                ; don't match objects and propositions:
                (type_compatible args1 args2 all_propositions)
           )
	   (setq new_conc_unit (catname conc1 conc2))
           ; if concept unit not already made, create it:
                (cond ( (not_member new_conc_unit all_units)   ; cond 2
                        ; set up unit
                        (put new_conc_unit 'concerns (list conc1 conc2))
                        ; note creation of unit:
                        (note_unit new_conc_unit)
                        (setq result (cons new_conc_unit result))
                        ; calculate semantic similarity of concepts
			
			(setq sem_sim?
			      (if use_arcs_semantics?
				  ;; Get similarity from table if parallel
				  (if *starsearch-loaded* 
				      (progn
					(my_print "getting parallel similarities")
					(cadr (assoc conc1 *sim-list*)))
				      (sem_sim_arcs conc1 conc2))
				  (sem_similarity conc1 conc2)))
			(if sem_sim?
			    (make_sym_link 'special new_conc_unit  
					   sem_sim?))

 
                         ; record hypotheses about a concept:
                         (record_hypothesis conc1 new_conc_unit)
                         (record_hypothesis conc2 new_conc_unit)
                      )
                ); end cond 2
                ; create proposition unit:
                (setq new_propn_unit (catname propn1 propn2))
                (cond ( (not_member new_propn_unit all_units)  ; new patch -eric 8/21/89
		       (put new_propn_unit 'concerns (list propn1 propn2))
			(note_unit new_propn_unit)
			(record_hypothesis propn1 new_propn_unit)
			(record_hypothesis propn2 new_propn_unit)
			(setq result (cons new_propn_unit result))))
                ; link proposition unit with concept unit:
                (make_sym_link new_propn_unit new_conc_unit excit_weight)

                ; if msg2 from target contains a query, treat specially:
                (cond ( (and look_for_queries?  ; cond 3
                             (contains_query (get_args msg2))
                        )
                        (setq query_connections 
                              (cons (list new_propn_unit
                                          new_conc_unit 
                                          (get_args msg1) 
                                          (get_args msg2)
                                    )
                                    query_connections
                               )
                         )
                       )
                       ; else make excitatory links with objects:
                       (t (setq object_units (make_obj_units (get_args msg1)
                                                             (get_args msg2)
                                                             default_activation
                                              )
                           )
                           (make_excit_links new_propn_unit
                                             object_units
                                             excit_weight
                           )
                           (if link_concepts_objects? 
                               (make_excit_links new_conc_unit
                                                 object_units
                                                 excit_weight
                               )
                            )
                        )
                ) ; end cond 3
            )
        ) ; end cond 1
    ) ; end let
)


;;; This new version of EXCIT_QUERIES does no set up excitatory links between 
;;; target elements which contain a query.

; ***************************************************
; EXCIT_QUERIES sets up special excitatory links for
; query objects starting with "?".
; Excitatory links are set up between each query object mapping to
; an element and all units involving that element.

(defun excit_queries (quer)
   (do ((quer_maps (get quer 'constraint_hyps) (cdr quer_maps)))
       ((null quer_maps) 'done) ; (important (list quer)) - retards settling.
       ; repeat:
       (my_print (car quer_maps) " query map")
       (make_excit_links (car quer_maps)
                         (remove_queries_from_list 
			   (other_element_maps quer (car quer_maps)))
                         excit_weight
       )
       (make_inhib_links (get quer 'constraint_hyps) inhib_weight)
   )
)    

(defun remove_queries_from_list (l)
  (cond ((null l) nil)
	((contains_query (list (car l))) (remove_queries_from_list (cdr l)))
	(t (cons (car l) (remove_queries_from_list (cdr l))))))



;;; This version of make_hyp_units_for_msg adds the starsearch feature (if starsearch
;;; is loaded), and should be incorporated into the compiled version of ACME if it works
;;; OK.

; ********************************************************
; MAKE_HYP_UNITS_FOR_MSG makes units for a particular message.

(defun make_hyp_units_for_msg (msg messages)
   ; set up null mappings:
   (if use_nothing_maps?
       (make_hyp_unit msg 
                     (list 'no-concept 
                           (list_of_n_elem (length (get_args msg))
                                           'no-object
                           )
                           'no-proposition
                     )
        )
   )
   ; Do Parallel search if running on CM (returns results in *sim-list*)
   (if (and use_arcs_semantics? *starsearch-loaded*)
       (progn
	 (my_print "calling parallel search")
(my_print "1")
	 (sem_sim!! msg messages)
(my_print "2")))
   ; all the rest:
   (do ((msgs messages (cdr msgs)))
        ; exit:
        ((null msgs) 'done)
        ; repeat:
        (make_hyp_unit msg (car msgs))
    )
)


;; This fixes a bug in the importance function, where if an important object 
;; occurs in both the source and the target (e.g. the predicate "increased"),
;; the old importance routine would give double weight to *different* predicates
;; (.e.g. increased=flow-rate and flow-rate=increased, where increased and
;; flow-rate are both important), but only single weight to *identical* predicates
;; (e.g. increased=increased).

;; I now have two separate importance routines, source_importance
;; and target_importance, which allows units involving only occurences
;; of a particular item in a particular analog to be marked important

; *********************************************************
; IMPORTANT indicates that the system has a special interest
; in an element (concept, object, proposition) although it
; does not desire any particular map.

(defun important (list_of_els)
  (source_important list_of_els)
  (target_important list_of_els))

(defun source_important (list_of_els)
   (create_pragmatic_unit)
   (do ((els  list_of_els (cdr els)))
       ((null els) 'done)
       ;
       (unless silent_map? (my_print (car els) " is an important element."))
       (make_excit_links 'pragmatic 
                         (get_firsts (car els) (get (car els) 'constraint_hyps) )
                         import_weight
       )
   )
)

(defun target_important (list_of_els)
   (create_pragmatic_unit)
   (do ((els  list_of_els (cdr els)))
       ((null els) 'done)
       ;
       (unless silent_map? (my_print (car els) " is an important element."))
       (make_excit_links 'pragmatic 
                         (get_seconds (car els) (get (car els) 'constraint_hyps) )
                         import_weight
       )
   )
)


; *********************************************************
; target_inhib_important and source_inhib_important
; is a new version of important which *inhibits* everything
; that is not deemed important, rather than exciting important
; elements.  
; Propositions are treated specially, by applying the routine to
; all elements in the proposition.

(defun target_inhib_important (list_of_els &optional unimportant-weight)
  (setf *inhibiting-unimportant* t)
  (create_pragmatic_unit)
  (let ((all_elt_list nil)
	(loe list_of_els)
	(leftovers nil)
	(leftover-els nil)
	(units nil))
    (if unimportant-weight
	(format t "Overriding unimportance value of ~a with ~a.~%" (* -1 import_weight) unimportant-weight)
	(progn
	  (setf unimportant-weight (* -1 import_weight))
	  (format t "Unimportant weight is ~a.~%" unimportant-weight)))
    (dolist (p all_propositions)
      (setf all_elt_list (union (cadr (get p 'message)) 
				all_elt_list)))
    (setf all_elt_list (append all_propositions all_elt_list))
    (setf all_elt_list (append all_preds all_elt_list))
    (dolist (el list_of_els)
      (if (member el all_propositions)
	  (setf loe (append (cons (car (get el 'message))
				  (cadr (get el 'message)))
			    loe))))
    (setf leftovers (set-difference all_elt_list loe))
    (dolist (e leftovers)
      (setf units (append (get_seconds e (get e 'constraint_hyps))
			  units)))
    
    (setf leftover-els (remove-duplicates
			(mapcar 'cadr
				(mapcar 
				 '(lambda (x) 
				    (get x 'concerns)) units))))
    (if (not (set-difference all_units units)) 
	(format t "Target important: Elements not found in target structure.~%")
	;; set up inhibitory links between all units and the pragmatic
	;; unit
	(progn
	  (dolist (e leftover-els)
	    (format t "~a is unimportant~%" e))
	  (make_inhib_links_for_unit 'special units unimportant-weight))))
  (setf *inhibiting-unimportant* nil))

(defun source_inhib_important (list_of_els &optional unimportant-weight)
  (setf *inhibiting-unimportant* t)
  (create_pragmatic_unit)
  (let ((all_elt_list nil)
	(loe list_of_els)
	(leftovers nil)
	(leftover-els nil)
	(units nil))
    (if unimportant-weight
	(format t "Overriding unimportance value of ~a with ~a.~%" (* -1 import_weight) unimportant-weight)
	(progn
	  (setf unimportant-weight (* -1 import_weight))
	  (format t "Unimportant weight is ~a.~%" unimportant-weight)))
    (dolist (p all_propositions)
      (setf all_elt_list (union (cadr (get p 'message)) 
				all_elt_list)))
    (setf all_elt_list (append all_propositions all_elt_list))
    (setf all_elt_list (append all_preds all_elt_list))
    (dolist (el list_of_els)
      (if (member el all_propositions)
	  (setf loe (append (cons (car (get el 'message))
				  (cadr (get el 'message)))
			    loe))))
    (setf leftovers (set-difference all_elt_list loe))
    (dolist (e leftovers)
      (setf units (append (get_firsts e (get e 'constraint_hyps))
			  units)))
    (setf leftover-els (remove-duplicates
			(mapcar 'car 
				(mapcar 
				 '(lambda (x) 
				    (get x 'concerns)) units))))
    (if (not (set-difference all_units units)) 
	(format t "Source important: Elements not found in source structure.~%")
	;; set up inhibitory links between all units and the pragmatic
	;; unit
	(progn
	  (dolist (e leftover-els)
	    (format t "~a is unimportant~%" e))
	  (make_inhib_links_for_unit 'special units unimportant-weight))))
  (setf *inhibiting-unimportant* nil))


; symmetrically sets weights
; New function added by eric from echo

(defun sym_set_weight (unit1 unit2 weight)
  (set_weight unit1 unit2 weight)
  (set_weight unit2 unit1 weight))


; ****************************************************
; MAKE_CONJUNCTIVE_LINK creates conjunctive or sigma-pi links
; between two or more "sender" units and a single recipient unit
; created by eric, imported from echo

(defun make_conjunctive_link (to_unit from_units weight &optional
back-weight)
  (put to_unit 'conjunctive_links_from 
       (cons (list from_units weight) (get to_unit
'conjunctive_links_from)))
  ;; Create links back from recpient to conjunctive senders
  ;; This isn't quite symmetrical, but it'll do for now...
  (dolist (u from_units)
    (make_link u to_unit back-weight)))

; ****************************************************
; LINK is a general purpose function which links units or groups of units.
; from_units is either a single unit or a group of units which are linked
; with to_units, again either a single unit or a group of units.
; both from_units and to_units may either be a list or an atom.
; from_units may contain sublists to indicate groups of "conjunctive",
; or "sigma-pi" type units.  e.g.
;     (link '((a b c) d) 'e .5)  
; will create a conjunctive link from the group a, b, and c to e, and a
; single
; symmetric link from d to e.
; Note that if sigma-pi units are specified, they must be a nested list
; within the main list of from_units. e.g.
;     (link 'd '(a b c) .5)
; will link a, b, and c separately to d, while
;     (link 'd '((a b c)) .5)
; will conjunctively link a, b, and c to d.
; Other examples:
;     (link 'a '(b c d) .5)
; creates links between a and b, a and c, and a and d,
;     (link '(a b) '(c d) .5)
; creates links between a and c, a and d, b, and c, and b, and d.
; this link takes an optional parameter which specified the weight of the
; link
; created from the recipient to the conjunctive units.  If not specified,
; the default back-weight is the forward weight.
; created by eric, imported from echo

(defun link (to_units from_units weight &optional back-weight)
  (if (not back-weight)
    (setf back-weight weight))
  (if (listp from_units)
    (dolist (fu from_units)
      (if (listp to_units)
        (dolist (tu to_units)
          (if (listp fu)
            (make_conjunctive_link fu tu weight back-weight)
            (make_sym_link fu tu weight)))
        (if (listp fu)
          (make_conjunctive_link to_units fu weight back-weight)
          (make_sym_link to_units fu weight))))
    (if (listp to_units)
      (dolist (tu to_units)
        (make_sym_link from_units tu weight))
      (make_sym_link from_units to_units weight))))
      


; RUN_HYP_NET is the main loop, updating activation.

(defun run_hyp_net (mode) ; acme, echo or arcs
;   (unless silent_run? (print_values mode))
   (setq testnum (newsym 'test))
   (if (eq mode 'echo) (setq *acme-mode* nil)  ; for grapher
       ; else
       (setq *acme-mode* t)
   )
   (do ((timestep 1 (+ 1 timestep))
        (cycle 0 (+ 1 cycle))
	(units (reverse all_units))
	(old-asymptoted nil)
       )
   ;exit:
    ( (or stop_run?
          (and stop_settled? settled?)   ; network has settled
          (> timestep (car (last when_to_print)))
      )
      (and (print_run mode 'verbose) 
	   (if silent_run? t (my_print '"Run finished. "))
      )
    )
     ;; repeat:
     (if (= 0 (mod cycle 10))
	(my_print "Cycle #" cycle ":"))
     (debug_run)
    ; probablistically turn off output or output and input of units
    ; added by eric
    (mapcar #'prob_deactivate_unit units)
    ; update all activations:
    ; this records each units new activation
    ; but for synchrony the new activation is not
    ; used in updating other units

    (if *staracme-loaded* 
	(progn 
;; Get timing info
;(princ "update time: ")

;	  (my_print "updating Parallel network")
          (if grossberg?
	     (if *time-cm*
; old line       (cm:time  (update_unit_activn_gross!!))
		 (time  (update_unit_activn_gross!!))
		 (update_unit_activn_gross!!))
	      (update_unit_activn!!)))
	(progn
	  ;(my_print "updating sequential net")
	  (if grossberg?  ; use Grossberg's updating rule.
	      (mapcar #'update_unit_activn_gross units)
	      ;; else use Rumelhart & McClelland rule:
	      (mapcar #'update_unit_activn units)))
    )
    ; record the new activation of each unit:
    (setq settled? t)    ; has network reached asymptote?
                         ; this turns nil if unit not asymptoted.
    (setq old-asymptoted asymptoted_units)
    (setq asymptoted_units nil) 
    (if *staracme-loaded*
        (progn 
;	   (my_print "fixing parallel activation")
;(princ "fix time:")
	  (if *time-cm* 
; old line    (cm:time  (fix_parallel))
	      (time  (fix_parallel))
	      (fix_parallel))
	   (if settled?
	       (my_print "Parallel settling detected")))
	(progn 
	  (mapcar #'fix_activation units)
	  (unless silent_run?
	    (mapcar #'announce_asymptote
		    (set-difference asymptoted_units old-asymptoted)
		    ))))
    (if (and stop_settled? settled? (not silent_run?))
        (my_print '"Network has settled by cycle " total_times ".")
    )
    ; print status:
    (if (member timestep when_to_print) (print_run mode))

     ; for graphics:
     (if (eq *last-init* 'act) (show-act))
     (if *trace-list* (update-trace))
     
     ;; for debugging parallel stuff
     (if *eric-debug* (debug-hook))

     (setq total_times (+ total_times 1))
   )
)


; ****************************************************

; PROB_DEACTIVATE_UNIT probablistically gates off either or both of a
; unit's
; net input and it's output, depending on the values of output_prob and
; input_prob

(defun prob_deactivate_unit (unit)
  (let ((r (random 100)))
    (if (get unit 'output_prob)
      (if (< r (* 100 (get unit 'output_prob)))
        (put unit 'deactivate_output nil)
        (put unit 'deactivate_output t))
      (put unit 'deactivate_output nil))
    (if (get unit 'input_prob)
      (if (< r (* 100 (get unit 'input_prob)))
        (put unit 'deactivate_input nil)
        (put unit 'deactivate_input t))
      (put unit 'deactivate_input nil))))
    


; ****************************************************

; New UPDATE_UNIT_ACTIVN updates the activation of a unit probablisitcally
; depending on the value of input_prob.  This implements a focus-
; of-attention mechanism.
; Added by eric

(defun update_unit_activn (unit)
  (declare (ftype (function (&rest float) float) min max + * -)
           (ftype (function (float float) symbol) >))  
  (if (not (get unit 'deactivate_input))
    (progn
      (let ((net_input_value (net_input unit)))
        (declare (type (float) net_input_value))
        (put unit 'new_activation
             (min max_activation
                  (max min_activation
                       (+ (* (get unit 'activation) (- 1.0 decay_amount))
                          (if (> net_input_value 0.0)
                            (* net_input_value
                               (- max_activation (get unit 'activation))
                               )
                            ; else:
                            (* net_input_value
                               (- (get unit 'activation)
min_activation)))))))))
    (put unit 'new_activation (get unit 'activation))))


; ****************************************************
; GET_OUTPUT is thresholds the activation of a unit and probablistically
; gates its output.
; added by eric

(defun get_output (unit)
  (if (get unit 'deactivate_output)
    0.0
    (let ((thresh (if (get unit 'output_thresh) 
                    (get unit 'output_thresh)
                    output_threshold)))
      (max thresh (get unit 'activation)))))



; ****************************************************
; the new NET_INPUT delegates thresholding to get_output, and handles
; sigma-pi units
; modified by eric

; NET_INPUT is the weighted sum of output from all input units.
; Note that links_from now contains weights, not names or indices.
; The new NET_INPUT probabilistically gates units.  See GET_OUTPUT above.
(defun net_input (unit)
  (declare (ftype (function (&rest float) float) max + *))
  (let ((result 0.0)
        (a 0.0))
    (declare (type (float) result))
    (declare (type (float) a))
    (do ((links (get unit 'links_from) (cdr links)))
        ((null links) result)
      (setq result (+ (* (float (cdar links))
			 (get_output (caar links)))
                      result) ))
                   
    ;; sigma-pi routine
    (dolist (cl (get unit 'conjunctive_links_from))
      (setf a 1.0)
      (dolist (fu (car cl))
        (setf a (* a (get_output fu))))
      (setf result (+ result (* (float (cadr cl)) a))))
    result))


;; these functions need modification and testing:

; New UPDATE_UNIT_ACTIVN_GROSS probablistically updates itself.  
; See UPDATE_UNIT_ACTIVN
; added by eric
;
; UPDATE_UNIT_ACTIVN_GROSS  updates the activation of a unit based on the
; links it has, using Grossberg's rule that treats excitation and
; inhibition separately.
; Uses global variables current_excit and current_inhib


(defun update_unit_activn_gross (unit)
  (declare (ftype (function (&rest float) float) + - * min max)) 
  (if (not (get unit 'deactivate_input))
    ;; calculate excitation and inhibition.
    (progn
      (if tversky?
        (excit_and_inhib_tversky unit)
        (excit_and_inhib unit)
        )
      (put unit 'new_activation
           (min max_activation
                (max min_activation
                     (+ (* (get unit 'activation) (- 1.0 decay_amount))
                        (* (- max_activation (get unit 'activation))
current_excit)
                        (* (- (get unit 'activation) min_activation)
current_inhib))))))
    (put unit 'new_activation (get unit 'activation)))) 


; *******************************************************************
; EXCIT_AND_INHIB is just like net_input, except that it keeps track
; of excitation and inhibition separately.  EXCIT_AND_INHIB_TVERSKY is
; identical except that units with negative activation can pull down
; their excitatory neighbors.

(defun excit_and_inhib (unit)
  (let ((a 0.0) (wt 0.0))
    (declare (ftype (function (&rest float) float) max + *) 	
             (ftype (function (float float) symbol) >))
    (do ((excit 0.0) (inhib 0.0) (wt 0.0) (activn 0.0) 	 
         (links (get unit 'links_from) (cdr links)))
        ((null links)
         (setq current_excit excit)
         (setq current_inhib inhib)
         )
      (declare (type (float) excit inhib wt activn)) 
      (setq wt (float (cdar links)))
      (setq activn (max output_threshold (get_output (caar links)))) 	
      (if (> wt 0.0)
        (setq excit (+ excit (* wt activn)))
        ; else wt is inhibitory:
        (setq inhib (+ inhib (* wt activn)))))
    ;; sigma-pi routine
    (dolist (cl (get unit 'conjunctive_links_from)) 	 
      (setf a 1.0)
      (dolist (fu (car cl))
        (setf a (* a (get_output fu))))
      (setq wt (float (cadr cl)))
      (if (> wt 0.0)
        (setq current_excit (+ current_excit (* wt a)))
        (setq current_inhib (+ current_inhib (* wt a)))))))

(defun excit_and_inhib_tversky (unit)
  (declare (ftype (function (&rest float) float) max + *)
	   (ftype (function (float float) symbol) >))
  (do ((excit 0.0) (inhib 0.0) (wt 0.0)
       (links (get unit 'links_from) (cdr links))
      )
      ((null links)
       (setq current_excit excit)
       (setq current_inhib inhib)
      )
      (declare (type (float) excit inhib wt))
      (setq wt (float (cdar links)))
      (if (> wt 0)
	  (setq excit (+ excit (* wt (get (caar links) 'activation))))
	  ; else wt is inhibitory:
          (setq inhib (+ inhib (* wt (max output_threshold
					  (get (caar links) 'activation)
				     ))))
     )
  )
)

; **********************************************************
; This new version of make_sym_link doubles inhibition if called from
; one of the _inhib_important routines 

(defun make_sym_link (unit1 unit2 weight)
  (cond (  (and (not (eq unit1 unit2))
                (or *inhibiting-unimportant*
		    (>= (weight_of_link_between unit1 unit2) 0)))
           ; then:
	   (setf *inhibiting-unimportant* nil)
	   (make_link unit1 unit2 weight)
           (make_link unit2 unit1 weight)
         )
   )
)


; ****************************************************
; PRINT_LINKS shows all the links and weights of a unit.

(defun print_links (unit)
   (mapcar #'(lambda (linked)
                  (my_print '"  Link from " (car linked)
                            '" (activation: "
                            (get (car linked) 'activation)
                            '") has weight "
                            (cdr linked)
                  )
             )
             (sort (get unit 'links_from) '(lambda (a b) (string-lessp (car
a) (car b))))
   )
   (mapcar #'(lambda (linked)
                  (my_print '"  Conjunctive Link from " (car linked)
                            " has weight "
                            (cadr linked)
                  )
             )
             (get unit 'conjunctive_links_from)))


;**********************************
;  OUTPUT_PROB takes a list of units or a single unit and sets the 
;  probability that the
;  unit(s) will generate an output value.  This a feature added to echo
;  to implement a kind of focus-of-attention mechanism.  
;  added by eric

(defun output_prob (units value)
  (if (listp units)
    (dolist (u units)
;      (format t "Setting output probability of ~a to ~a.~%" u value)
      (put u 'output_prob value))
    (progn
;      (format t "Setting output probability of ~a to ~a.~%" units value)
      (put units 'output_prob value))))

;**********************************
;  ACTIVE_PROB is similar to OUTPUT_PROB. 
;  It takes a list of units or a single unit and sets the probability that
;  the unit(s) will be updated, and generate an output.  This a feature added
;  to echo to implement a kind of focus-of-attention mechanism.  
;  added by eric

(defun active_prob (units value)
  (if (listp units)
    (dolist (u units)
;      (format t "Setting active probability of ~a to ~a.~%" u value)
      (put u 'output_prob value)
      (put u 'input_prob value))
    (progn
;      (format t "Setting active probability of ~a to ~a.~%" units value)
      (put units 'output_prob value)
      (put units 'input_prob value))))


;******************************************************
; OUTPUT_FLOOR takes a single unit or a list of units, and sets
; the lower threshold for the output of the unit(s)

(defun output_floor (units value)
  (if (listp units)
    (dolist (u units)
      (format t "Setting output floor of ~a to ~a.~%" u value)
      (put u 'output_thresh value))
    (progn
      (format t "Setting output floor of ~a to ~a.~%" units value)
      (put units 'output_thresh value))))


;******************
;  INIT-ACTIVATION initializes the activation of a unit or list of units
;  New function added by eric 

(defun init_activation (units value)
  (if (listp units)
    (dolist (u units)
      (put u 'activation value)
      (format t "Setting initial activation of ~a to ~a~%" u value))
    (progn
      (put units 'activation value)
      (format t "Setting initial activation of ~a to ~a~%" units value))))


;*********************
;  Functions which allow association lists to uses lists as keys


; LIST_EQUAL compares to lists to see if they're equivalent (doesn't do
; nested checking)

(defun list_equal (l1 l2)
  (cond ((null l1) (null l2))
        ((null l2) (null l1))
        (t (and (eq (car l1) (car l2)) (list_equal (cdr l1) (cdr l2))))))

; LIST_ASSOC takes a list or an atom as a key for an association list,
; and returns the first pair that matches the key.
; the association list is of the form ((attr1 val1) (attr2 val2)), not
; (attr1.val1 attr2.val2).  attr is either an atom or alist

(defun list_assoc (key alist)
  (cond ((null alist) nil)
        ((listp key) (if (listp (caar alist))
                       (if (list_equal key (caar alist)) 
                         (car alist)
                         (list_assoc key (cdr alist)))
                       (list_assoc key (cdr alist))))
        (t (if (eq key (caar alist)) 
             (car alist)
             (list_assoc key (cdr alist))))))

             
             
         


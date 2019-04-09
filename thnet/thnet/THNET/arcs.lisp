
; FILE:       net/arcs.l
; PURPOSE:    Analogical Retrieval by Constraint Satisfaction
; PROGRAMMER: Greg Nelson, Paul Thagard
; CREATED:    1-22-88
; UPDATED:    1-26-88

; This file contains functions for creating constraint satisfaction networks 
; for retrieving structures in memory using similar structures as primes. 
; The constraints are semantic relevance, which gets things going,
; relational consistency, and structural organization.

(defun lar () (load "//tinman/ucla/psych/emelz/arcs.l"))

; *************************************************************
;                      NETWORK CREATION
; *************************************************************
; PROBE_FROM_STRUC initiates a retrieval attempt from a given
; structure, which can be a problem, story, etc.
; It begins to set up a network based on semantic similarity
; using features in common between the predicates and objects
; in the given structure and others stored in memory.
; After the network is set up, it runs it to settle into 
; a state indicating the most relevant structures in memory.

; *************** DETAILED PROCESS
; 1: mapped_structures (initialized to struc) to set up inhibitions between
;    the different retrieval hypotheses
; 2: for each proposition in struc call probe_from_propn (see list below)
; 3: set up inhibitions between retrieval hypotheses, between different concept
;    mappings, between different proposition mappings, and between all object
;    mappings

(defun probe_from_struc (struc)
   (unless silent_map? (my_print '"Probing memory using structure: " struc))
   ; probe from the structure's propositions, creating the network:
   (setq mapped_structures (list struc))
   (if simple_semantic_weight?
       (and (if silent_map? t (my_print '"Using special unit."))
	    (put 'special 'activation .99)
       ) ;else:
       (unless silent_map? (my_print '"Not using special unit."))
   )
   (setq *struc1* struc)
   (do ((propns (get struc 'propositions) (cdr propns)))
       ((null propns) 'done)
       (probe_from_proposition struc (car propns))
   )
   (setq excit_links total_links)
   ; add inhibitions:
   (inhibit_multiple_mappings mapped_structures inhib_weight)
   (inhibit_multiple_mappings mapped_concepts inhib_weight)
   (inhibit_multiple_mappings mapped_propositions 
                              (* propn_uniqueness inhib_weight)
   )
   (inhibit_multiple_mappings all_objects inhib_weight)
   (setq inhib_links (- total_links excit_links))
   ; add importance (optionally):
   (if auto_importance? (check_importance struc))
   (setq excit_links (- total_links inhib_links))
   (cond ((null silent_map?)
	  (my_print "   Total number of links: " total_links)
	  (my_print "         excitatory:      " excit_links)
	  (my_print "         inhibitory:      " inhib_links)
	 )
   )
)

; *************************************************************
; PROBE_FROM_PROPOSITION begins a memory probe using a given proposition.

; *************** DETAILED PROCESS
; 1:  Retrieve all "related" predicates (using get_associated_preds)
; 2:  Discard all of those which only occur in struc (using exists_elsewhere)
; 3:  Use each of these as the probe in probe_from_pred
; 4:  Later, mapcar probe_from_obj on objects of the proposition

(defun probe_from_proposition (struc propn)
  (let (associates)
    (setq associates (choose_associate_function (pred_from_propn propn)
						 (tv_from_propn propn)
                     )
    )
    (do ((search_preds
	  (if probe_self?
	      (exists_at_all associates)
	      (exists_elsewhere struc associates)
          )
	  (cdr search_preds))
        )
	((null search_preds) t)
	(probe_from_pred struc propn (car search_preds))
    )
  )
;  (mapcar #'probe_from_obj (args_from_propn propn))
)

; *************************************************************
; CHOOSE_ASSOCIATE_FUNCTION selects between the various associate retrieval
; functions so this is done neatly.

(defun choose_associate_function (predicate &optional (truthval 'true))
  (cond (simple_semantic_weight?
	 (get_associates_with_weights predicate truthval)
	)
	(t
	 (get_associated_preds predicate truthval)
	)
  )
)

; *************************************************************
; PROBE_FROM_PRED begins by mapping the predicate, then proceeds to connect
; all of the relevant propositions

(defun probe_from_pred (struc propn new_pred)
  (let ((pred (pred_from_propn propn)) pred_unit)
; A
    (setq mapped_concepts
	  (cons_if_new pred
		       (cons_if_new new_pred mapped_concepts)
	  )
    )
    (setq pred_unit (catname pred new_pred))
    (cond ((not (member pred_unit all_units))
	   (create_unit '"concepts " pred new_pred)
	   (cond (simple_semantic_weight?
		  (make_sym_link 'special pred_unit (* base_sim_weight
						      (get new_pred 'strength)
				                   )
	          )
		  (remprop new_pred 'strength)
	         )
	   )
	  )
    )
; B & C
    (do ((search-props (get new_pred 'from_propns) (cdr search-props)))
	((null search-props) t)
	(probe_from_propn struc propn (car search-props) pred new_pred)
    )
  )
)


; *************************************************************
; PROBE_FROM_PROPN begins by creating the proposition unit, then connecting
; this unit to the predicate unit, the argument units, and various other
; things.

; I passed the predicates merely to save recalculation.
(defun probe_from_propn (struc1 prop1 prop2 pred1 pred2)
  (prog ((struc2 (struc_from_propn prop2))
	 (struc_unit (catname struc1 (struc_from_propn prop2)))
	 (prop_unit (catname prop1 prop2))
	 (pred_unit (catname pred1 pred2)))
    (if (and (not probe_self?) (equal struc1 struc2)) (return nil))
; 1.  Create the unit prop<T,i>=search-prop<k>
    (create_unit '"propositions " prop1 prop2)
    (setq mapped_propositions (union mapped_propositions (list prop1 prop2)))
; 2.  Link this unit to pred<T,i>=search-pred<j> 
    (make_sym_link prop_unit pred_unit excit_weight)
; 3.  Create units for arg<T,i,m>= the m-th argument of search-prop<k>
; 4.  Link each of these units to prop<T,i>=search-prop<k>
    (make_excit_links prop_unit (make_obj_units (args_from_propn prop1)
						(args_from_propn prop2)
						default_activation
				)
		      excit_weight
    )
; New line added 6/15 by GHN to make all_objects for arcs.  ACME does this
; in CONSTRAINT_MAP, so ARCS should correspondingly do it somewhere during the
; mapping process, but I don't think this is very efficient.
;    (setq all_objects (union all_objects
;			      (union (args_from_propn prop1)
;				     (args_from_propn prop2)
;			      )
;                       )
;    )
; 5.  If in link_to_preds mode, link each of these units to
;     pred<T,i>=search-pred<j>  (remember this can get multiplied by N)
    (if link_concepts_objects?
	(make_excit_links pred_unit (make_obj_units (args_from_propn prop1)
						    (args_from_propn prop2)
						    default_activation
			 	    )
			  excit_weight
        )
    )
; 6.  Find the source structure struc<n> which contains search-prop<k>
; 7.  Create the unit struc<T>=struc<n> (because struc1 is already in
;     mapped_structures, we don't need to add it here)
    (create_unit '"structures " struc1 struc2)
; 8.  Link this unit to prop<T,i>=search-prop<k>
    (make_sym_link prop_unit struc_unit excit_weight)
; 9.  Optionally link struc<T>=struc<n> to pred<T,i>=search-pred<j>
;     (remember, this multiplies by the number of discrete k's)
    (if link_strucs_concs? (make_sym_link pred_unit struc_unit excit_weight))
  )
)

; ************************************************************
; EXISTS_ELSEWHERE removes from the given list of concepts all of those
; which appear nowhere but in the probe analog.  EXISTS_AT_ALL does the
; same thing, but checks if they appear anywhere, including the probe.

; *************** DETAILED PROCESS
; 1: for each member of conc-list
; 2: if there are atoms in the concept's 'belongs_to list other than the probe
;    structure, cons it onto a return list

(defun exists_elsewhere (struc conc-list)
  (do ((need_check conc-list (cdr need_check))
       (okay_check nil))
      ((null need_check) okay_check)
      (if (remove struc (get (car need_check) 'belongs_to))
	  (setq okay_check (cons (car need_check) okay_check))
      )
  )
)

(defun exists_at_all (conc-list)
  (do ((need_check conc-list (cdr need_check))
       (okay_check nil))
      ((null need_check) okay_check)
      (if (get (car need_check) 'belongs_to)
	  (setq okay_check (cons (car need_check) okay_check))
      )
  )
)

; *************************************************************
; SEMANTIC_SIMILARITY should eventually use the semantic decomposition
; more carefully.  For now, it justs looks at intersection.
; Compare sem_similarity in acme.l.

(defun semantic_similarity (conc1 conc2)
  (/ (length (intersection (all_associates conc1) (all_associates conc2)))
     (length (all_associates conc1))
     1.0
  )
)

(defun all_associates (pred)
  (union_list
   (list pred)
   (get pred 'superordinates)
   (get pred 'subordinates)
   (get pred 'synonyms)
   (get pred 'antonyms)
   (get pred 'part-of)
   (get pred 'sub-parts)
   (feat_from_decomp (get pred 'decomp))
  )
)

; *************************************************************
; CREATE_UNIT
; Question:  how to handle multiple hits?

(defun create_unit (type one two)
   (let ((new_unit (catname one two)))
        (cond ( (not_member new_unit all_units)
		(unless silent_map?
			(my_print '"      Creating units for " type 
				  one '" and " two
			)
                )
		(put new_unit 'concerns (list one two))
                (note_unit new_unit)
                (note_constraint one new_unit)
                (note_constraint two new_unit)
              )
         )
         new_unit ; return name of unit
   )
)

; *************************************************************
; NOTE_CONSTRAINT notes for a thing the various units that
; have been formed concerning how it can be mapped.

(defun note_constraint (thing new_unit)
  (put thing 'constraint_hyps
       (cons_if_new new_unit (get thing 'constraint_hyps))
  )
)

; end of arcs.l

; *************************************************
;            FOR EXAMINING THE RESULTS OF THE PROBE:
; *************************************************

(defun best_retrieval (struc &optional (verbose t))
  (prog ()
    (my_print "Results of probing from " struc " after " total_times " cycles:")
    (my_print "Structures retrieved:")
    (shu struc)
    (if (null verbose) (return nil))
    (my_print "Factors involved were:")
    (mapcar #'best_match
	    (sort
	     (union
	      (union (intersection mapped_concepts (conc_from_struc struc))
		     (intersection all_objects (obj_from_struc struc))
	      )
	      (intersection mapped_propositions (propns_from_struc struc))
	     )
	     #'string-lessp
	    )
    )
    t
  )
)



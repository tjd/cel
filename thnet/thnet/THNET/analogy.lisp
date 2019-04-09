
; FILE:        analogy.l
; PURPOSE:     contains functions shared by ACME and ARCS
; PROGRAMMER:  Greg Nelson
; CREATED:     9-7-88

; ****************************************************
; PRINT_GOOD reports on goodness of fit of analogy.

(defun print_good ()
   (my_print '"Goodness of network:  " (goodness all_units))
)

; **********************************************************       
; MAKE_OBJ_UNITS  makes hypotheses relating pairs of
; ordered sequences of arguments.

(defun make_obj_units (arguments1 arguments2 activation)
  (do ((args1 arguments1 (cdr args1))
       (args2 arguments2 (cdr args2))
       (units_made nil (cons (make_obj_unit (car args1)
					    (car args2)
					    activation
			     )
			     units_made
                       )
       )
       ; adjust excitation weight for duplicate arguments:  see data/num.l
       (weight (if watch_for_dup_arguments?
		   (* excit_weight (number_dups arguments1 arguments2))
		   excit_weight))
      )
      ((null args1)
       (setq units_made (remove-duplicates units_made))
       (setq object_units (union units_made object_units))
       (if link_objects? (make_all_excit_links units_made weight))
       units_made
      )
  )
)

; **********************************************************
; MAKE_OBJ_UNIT makes a unit corresponding to the hypothesis that
; two objects are identical.

(defun make_obj_unit (obj1 obj2 activation)
   (let (new_unit)
        (setq new_unit (catname obj1 obj2))
        (cond ( (member new_unit all_units) new_unit)
              ;else:
              (t (put new_unit 'concerns
                      (list obj1 obj2)
                 )
                 (note_unit new_unit)

                 ; unless unit already got activation from make_hyp_unit
                 (unless (get new_unit 'activation)
                    (and (put new_unit 'activation activation)
                         (put new_unit 'original_activation activation)
                    )
                 )
                 (put obj1 'constraint_hyps 
                      (cons_if_new new_unit (get obj1 'constraint_hyps))
                 )
                 (put obj2 'constraint_hyps 
                      (cons_if_new new_unit (get obj2 'constraint_hyps))
                 )
		 (setq all_objects (union all_objects (list obj1 obj2)))
                 new_unit
              )
        )
    )
)

; **************************************************       
; INHIBIT_MULTIPLE_MAPPINGS sets up inhibitory links among competing 
; hypotheses about a particular object or concept.  
; Will not clobber excitatory links:  see make_sym_link.
; For pragmatics sake, it treats queries differently, forming
; excitatory rather than inhibitory links.

(defun inhibit_multiple_mappings (list_of_objs_or_concs weight)
   (do ((lst (remove_nil_dup list_of_objs_or_concs) (cdr lst)))
        ; exit:
       ((null lst) total_links)
       (if (atom_begins (car lst) #\?)  ; query
           (excit_queries (car lst))
           ; otherwise:
          (make_inhib_links (no_queries (get (car lst) 'constraint_hyps))
                           weight
          )
       )
    )
)

; ***************************************************
;         PRAGMATICS:                               
; ****************************************************

; *************************************************************
; CHECK_IMPORTANCE checks a structure to see if it is of the type problem,
; and, if so, makes appropriate connections to the "pragmatic" unit.
; The units connected include all hypotheses about predicates and propositions
; in the goal, and all hypotheses about propositions and predicates of
; proposition mentioned in the goal.  If propns_import? is nil, the
; proposition connections will not be made.

(defun check_importance (struc)
  (prog (goal goalprops)
    (if (or 
	 (not (equal (get struc 'data_type) 'problem))
	 (not (member 'goal (get struc 'fields)))
	)
	(return nil)
    )
    (setq goal (get struc 'goal))
    ; If any predicates have been mapped, IMPORTANT them.
    (important
     (remove-if-not '(lambda (el) (member el mapped_concepts))
		    (mapcar #'get_pred goal)
     )
    )
    ; If any propositions have been mapped, IMPORTANT them.
    (if propns_import?
	(important
	 (remove-if-not '(lambda (el) (member el mapped_propositions))
			(mapcar #'get_propn_name goal)
	 )
	)
    )
    ; If any arguments which are propositions have been mapped,
    (setq goalprops
	  (remove-if-not '(lambda (el) (member el mapped_propositions))
			 (apply #'append (mapcar #'get_args goal))
	  )
    )
    ; IMPORTANT their predicates,
    (important
     (remove-if-not '(lambda (el) (member el mapped_concepts))
		    (mapcar #'pred_from_propn goalprops)
     )
    )
    ; and IMPORTANT them.
    (if propns_import? (important goalprops))
  )
)

; **********************************************************
; PRESUMED  notes that certain  mapping hypotheses or desired or presumed.

(defun presumed (list_of_units)
   (create_pragmatic_unit)
   (do ((units list_of_units (cdr units)))
       ((null units) 'done)
       ; repeat:
       (unless silent_map? (my_print (car units) '" is a presumed mapping."))
       (make_sym_link 'pragmatic (car units) prag_weight)
   )
)

(defun desired (lst) (presumed lst))
      
; *********************************************************
; IMPORTANT indicates that the system has a special interest
; in an element (concept, object, proposition) although it
; does not desire any particular map.

(defun important (list_of_els)
   (create_pragmatic_unit)
   (do ((els  list_of_els (cdr els)))
       ((null els) 'done)
       ;
       (unless silent_map? (my_print (car els) " is an important element."))
       (make_excit_links 'pragmatic 
                         (get (car els) 'constraint_hyps) 
                         import_weight
       )
   )
)
                         
; **********************************************************
; CREATE_PRAGMATIC_UNIT makes a special pragmatic unit if necessary.

(defun create_pragmatic_unit ()
   (cond ((null pragmatic_unit_made)
          (put 'pragmatic 'activation 1)   ; clamp it
          (setq all_units (remove 'pragmatic all_units))
          (setq pragmatic_unit_made t)
         )
   )
)

; ***************************************************

; NO_QUERIES removes mappings about queries from a list of units.

(defun no_queries (lst)
    (do ((ls lst (cdr ls))
         (result nil)
        )
        ((null ls) result)
        (unless (or (is_query (car (get (car ls) 'concerns)))
                    (is_query (second (get (car ls) 'concerns)))
                )
                (push (car ls) result)
        )
    )
)


; IS_QUERY

(defun is_query (atm)
   (atom_begins atm #\?)
)

; **********************************************************
; BEST_MATCH figures out what concept or object has been found to be
; the best match.

(defun best_match (conc_or_obj) 
  (let (bm best_unit other_good_units)
    (setq best_unit (highest-l (get conc_or_obj 'constraint_hyps)
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
			       (get conc_or_obj 'constraint_hyps)
		       )
        )
    )
    (setq best_matches (cons (list conc_or_obj bm) best_matches))
  )
)

; ******************************************************************
; OTHER_FROM_PAIR pick the other out of a pair that includes the given atom.

(defun other_from_pair (atm lst)
   (if (eq atm (car lst)) (second lst)
       (car lst)
   )
)

; *******************************************************************
; GOOD_ENOUGH_S checks whether other mappings are over a threshold.

(defun good_enough_s (el units)
   (do ((unts units (cdr unts)))
       ((null unts) 'done)
       (good_enough el (car unts))
   )
)

; *******************************************************************
; GOOD_ENOUGH says if a mapping has an activation over a threshold.

(defun good_enough (el unit)
    (if (> (get unit 'activation) min_good)
        (my_print "     Mapping with "
                  (other_from_pair el (get unit 'concerns))
                  " is also possible:  "
                  (get unit 'activation)
        )
    )
)

; **********************************************************************
; CATNAME is used by ACME and ARCS to put together hypothesis unit names.

(defun catname (unit1 unit2)
  (read-from-string (string-append unit1 "=" unit2))
)


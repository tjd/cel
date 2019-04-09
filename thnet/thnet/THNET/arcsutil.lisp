;;; FILE: arcsutil

; *************************************************************
; GET_ASSOCIATED_PREDS retrieves predicates which are somehow associated
; with a probe predicate.  GET_ASSOCIATES_WITH_WEIGHTS does the same thing,
; but adds a special, temporary property "strength."  Can do many combinations
; of the following:
; A) If the concept has a singular property or a root-tense property, use
;    this rather than the concept itself.  (Optional.)
; B) The predicate's own superordinates, synonyms, antonyms, subordinates,
;    sub-part, parts-of, "decomp".  (Some parts might be optional.)
; C) Higher superordinates.  (Optional, variable number.)
; D) Subordinates of some or all superordinates.  (Various variables.)
; E) Further synonyms of synonyms.  (Optional.)
; F) Other things which share parts-of relations (sub-parts of things the
;    concept is a part-of).  (Optional.)

(defun get_associated_preds (predicate &optional (truthval 'true))
  (let ((pred predicate) root)
    (cond ((member pred unwanted_preds) (return-from get_associated_preds nil))
	  ((not use_root_concs?) nil)
	  ((setq root (car (get pred 'singular))) (setq pred root))
	  ((setq root (car (get pred 'root-tense))) (setq pred root))
	  ((setq root (car (get pred 'root-conc))) (setq pred root))
    )
    (prog ((supers (get pred 'superordinates))
	   (subs (get pred 'subordinates))
	   (syns (get pred 'synonyms))
	   (ants (get pred 'antonyms))
	   (holos (get pred 'part-of))
	   (parts (get pred 'sub-parts))
	   (decmp (feat_from_decomp (get pred 'decomp)))
	   (result (if root (list predicate pred) (list pred)))
	   temp
	  )
	  (cond ((and swap_when_false? (equal truthval 'false))
		 (setq temp syns)
		 (setq syns ants)
		 (setq ants temp)
		)
	  )
	  (setq result (union_list result supers subs syns))
	  (if include_ants? (setq result (union result ants)))
	  (if include_sub_parts? (setq result (union result parts)))
	  (if include_part_ofs? (setq result (union result holos)))
	  (if include_decomp? (setq result (union result decmp)))
	  (if (> superord_distance 1)
	      (setq result (union result
				  (get_higher_superords supers (1- superord_distance)) 
			   )
	      )
	  )
; Might also use this somewhere
;      (if (eq feature_selection 'loose)
;	  (setq result (union result
;			      (concs_from_rules (get pred 'rules))
;		       )
;          )
;     )
	  (if subs_from_supers?
	      (setq result (union result (get_from_list supers 'subordinates)))
	  )
	  (if shared_sub-parts?
	      (setq result (union result (get_from_list parts 'part-of)))
	  )
    (return result)
    )
  )
)

(defun get_associates_with_weights (predicate &optional (truthval 'true))
  (let ((pred predicate) root)
    (cond ((member pred unwanted_preds)
	   (return-from get_associates_with_weights nil)
	  )
	  ((not use_root_concs?) nil)
	  ((setq root (car (get pred 'singular))) (setq pred root))
	  ((setq root (car (get pred 'root-tense))) (setq pred root))
	  ((setq root (car (get pred 'root-conc))) (setq pred root))
    )
    (prog ((supers (get pred 'superordinates))
	   (subs (get pred 'subordinates))
	   (syns (get pred 'synonyms))
	   (ants (get pred 'antonyms))
	   (holos (get pred 'part-of))
	   (parts (get pred 'sub-parts))
	   (decmp (feat_from_decomp (get pred 'decomp)))
	   (result (if root (list predicate pred) (list pred)))
	   temp
	  )
	  (cond ((and swap_when_false? (equal truthval 'false))
		 (setq temp syns)
		 (setq syns ants)
		 (setq ants temp)
		)
	  )
	  (setq result (union_list
			(put_on_list_if_better result 'strength self_sim_weight)
			(put_on_list_if_better supers 'strength superord_sim_weight)
			(put_on_list_if_better subs 'strength subord_sim_weight)
			(put_on_list_if_better syns 'strength syn_sim_weight)
		       )
	  )
	  (if include_ants?
	      (setq result (union result
				  (put_on_list_if_better ants 'strength ant_sim_weight)
			   )
	      )
	  )
	  (if include_sub_parts?
	      (setq result (union result
				  (put_on_list_if_better parts 'strength part_sim_weight)
			   )
	      )
	  )
	  (if include_part_ofs?
	      (setq result (union result
				  (put_on_list_if_better holos 'strength holo_sim_weight)
			   )
	      )
	  )
	  (if include_decomp? (setq result (union result decmp)))
	  (if (> superord_distance 1)
	      (setq result (union result
				  (put_on_list_if_better (get_higher_superords supers (1- superord_distance)) 'strength higher_super_sim_weight)
			   )
	      )
	  )
; Might also use this somewhere
;      (if (eq feature_selection 'loose)
;	  (setq result (union result
;			      (concs_from_rules (get pred 'rules))
;		       )
;          )
;     )
	  (if subs_from_supers?
	      (setq result (union result 
				  (put_on_list_if_better (get_from_list supers 'subordinates) 'strength sub_of_super_sim_weight)
			   )
	      )
	  )
	  (if shared_sub-parts?
	      (setq result (union result
				  (put_on_list_if_better (get_from_list parts 'part-of) 'strength shared_sub-part_sim_weight)))
	  )
    (return result)
    )
  )
)

(defun get_higher_superords (supers dist)
  (let ((higher (get_from_list supers 'superordinates)))
    (union higher
	   (if (> dist 1)
	       (get_higher_superords higher (1- dist))
	       nil
	   )
    )
  )
)

(defun sem_sim_arcs (conc1 conc2)
  "Return the highest value (according to the list of priorities in init.l)
of a relationship between CONC1 and CONC2."
  ; Make the default as small as conceivably conceivable.
  (let ((weight -100))
    (if (equal conc1 conc2)
	(setq weight (max weight self_sim_weight))
    )
    (if (member conc2 (get conc1 'synonyms))
	(setq weight (max weight syn_sim_weight))
    )
    (if (member conc2 (get conc1 'superordinates))
	(setq weight (max weight superord_sim_weight))
    )
    (if (member conc2 (get conc1 'subordinates))
	(setq weight (max weight subord_sim_weight))
    )
    (if (member conc2 (get conc1 'synonyms))
	(setq weight (max weight syn_sim_weight))
    )
    (if (member conc2 (get conc1 'antonyms))
	(setq weight (max weight ant_sim_weight))
    )
    (if (member conc2 (get conc1 'sub-parts))
	(setq weight (max weight part_sim_weight))
    )
    (if (member conc2 (get conc1 'part-of))
	(setq weight (max weight holo_sim_weight))
    )
    (if (member conc2 (get_higher_superords (list conc1) superord_distance))
	(setq weight (max weight higher_super_sim_weight))
    )
    (if (member conc2 (get_from_list (get conc1 'superordinates) 'subordinates))
	(setq weight (max weight sub_of_super_sim_weight))
    )
    (if (> weight -100) (* base_sim_weight weight) 0)
  )
)

; *************************************************************
; FEAT_FROM_DECOMP pulls features out of a structured list
; representing semantic decomposition.  For now, keep it simple.
; This really never does anything, anyway.

(defun feat_from_decomp (lst)
   (flatten lst)
) 

(if (or load_arcs? use_arcs_semantics?)
    (load "//tinman/ucla/psych/emelz/ARCS/semantics")
)




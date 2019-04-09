
; FILE:       /ut/thnet/struc.l
; PURPOSE:    data structures for ACME and ARCS
; PROGRAMMER: Paul Thagard
; CREATED:    4-4-88
; UPDATED:    7-26-88 - Added REMOVE_HYPOTHET and inserted it in GET_ARGS
;                       Added CLEAR_STRUC to remove unwanted structures.
;             8-3-89 -  updated concept code (at end)

(defun lstr () (load "//tinman/ucla/psych/emelz/ARCS/struc.l"))

; ************************************************************
;               DATA STRUCTURES AND ABSTRACTION
; *************************************************************
; MAKE_STRUC creates a structure with a name, type, and an unlimited
; number of fields.   E.g. a problem will be:
;     Name of structure:
;               structure type
;               start conditions
;               goals
;               constraints (optional)
; Each of some_fields has the structure:  
;      (property (list of propositions))

; Modification to make_struc (GHN, 6/23/88):
; If the flag use_selection_list? (nil by default) is non-nil, it will not
; make the structure unless it is listed in the global variable selection_list.
; This allows ARCS routines to select certain structures from the entire data-
; base which will be used on a given run.

(defun make_struc (name type &rest some_fields)
  (if (and use_selection_list?
	   (not (member name selection_list))
      )
      (return-from make_struc
		   (unless silent_map? (my_print "Structure ignored: " name))
      )
  )
  (setq all_structures (cons name all_structures))
  (put name 'data_type type)
  (do ((fields some_fields (cdr fields)))
      ((null fields) (unless silent_map? (my_print '"Structure made: " name)))
      (put name 'fields (cons (caar fields) (get name 'fields)))
      (put name (caar fields) (second (car fields)))
      ; set up propositions:
      (make_propns name (caar fields) (second (car fields)))
  )
)
; ************************************************************
; ADD_STRUC adds to an existing structure:

(defun add_struc (name field msgs)
   (put name field (union msgs (get name field)))
   (make_propns name field msgs)
)
; ************************************************************
; CLEAR_STRUC removes from memory an existing structure.

(defun clear_struc (name)
  (remprop name 'constraint_hyps) ; if this hasn't been removed
  (do ((fields (get name 'fields) (cdr fields)))
      ((null fields) nil)
      (remprop name (car fields))
  )
  (remprop name 'fields)
  (remprop name 'data_type)
  (mapcar #'clear_propn (get name 'propositions))
  (remprop name 'propositions)
  (setq all_structures (remove name all_structures))
  (unless silent_map? (my_print '"Structure cleared: " name))
)

(defun clear_propn (propn)
  (let ((struc (caar (get propn 'belongs_to)))
	(pred (pred_from_propn propn))
       )
    (remprop propn 'belongs_to)
    (remprop propn 'constraint_hyps)
    (setq all_propositions (remove propn all_propositions))
    (put pred 'from_propns (remove propn (get pred 'from_propns)))
    (if (put pred 'belongs_to (remove struc (get pred 'belongs_to)))
	nil (clear_pred pred)
    )
   )
  (remprop propn 'message)
)

(defun clear_pred (pred)
  (remprop pred 'constraint_hyps)
  (remprop pred 'from_propns)
  (remprop pred 'belongs_to)
  (setq all_preds (remove pred all_preds))
)

; ************************************************************

; Convention:  propositions are names like P1.  Messages are 
; lists:  (predicate (arguments) truth-value proposition-name)

; PROPNS_FROM_STRUC
(defun propns_from_struc (struc)
    (get struc 'propositions)
)
    
; GET_PROPN_NAME
(defun get_propn_name (message)
   (car (last message))
)
; GET_PRED
(defun get_pred (message)
   (car message)
)
; PRED_FROM_PROPN
(defun pred_from_propn (propn)
   (get_pred (get propn 'message))
)
; GET_ARGS
(defun get_args (message)
  (mapcar #'remove_hypothet (second message))
)
; REMOVE_HYPOTHET
(defun remove_hypothet (arg)
  (if (atom arg) arg (car arg))
)
; ARGS_FROM_PROPN
(defun args_from_propn (propn)
   (get_args (get propn 'message))
)
; GET_TR_VAL
(defun get_tr_val (message)
  (third message)
)
; TV_FROM_PROPN
(defun tv_from_propn (propn)
  (get_tr_val (get propn 'message))
)

; *********************************************************
; CONC_FROM_STRUC lists all the concepts in a structure.

(defun conc_from_struc (struc)
  (remove-duplicates (mapcar #'pred_from_propn (get struc 'propositions)))
)

; OBJ_FROM_STRUC

(defun obj_from_struc (struc)
  (union_map #'args_from_propn (get struc 'propositions))
)

; STRUC_FROM_PROPN returns the structure indexed by proposition number

(defun struc_from_propn (propn)
  (caar (get propn 'belongs_to))
)
; *********************************************************
; MAKE_PROPNS sets up propositions which have the structure:
; Name of proposition:
;     message
;     belongs_to: ((structure field) ...)

(defun make_propns (struc field lst_of_messages)
   (do ((msgs lst_of_messages (cdr msgs))
        (result nil)
        (propn nil)
       )
       ((null msgs) result)
       (setq propn (get_propn_name (car msgs)))
       (put propn 'message (car msgs))
       (put propn 'belongs_to
            (cons_if_new (list struc field) (get propn 'belongs_to))
       )
       (setq all_propositions (cons_if_new propn all_propositions))
       (put struc 'propositions
            (cons_if_new propn (get struc 'propositions))
       )
       (put (get_pred (car msgs)) 'belongs_to
            (cons_if_new struc (get (get_pred (car msgs)) 'belongs_to))
       )
       (put (get_pred (car msgs)) 'from_propns
            (cons_if_new propn (get (get_pred (car msgs)) 'from_propns)
	    )
       )
       (setq all_preds (cons_if_new (get_pred (car msgs)) all_preds))
   )
)   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the old code, just in case
;;; -eric 8/3/89      
;;;
;;; ************************************************************
;;; MCON (formerly MAKE_CONCEPT_A) is a bit different from make_concept in PI.
;;; As for structures, fields are:  ((field-name value) ...)
;;; Typical structure will have some of:
;;;   Concept name:
;;;       1. data_type:  concept
;;;       2. superordinates:  lst
;;;       3. subordinates:  lst
;;;       4. part-of: lst
;;;      5. sub-parts: lst
;;;       6. decomp: structured list, representing semantic decomposition
;;;               (maybe this should just be synonyms?)
;;;       7. rules:  lst of rule-names
;;;    Add to this:  antonyms.  Use when propositions are negated.
;;;
;;; There is some extra garbage around this stuff to provide a switch between
;;; the old version (symmetric_concepts) and the new.
;;;
;;;(if symmetric_concepts
;;;    (defun mcon (name fields &optional syntax)
;;;      (put name 'data_type 'concept)
;;;      (put name 'explicit t)
;;;      (setq all_concepts (cons_if_new name all_concepts))
;;;      (do ((flds fields (cdr flds)))
;;;	  ((null flds) (unless silent_map? (my_print '"Concept made: " name)))
;;;	  (put name
;;;	       (caar flds) 
;;;	       (union (get name (caar flds)) (second (car flds)))
;;;	  )
;;;	  (note_features name (caar flds) (second (car flds)))
;;;      )
;;;      (if syntax (put name 'syntax syntax))
;;;    )
;;;    (defun mcon (name fields &optional syntax)
;;;      (put name 'data_type 'concept)
;;;      (put name 'explicit t)
;;;      (setq all_concepts (cons_if_new name all_concepts))
;;;      (do ((flds fields (cdr flds)))
;;;	  ((null flds) (unless silent_map? (my_print '"Concept made: " name)))
;;;	  (put name
;;;	       (caar flds) 
;;;	       (union (get name (caar flds)) (second (car flds)))
;;;	  )
;;;	  (cond ((eq (caar flds) 'tenses)
;;;		 (do ((feats (second (car flds)) (cdr feats)))
;;;		     ((null feats) nil)
;;;		     (put (car feats) 'root-tense (list name))
;;;		 )
;;;		)
;;;		((eq (caar flds) 'plural)
;;;		 (do ((feats (second (car flds)) (cdr feats)))
;;;		     ((null feats) nil)
;;;		     (put (car feats) 'singular (list name))
;;;		 )
;;;		)
;;;	  )
;;;      )
;;;      (if syntax (put name 'syntax syntax))
;;;    )
;;;)
;;;
;;; ************************************************************
;;; NOTE_FEATURES sets up associations from feature to concepts
;;;
;;;(defun note_features (conc field lst)
;;;   (unless (eq field 'rules)
;;;           (associate_conc conc field lst)
;;;   )
;;;   (if (and (eq field 'rules)
;;;            (eq feature_selection 'loose)
;;;       )
;;;       (associate_conc conc 'rules (concs_from_rules lst))
;;;   )
;;;)
;;;
;;; ************************************************************
;;; ASSOCIATE_CONC puts the current concept into the appropriate
;;; field of each feature.
;;;
;;;(defun associate_conc (conc field feats)
;;;  (let (thisfield)
;;;    (cond
;;;     ((equal field 'synonyms) (setq thisfield 'synonyms))
;;;     ((equal field 'antonyms) (setq thisfield 'antonyms))
;;;     ((equal field 'superordinates) (setq thisfield 'subordinates))
;;;     ((equal field 'subordinates) (setq thisfield 'superordinates))
;;;     ((equal field 'sub-parts) (setq thisfield 'part-of))
;;;     ((equal field 'part-of) (setq thisfield 'subparts))
;;;     ((equal field 'tenses) (setq thisfield 'root-tense))
;;;     ((equal field 'plural) (setq thisfield 'singular))
;;;     ((equal field 'variations) (setq thisfield 'root-conc))
;;;    )
;;;    (do ((lst feats (cdr lst)))
;;;	((null lst) 'done)
;;;	(put (car lst) thisfield 
;;;	     (cons_if_new conc (get (car lst) thisfield))     
;;;	)
;;;	(setq all_concepts (cons_if_new (car lst) all_concepts))	
;;;    )
;;;   )
;;;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the new code
;;; -eric 8/3/89
; ************************************************************
; MCON (formerly MAKE_CONCEPT_A) is a bit different from make_concept in PI.
; As for structures, fields are:  ((field-name value) ...)
; Typical structure will have some of:
;   Concept name:
;       1. data_type:  concept
;       2. superordinates:  lst
;       3. subordinates:  lst
;       4. part-of: lst
;       5. sub-parts: lst
;       6. decomp: structured list, representing semantic decomposition
;               (maybe this should just be synonyms?)
;       7. rules:  lst of rule-names
;    Add to this:  antonyms.  Use when propositions are negated.

; There is some extra garbage around this stuff to provide a switch between
; the old version (symmetric_concepts) and the new.  By default, it's nil:

(defvar symmetric_concepts nil "Make all concept relationships bidirectional.  Much faster when set to nil.")

(if symmetric_concepts
    (defun mcon (name fields &optional syntax)
      (put name 'data_type 'concept)
      (put name 'explicit t)
      (setq all_concepts (cons_if_new name all_concepts))
      (do ((flds fields (cdr flds)))
	  ((null flds) (unless silent_map? (my_print '"Concept made: " name)))
	  (put name
	       (caar flds) 
	       (union (get name (caar flds)) (second (car flds)))
	  )
	  (note_features name (caar flds) (second (car flds)))
      )
      (if syntax (put name 'syntax syntax))
    )
    (defun mcon (name fields &optional syntax)
      (put name 'data_type 'concept)
      (put name 'explicit t)
      (setq all_concepts (cons_if_new name all_concepts))
      (do ((flds fields (cdr flds)))
	  ((null flds) (unless silent_map? (my_print '"Concept made: " name)))
	  (put name
	       (caar flds) 
	       (union (get name (caar flds)) (second (car flds)))
	  )
	  (cond ((eq (caar flds) 'tenses)
		 (do ((feats (second (car flds)) (cdr feats)))
		     ((null feats) nil)
		     (put (car feats) 'root-tense (list name))
		 )
		)
		((eq (caar flds) 'plural)
		 (do ((feats (second (car flds)) (cdr feats)))
		     ((null feats) nil)
		     (put (car feats) 'singular (list name))
		 )
		)
	  )
      )
      (if syntax (put name 'syntax syntax))
    )
)

; ************************************************************
; NOTE_FEATURES sets up associations from feature to concepts

(defun note_features (conc field lst)
   (unless (eq field 'rules)
           (associate_conc conc field lst)
   )
   (if (and (eq field 'rules)
            (eq feature_selection 'loose)
       )
       (associate_conc conc 'rules (concs_from_rules lst))
   )
)

; ************************************************************
; ASSOCIATE_CONC puts the current concept into the appropriate
; field of each feature.

(defun associate_conc (conc field feats)
  (let (thisfield)
    (cond
     ((equal field 'synonyms) (setq thisfield 'synonyms))
     ((equal field 'antonyms) (setq thisfield 'antonyms))
     ((equal field 'superordinates) (setq thisfield 'subordinates))
     ((equal field 'subordinates) (setq thisfield 'superordinates))
     ((equal field 'sub-parts) (setq thisfield 'part-of))
     ((equal field 'part-of) (setq thisfield 'subparts))
     ((equal field 'tenses) (setq thisfield 'root-tense))
     ((equal field 'plural) (setq thisfield 'singular))
     ((equal field 'variations) (setq thisfield 'root-conc))
    )
    (do ((lst feats (cdr lst)))
	((null lst) 'done)
	(put (car lst) thisfield 
	     (cons_if_new conc (get (car lst) thisfield))     
	)
	(setq all_concepts (cons_if_new (car lst) all_concepts))	
    )
   )
)
;;;
;;; end of new code
;;; -eric 8/3/89
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ************************************************************
; CONCS_FROM_RULES

(defun concs_from_rules (lst_of_rules)
    (my_print '"WARNING:  concs_from_rules not yet implemented.")
    nil
)



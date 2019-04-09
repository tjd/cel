; FILE:        acme.l
; PURPOSE:     analogical constraint mapping 
; PROGRAMMER:  Paul Thagard
; CREATED:     5-18-87
; UPDATED:     6-12-88
; UPDATED:     6-21-88 - objects now taken care of in make_obj_unit.
;                        make_obj_units rewritten as a do loop.
; UPDATED:     7-23-88 - Improvements to query mechanism
; UPDATED:     8-12-88 - check_importance added and made option for constraint
;                        map

(defun lac () (load "acme.l"))

; **************************************************
; CONSTRAINT_MAP uses a constraint network to do analogical
; mapping between two problems.

(defun constraint_map (struc1 struc2)
   (unless silent_map? (my_print '"Constructing analogical mapping between "
				 struc1 '" and " struc2
		       )
   )
   ; set up semantic activation unit:
   (if use_special? (and (if silent_map? t (my_print '"Using special unit."))
                         (put 'special 'activation .99)
                    ) ;else:
                    (unless silent_map? (my_print '"Not using special unit."))
   )
   ; note objects and concepts for sake of clear_net:
   ; the objects are now taken care of in make_obj_unit - GHN 6/21/88
   (setq all_preds (union (conc_from_struc struc1)
                          (conc_from_struc struc2)
                      )
   )

   ; note:  all_propositions set up by make_struc

   ; set up the network:
   (make_constraint_net struc1 struc2)
   (if use_auto_prag? (check_importance struc1))
)

; ***************************************************

; PROPNS_FROM_MSGS pulls out proposition names - last part of message.

(defun propns_from_msgs (msgs)
   (mapcar #'last_element msgs)
)

(defun last_element (lst) (car (last lst)))


; **************************************************
; MAKE_CONSTRAINT_NET sets up a network of hypotheses
; based on two structures that may be analogous.
; Each structure can have any number of fields.

(defun make_constraint_net (struc1 struc2)
 (let (links_num)
   (unless silent_map? (my_print '"Constructing constraint network for "
				 struc1 '" and " struc2
                       )
   )
   (setq *struc1* struc1)  
   ;; Added to facilitate presumed mappings with "new" units (see patches)
   (setq *struc2* struc2)  
   ; set up units and excitatory links for each field:
   
   (make_units_for_fields struc1 struc2)

   (unless silent_map?
	   (and (my_print "   Total number of units made: " (length all_units))
		(my_print "   Total number of symmetric excitatory links:  "
			  (/ total_links 2)
                )
           )
   )
   (setq links_num total_links)
   ; set up links for queries:
   (if look_for_queries? (mapcar #'make_query_links query_connections))
   
   ; set up inhibitory links among concept hypotheses:
   (unless silent_map? (my_print "   Making inhibitory links ..."))
   (inhibit_multiple_mappings (conc_from_struc struc1) inhib_weight)
   ; for 1-1 mapping:

   (if map_one_one? (inhibit_multiple_mappings (conc_from_struc struc2) 
                                           (* stop_many_one inhib_weight)
                    )
   )
   ; inhibitory links among object hypotheses (can be less than concepts)
   (inhibit_multiple_mappings (obj_from_struc struc1)
                          (* inhib_weight obj_conc_fraction)
   )
   ; for 1-1 mapping:
   (if map_one_one? (inhibit_multiple_mappings (obj_from_struc struc2)
                                               (* inhib_weight obj_conc_fraction)
                    )
   )
   ; inhibitory links to enforce 1-1 mapping of propositions:
   (inhibit_multiple_mappings all_propositions 
                              (* inhib_weight propn_uniqueness)
   )
   (unless silent_map?
	   (and (my_print "      Symmetric inhibitory links made:  "
			  (/ (- total_links links_num) 2)
                )
		(my_print  '"   Total symmetric links made:  "
			   (/ total_links 2) 
		)
           )
   )
   (length all_units)
 ); end let
)

; **************************************************
; MAKE_UNITS_FOR_FIELDS constructs units excitatory links
; for proposition mappings in particular fields.
; It ignores the different fields, combining them all
; into one big field, if map_all? is t.

(defun make_units_for_fields (struc1 struc2)
   (if map_all? 
       (and (combine_fields struc1)
            (combine_fields struc2)
       )
   )
   (do ((fields (get struc1 'fields) (cdr fields))
        (unit_num (length all_units))
        (link_num total_links)
       )
       ((null fields) 'done) 
       ; repeat:
       (setq unit_num (length all_units))
       (setq link_num total_links)
       (unless silent_map?
	       (my_print "   Making units and excitatory links for field " 
			 (car fields) " ..."
               )
       )
       (make_hyp_units struc1 struc2 (car fields))
       (unless silent_map?
	       (and (my_print "      Units made:  "
			      (- (length all_units) unit_num)
		    )
		    (my_print "      Symmetric links made:  "
			      (/ (- total_links link_num) 2)
		    )
               )
       )
   )
)

; **************************************************
; COMBINE_FIELDS conglomerates different fields into one.

(defun combine_fields (struc)
   (do ((fields (get struc 'fields) (cdr fields))
         (result nil)
        )
        ((null fields) 
         (put struc 'all result)
        )
        (setq result (union result
                            (get struc (car fields))
                     )
        )
     )
     (put struc 'fields '(all))
)




; **************************************************
; MAKE_HYP_UNITS sets up units representing hypotheses that
; predicates and their associated objects are analogous.
; New: also does propositions.

(defun make_hyp_units (prob1 prob2 field)
   (do ((msgs (get prob1 field) (cdr msgs)))
       ; exit:
       ((null msgs) 'done)
       ; repeat:
       (make_hyp_units_for_msg  (car msgs)
                                (get prob2 field)
       )
   )
)

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
   ; all the rest:
   (do ((msgs messages (cdr msgs)))
        ; exit:
        ((null msgs) 'done)
        ; repeat:
        (make_hyp_unit msg (car msgs))
    )
)

; ********************************************************
; MAKE_HYP_UNIT creates units corresponding to the hypotheses
; that two messages are analogous, i.e. that their concepts, arguments,
; and whole propositions correspond.
; Returns a list of the concept and proposition mappings created.
; Only concepts with same # of arguments are candidates.
; Structure of units:
;    Unit-name
;       Activation:  -1 ... 1
;       Linked_from:  list of units.

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
				  (sem_sim_arcs conc1 conc2)
				  (sem_similarity conc1 conc2)
			      )
			)
                        (if sem_sim?
                            (make_sym_link 'special new_conc_unit 
                                           sem_sim?
                            )
                         )
                         ; record hypotheses about a concept:
                         (record_hypothesis conc1 new_conc_unit)
                         (record_hypothesis conc2 new_conc_unit)
                      )
                ); end cond 2
                ; create proposition unit:
                (setq new_propn_unit (catname propn1 propn2))
                (put new_propn_unit 'concerns (list propn1 propn2))
                (note_unit new_propn_unit)
                (record_hypothesis propn1 new_propn_unit)
                (record_hypothesis propn2 new_propn_unit)
                (setq result (cons new_propn_unit result))
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
; *******************************************************************
; TYPE_COMPATIBLE ensures for all members of two sequences (A B C) and (1 2 3)
; that if A is a member of a set, then so is 1.

(defun type_compatible (lst1 lst2 set)
   (do ((seq1 lst1 (cdr seq1))
        (seq2 lst2 (cdr seq2))
       )
       ; if everything ok, return t.
       ((null seq1) t)
       ; check all pairs:
       (if (or (and (member (car seq1) set)
                    (not_member (car seq2) set)
               )
               (and (not_member (car seq1) set)
                    (member (car seq2) set)
               )
            )
            (return nil)
        )
    )
)


; *******************************************************************
; RECORD_HYPOTHESIS notes that a unit provides one of the possible 
; mappings for  concept, object, or proposition.

(defun record_hypothesis (thing unit)
    (put thing 'constraint_hyps 
         (cons_if_new unit (get thing 'constraint_hyps))
    )
)
; *************************************************************
; CONTAINS_QUERY checks whether a list contains a query to be 
; filled in, as indicated by an argument that starts with a "?".

(defun contains_query (list_of_atm)
   (do ((lst list_of_atm (cdr lst))
        (result nil)
       )
       ; exit:
       ((null lst) result) 
       ; repeat:
       (if (atom_begins (car lst) #\?) 
           (setq result (cons (car lst) result))
       )
   )
)
 

; *************************************************************
; MAKE_QUERY_LINKS sets up links based on a queries.     
; The basic idea here is that units that provide a chance of
; answering a query will be favored over those that don't.

(defun make_query_links (lst_of_four)
  (let ((propn_unit (car lst_of_four))
        (conc_unit (second lst_of_four))
        (arguments1 (third lst_of_four))
        (arguments2 (fourth lst_of_four))
       )
 
   (unless silent_map?
	   (my_print '"Making query links:  " propn_unit " " conc_unit '" "
		     arguments1 '" " arguments2
           )
   )
   (do ((args1 arguments1 (cdr args1))
        (args2 arguments2 (cdr args2))
        (result_args1 arguments1)
        (result_args2 arguments2)
        (new_obj_units nil)
        (query_args nil)
       )
       ; exit:
       ((null args1)
        ; make normal object units:
        (setq new_obj_units
              (make_obj_units result_args1 
                              result_args2 
                              default_activation
               )
        )
        (cond ((and look_for_queries? (not silent_map?))
                (my_print '"DEBUGGING make_query_links ")
                (my_print '"new_obj_units " new_obj_units)
                (my_print '"result args " result_args1 result_args2)
              )
        )				 
        ; make normal links:

        (make_sym_link propn_unit 
                       conc_unit
                       excit_weight
        )
        (make_excit_links propn_unit
                          new_obj_units
                          excit_weight
        )
        ; make special query links:
        (make_query_links_for_objs query_args 
                          (cons conc_unit new_obj_units)
        )
       )
       ; repeat:
       (cond ( (atom_begins (car args2) #\?)
               (setq query_args (cons (car args1) query_args))
               (setq result_args1 (remove (car args1) result_args1))
               (setq result_args2 (remove (car args2) result_args2))
                              	       
             )
       )
   ) ; end do
   ) ; end let
)
; **************************************************************       
; MAKE_QUERY_LINKS_FOR_OBJS makes links among each member of a set of
; constraint hypotheses (e.g. all hypotheses concerning obj1)
; and the pragmatic unit.

(defun make_query_links_for_objs (list_of_objects units)
   (create_pragmatic_unit)
   (do ((objs list_of_objects (cdr list_of_objects)))
       ; exit:
       ((null objs) 'done)
       ; repeat:
       (make_query_links_for_obj (car objs) units)
   )
)
 
; *************************************************************
; MAKE_QUERY_LINKS_FOR_OBJ does it for one object.  

(defun make_query_links_for_obj (obj units)
   (do ((unts units (cdr unts)))
       ; exit
       ((null unts) 'done)
       ; repeat:
       (make_excit_links 'pragmatic
                         (get obj 'constraint_hyps)
                         import_weight
       )
       (unless silent_map? 
	       (my_print "Units important for querying:  "
			 (get obj 'constraint_hyps)
               )
       )
    )
)

; **********************************************************
; NUMBER_DUPS looks for pairs of duplicate arguments:  e.g. 
; (a b a) and (d e d).  Not quite perfect.

(defun number_dups (list1 list2) 
   (do ((lst (make_pairs list1 list2) (cdr lst))
        (result 1)
       )
       ; exit:
       ((null lst) result)
       ; repeat:
       (if (memberlist (car lst) (cdr lst))
           (setq result (+ result 1))
        )
    )
)

; **************************************************************
; MAKE_PAIRS takes two lists, and returns a list of pairs,
; with the nth atom of list1 paired with the nth of list2.
; [from pi/analog.l].

(defun make_pairs (lst1 lst2)
   (cond ( (not_equal (length lst1) (length lst2)) nil)
         ( (null lst1) nil )
         ( t (cons (list (car lst1) (car lst2))
                   (make_pairs (cdr lst1) (cdr lst2))
             ) 
         )
   )
)

; ***********************************************************
; SEM_SIMILARITY calculates the semantic similarity between
; two concepts.  It should use an overlapping feature count,
; but see particular data files for sim_lists.
       
(defun sem_similarity (conc1 conc2)
    (cond ( (eq conc1 conc2) ident_weight) ; concepts are the same
          ( (eq conc2 'no-concept) no-concept_weight) ; null mapping
          (t (or (second (assoc (list conc1 conc2) sim_list :test #'equal))
                 (second (assoc (list conc2 conc1) sim_list :test #'equal))
                 no_sim_weight
             )
          )
     )
)

; SIMILAR

(defun similar (conc1 conc2 val)
     (setq sim_list (cons (list (list conc1 conc2) val) sim_list))
)

; NOSIMILAR

(defun nosimilar ()
     (setq sim_list nil)
)




; Here's how it should be if Wordnet were comprehensive enough:
; SEM_SIMILARITY_FUTURE  calculates the semantic similarity between
; two concepts.  Typically, this should be:
; identical:  .1
; synonymous: .08
; coordinates: .06 (i.e. are both kinds of same superordinate,
;                        or parts of same whole)
       
(defun sem_similarity_future (conc1 conc2)
    (cond ( (eq conc1 conc2) ident_weight) ; concepts are the same
          ( (eq conc2 'no-concept) no-concept_weight) ; null mapping
          ( (synonymous conc1 conc2) synon_weight)
          ( (coord conc1 conc2) coord_weight)
          (t no_sim_weight)
    )
)
; *************************************************
; SYNONYMOUS

(defun synonymous (conc1 conc2)
    (or (memberlist (list conc1 conc2) synonyms)
        (memberlist (list conc2 conc1) synonyms)
    )
)

; COORD identifies concepts that are parts of the same whole or
; subkinds of the same kind.

(defun coord (conc1 conc2)
    (or (memberlist (list conc1 conc2) same_kinds)
        (memberlist (list conc2 conc1) same_kinds)
        (memberlist (list conc1 conc2) same_parts)
        (memberlist (list conc2 conc1) same_parts)
    )
)
 
; **************************************************
; MAKE_OBJ_UNITS_FROM_OBJECTS sets up hypotheses about 
; about object maps.

(defun make_obj_units_from_objects (struc1_objs struc2_objs)
   (do ((s1_objs struc1_objs (cdr s1_objs))
        (units_made nil)
       )
       ; exit: return units made:
       ((null s1_objs) units_made)
       ; while repeating:
       (setq units_made
             (union units_made
                    (make_obj_units_from_1_obj (car s1_objs)
                                               struc2_objs
                    )
             )
        )
   )
)

       
; ***************************************************
; MAKE_OBJ_UNITS_FROM_1_OBJ does it for 1.

(defun make_obj_units_from_1_obj (obj objects)
   (do ((obs objects (cdr obs))
        (units_made (if use_nothing_maps? 
                        (list (make_obj_unit obj 'no-object default_activation)))
                        ;else
                        nil
        )
       )
       ; exit: return units
       ((null obs) (remove nil units_made))
       ; repeat:
       (setq units_made 
             (cons (make_obj_unit obj (car obs) default_activation)
                   units_made
             )
       )
   )
)

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
                         (other_element_maps quer (car quer_maps))
                         excit_weight
       )
       (make_inhib_links (get quer 'constraint_hyps) inhib_weight)
   )
)    

; **************************************************
; OTHER_ELEMENT_MAPS gives for an element E all
; the units concerning the mapping of all elements that
; map to E.

(defun other_element_maps (el unit)
  (remove unit 
	  (get (other_from_pair el
				(get unit 'concerns)
               )
	       'constraint_hyps
          )
  )
)

; ****************************************************
; To calculate the best match:
; ****************************************************
; BEST_ANALOGY uses the hypothesis units to judge what is the 
; best overall match:

(defun best_analogy (prob)
   (unless silent_run?
	   (my_print '"Calculating the best mappings after "
		     total_times '" cycles."
           )
   )
   (setq best_matches nil)
   (mapcar #'best_match (union (conc_from_struc prob)
                               (obj_from_struc prob)
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

(defun ba () (best_analogy *struc1*))

; *******************************************************************
; FIND_ANALOG_MESSAGES constructs analogous messages.

(defun find_analog_messages (lst_of_preds prob part_of_prob)
   (do ((preds lst_of_preds (cdr preds)))
       ; exit:
       ((null preds) nil)
       ; repeat:
       (find_analog_message (car preds) prob part_of_prob)
   )
)


; *******************************************************************
; FIND_ANALOG_MESSAGE constructs an analogous message for a predicate
; based on the best match of objects.

(defun find_analog_message (pred problem part_of_problem)
   (do ((msgs (get problem part_of_problem) (cdr msgs))
        (result nil)
       )
       ; exit:
       ((null msgs) nil)
       ; repeat:
       (cond ( (> (sem_similarity pred (caar msgs)) coord_weight)
               (setq result (list (caar msgs)
                                  (mapcar #'best_match (get_args (car msgs)))
                            )
               )
               (my_print '"Analogous message is " result)
               (return result)
             )
        )
   )
)

; ****************************************************************
; LIST_OF_N_ELEM makes a list consisting of n occurrences of an element.

(defun list_of_n_elem (number element)
    (do ((num number (- num 1))
         (lst nil)
        )
        ((= 0 num) lst)
        (setq lst (cons element lst))
    )
)

; ************************************************************

(defun coa () (compile-file "acme.l") (fasload "acme.2bin"))

; END of acme.l




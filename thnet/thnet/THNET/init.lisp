
; FILE:       init.l
; PURPOSE:    initializations for networks
; PROGRAMMER: Paul Thagard, Greg Nelson
; CREATED:    12-9-87
; UPDATED:    5-13-88
; UPDATED:    9-12-88 -- init_net now makes a hash table for links.
; UPDATED:    9-27-88 -- init_net now does nothing for links (see links.l)

(defun lin () (load "//tinman/ucla/psych/emelz/ARCS/init.l"))

; does not need compiling.
; **********************************************************************
; INIT_NET prints a pretty message.  That's it.

(defun init_net ()
   (my_print "Welcome to THNET, incorporating ACME, ARCS, and ECHO.")
   (my_print "Version 1.1, September 1988.")
   (my_print "This program is copyright (c) Paul Thagard and Greg Nelson, 1988.")
   (my_print "Permission is granted for use for research purposes only.")
   (my_print " ")
)

; ******************************************************
;         Global variables:
; ******************************************************
; for general net use:

(defvar trace_data t)
(defvar all_units nil)
(defvar total_links 0)
(defvar excit_links 0)
(defvar inhib_links 0)
(defvar total_sym_links 0)
(defvar default_link 0)
(defvar inhib_weight -.04)
(defvar excit_weight .04)
(defvar default_activation .01)
(defvar total_times 0)
(defvar testnum 0)
(defvar experiment nil)
(defvar when_to_print '(1 10 100 500 1000 1500 2000))
(defvar all_problems nil)
(defvar all_preds nil)
(defvar all_concepts nil)
(defvar all_objects nil)
(defvar decay_amount .01 "theta in PDP: rate of decay")     
(defvar decay_register .01 "used in function unexplained")
(defvar check_unexplained nil "check for unexplained data")
(defvar stop_run? nil)
(defvar silent_map? nil)            ; turn off printing during mapping?
(defvar silent_run? nil)            ; turn off printing during run?
(defvar use_special? t)
(defvar min_activation -.99)        ; minimum activation of units
(defvar max_activation .99)         ; maximum activation of units
(defvar grossberg? nil)             ; use Grossberg rule?
(defvar tversky? nil)               ; use altered form when act. is negative?
(defvar current_excit 0.0)          ; for Grossberg rule.
(defvar current_inhib 0.0)          ;        "
(defvar output_threshold 0.0)       ; minimum activation unit must have for output
(defvar *acme-mode* nil)            ; for grapher
(defvar settled? nil)               ; has network reached asymptote?
(defvar asymptote .001)             ; unit must changed less than this on
                                    ; a cycle to be judged to have asymptoted
(defvar stop_settled? t)            ; stop if the network has settled.
(defvar asymptoted_units nil)       ; units that have reached asymptote
(defvar min_settle 25)              ; minimum settling time
(defvar *acme_loaded* nil)          ; variables to tell which sections of
(defvar *arcs_loaded* nil)          ; code have been loaded
(defvar *echo_loaded* nil)
(defvar load_graphics? nil)
(defvar *grapher_loaded* nil)
(defvar *misc_loaded* nil)
(defvar *starsearch-loaded* nil)    ; Parallel search routines loaded
(defvar *optimized* nil)            ; whether memory optimization is done
(defvar *eric-debug* nil)           
(defvar *last-prop* nil)            ; specifies the prop after which cwsg should begin
                                    ; creating new props (e.g. if last prop is T25,
                                    ; *last-prop should be '(t 25)
(defvar *inhibiting-unimportant* nil) ; temporary flag when _inhib_important
                                    ; routines are called.
; for analogy in ACME: 

(defvar *struc1* nil)
(defvar *struc2* nil)               ; Added to faciliated presumed mappings of nonexistent units
(defvar start_units nil)
(defvar goal_units nil)
(defvar effector_units nil)
(defvar object_units nil)
(defvar sim_list nil)
(defvar no_sim_weight 0.0)
(defvar obj_conc_fraction 1 )       ; inhibition of object hypotheses
(defvar stop_many_one 1)            ; if > 1, discourages many-one mappings
(defvar propn_uniqueness 1)         ; enforces 1-1 mappings of propositions
(defvar map_all? nil)               ; map regardless of fields

(defvar ident_weight .1)            ; similarity of concept to self
(defvar synon_weight .08)           ; similarity of synonyms
(defvar coord_weight .06)           ; similarity of coordinates 
(defvar synonyms nil)               
(defvar same_kinds nil)
(defvar same_parts nil)

(defvar map_one_one? t)             ; do 1-1 mapping
(defvar no-concept_weight 0.0)      ; weight from special to null units
(defvar stop_when_matched? nil)     ; for automatic stopping on correct answer
(defvar best_matches nil)
(defvar desired_matches nil)
(defvar use_nothing_maps? nil "If t, construct no-concept maps.")


(defvar look_for_queries? nil)      ; for query arguments
(defvar query_connections nil)
(defvar query_weight_proportion 1)
(defvar link_concepts_objects? t)   ; link concept hyps to object hyps directly
(defvar link_objects? t)              ; make links between object mappings
(defvar watch_for_dup_arguments? nil) ; see dup_arguments
(defvar pragmatic_unit_made nil)      ; for pragmatic constraint

(defvar show_others? t)               ; show other good maps
(defvar min_good .2)                  ; good enough to notice
(defvar prag_weight .3)               ; weight to pragmatic unit for presumed
(defvar import_weight .1)             ; importance links to pragmatic unit

(defvar use_arcs_semantics? nil)      ; to use arcs semantics file to make
                                      ; similarity judgements
(defvar use_auto_prag? nil)     ; to have function check_importance invoked

; for transfer in ACME:
(defvar *prop-threshold* .1)          ; prop act < this to transfer to transfer prop
(defvar *elt-threshold* .1)           ; element act > this to consider "mapped"
(defvar *transfer-type* nil)          ; either 'cws, 'cwsg or nil (no transfer)
(defvar *transfer-fields* nil)        ; list of pairs of the form (struc field) to
                                      ; perform transfer on.

; for explanation in ECHO:

(defvar all_explainers nil)
(defvar all_explained nil)
(defvar explan_data nil)
(defvar data_excit .05)         ; excitation of data
(defvar special_activation 1)
(defvar all_propositions nil)
(defvar all_data nil)
(defvar data_self_links? nil)
(defvar analogy_impact 1)       ; impact of analogy
(defvar simpl_impact 1)         ; impact of simplicity
(defvar co-hyp-importance 1)    ; if < 1, links between cohypotheses are less.
(defvar self_link_data nil)
(defvar data_init_activ .01)
(defvar init_activ .01)
(defvar entail_impact 1)        ; if > 1, entailment excites more than expln.
(defvar contradictions nil)     ; pairs of contradictory propns
(defvar help_hier? nil)         ; help higher level hypotheses
(defvar explained_away_weight .001)
(defvar ad-hoc_factor 1 "decrease excitatory links for ad hoc explanations")

; for parallel routines

(defvar *staracme-loaded* nil)
; for the grapher

(defvar *last-init* nil "Name of grapher last initialized.")
(defvar *trace-list* nil "List of units to trace.")

; for retrieval in ARCS:

(defvar load_arcs? nil "Save time by not loading arcs unless explicitly set.")
(defvar mapped_concepts nil "Those concepts for which a mapping has been found")
(defvar mapped_propositions nil)
(defvar mapped_structures nil)
(defvar all_structures nil)
(defvar structures_loaded nil)

; The following flags determine which (if any) features should be used as
; retrieval cues.  If use_features? is non-nil it will take precedence, that
; is, use_synonyms? is only evaluated if use_features? is nil.

(defvar use_features? nil "Use all associated features as probe concepts.")
(defvar use_synonyms? t "Use synonyms as probe concepts.")

(defvar link_strucs_concs? t "Create links between concepts and strucs.")

(defvar probe_self? nil "Allow a probe to turn up the structure itself.")

(defvar feature_selection 'tight)
; If feature_selection is tight, consider only hieararchial features
; and part of semantic decomposition.
; If loose, consider also features of rules.

(defvar unwanted_preds '(conjoin-object cause conjoin-event if become-true become-false) "Predicates not to be used for retrieval purposes.")

(defvar use_selection_list? nil "Put make_struc in the selective mode, only making certain structures.")
(defvar selection_list nil "Which structures are to be made on this pass, if in selective mode.")

(defvar use_root_concs? t "Whether to use the root concept when given a plural or a different tense.")
(defvar swap_when_false? t "Whether to exchange synonyms and antonyms when the
truth value is false.")
(defvar include_ants? t "Whether to include antonyms.")
(defvar include_sub_parts? t "Whether to include subparts.")
(defvar include_part_ofs? t "Whether to include part of relationships (holonyms).")
(defvar include_decomp? nil "Whether to include decomp, whatever that is.")
(defvar subs_from_supers? t "Whether to include the subordinates of immediate superordinates.")
(defvar shared_sub-parts? t "Whether to include things which share sub-parts.")
(defvar superord_distance 1 "Tells how far to proceed up the heirarchy.")

(defvar base_sim_weight .1 "Maximum strength of connection to semantic unit.")

; These are the values which I came up with off the top of my head.
(defvar self_sim_weight 1 "Semantic weight for identical predicates.")
(defvar syn_sim_weight .6 "For synonyms.")
(defvar superord_sim_weight .3 "For superordinates.")
(defvar sub_of_super_sim_weight .25 "For subordinates of superordinates.")
(defvar subord_sim_weight .2 "For subordinates.")
(defvar higher_super_sim_weight .2 "For higher superordinates.")
(defvar holo_sim_weight .1 "For holonyms (part-ofs).")
(defvar shared_sub-part_sim_weight .1)
(defvar part_sim_weight 0.0 "For sub-parts.")
(defvar ant_sim_weight -.4 "For antonyms.")

(defvar symmetric_concepts nil "Make all concept relations symmetric.")

(defvar simple_semantic_weight? t "Whether to use VERSION B of weighting.")

(defvar auto_importance? t "Whether to automatically add importance info for problems.")
(defvar propns_import? t "Whether consider propositions important.")

; THESE ARE THE VALUES USING THE WORD ASSOC. DATA
; What I need to do is to figure out a proportional system, where the most
; likely retrievals are reasonably related to the word.  What I want is a scale
; from -1 to 1, with 1 representing identical predicates, and 0 representing
; no positive or negative similarity.

; This is really not going to be very good.  However...

; We have to assume this, because no one in the word association tasks gave
; back the same word.  This makes this hard to use as a standard of comparison.
;(defvar self_sim_weight 1 "Semantic weight for identical predicates.")
;(defvar syn_sim_weight .33 "For synonyms.")
;(defvar superord_sim_weight .3 "For superordinates.")
;(defvar sub_of_super_sim_weight .05 "For subordinates of superordinates.")
;(defvar subord_sim_weight .1 "For subordinates.")
;(defvar higher_super_sim_weight .11 "For higher superordinates.")
;(defvar ant_sim_weight .26 "For antonyms.")
;(defvar holo_sim_weight 0 "For holonyms (part-ofs).")
;(defvar part_sim_weight 0 "For sub-parts.")


; For MENUS:
(defvar *all-fables* '(fable1 fable2 fable3 fable4 fable5 fable6 fable7 fable8 fable9 fable10 fable11 fable12 fable13 fable14 fable15 fable17 fable18 fable19 fable20 fable21 fable22 fable23 fable24 fable25 fable26 fable27 fable28 fable29 fable30 fable31 f

able33 fable34 fable35 fable36 fable37 fable38 fable39 fable40 fable41 fable42 fable43 fable44 fable45 fable46 fable47 fable48 fable49 fable50 fable51 fable52 fable53 fable54 fable55 fable56 fable57 fable58 fable59 fable60 fable62 fable63 fable64 fable65 

fable66 fable67 fable68 fable69 fable70 fable71 fable72 fable73 fable74 fable75 fable76 fable77 fable78 fable79 fable80 fable81 fable82 fable83 fable84 fable85 fable86 fable87 fable88 fable89 fable90 fable91 fable92 fable93 fable94 fable95 fable96 fable97

 fable98 fable99 fable100 fable101 fable102 fable103))

(defvar *all-plays* '(cymbeline richard-ii henry-v romeo-and-juliet othello julius-caesar king-lear antony-and-cleopatra hamlet macbeth merry-wives alls-well measure-for-measure merchant-of-venice tempest two-gentlemen as-you-like-it much-ado twelfth-nigh

t loves-labours winters-tale midsummer-night taming-shrew timon-of-athens pericles))


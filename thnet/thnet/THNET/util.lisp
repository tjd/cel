
; FILE:       net/util.l
; PURPOSE:    untility functions for networks
; PROGRAMMER: Paul Thagard
; CREATED:    12-6-87
; UPDATED:     5-4-88

(defun lut  () (load "//tinman/ucla/psych/emelz/ARCS/util.l"))


; ******************************************************
;          Functions for changing parameters.
; ******************************************************
; GROSS_ON and GROSS_OFF turn on and off the use of the Grossberg
; updating rule.

(defun gross_on () 
    (if grossberg? (or silent_map? (my_print '"Already using Grossberg rule."))
        ; else:    
        (and (setq grossberg? t)
             (or silent_map? (my_print '"Now using Grossberg updating rule."))
        )
    )
)

(defun gross_off () 
    (if (null grossberg?)
        (or silent_map? (my_print "Already using Rumelhart and McClelland updating rule."))
        ; else:
        (or (setq grossberg? nil)
	    (or silent_map? (my_print '"Now using Rumelhart and McClelland updating rule."))
        )
    )
)

(defun tversky_on ()
  (if tversky? (or silent_map? (my_print "Already using altered negative activation rule."))
    (and (setq tversky? t)
	 (or silent_map? (my_print "Now using altered negative activation rule."))
    )
  )
)

(defun tversky_off ()
  (if (null tversky?) (or silent_map? (my_print "Altered rule is not in use."))
    (or (setq tversky? nil)
	 (or silent_map? (my_print "No longer using altered negative activation rule."))
    )
  )
)

; ****************************************************************
; WTP determines when to print system information while running
; the net.  At every timestep on the list, info is presented.
; The run stops with the last timestep on the list.

(defun wtp (lst)
    (setq when_to_print lst)
    (my_print '"Printing results at cycles: " lst)
)
; ***************************************************
; STOP says to stop network if it has settled.

(defun stop ()
   (setq stop_settled? t)
)

; UNSTOP

(defun unstop ()
   (setq stop_settled? nil)
)

; ***************************************************

; INHIB
(defun inhib (wt)
   (cond ( (equal silent_map? t))
	 ( (= wt inhib_weight)
           (my_print '"Inhibition unchanged at " wt)
         )
	 ( (> wt inhib_weight) 
           (my_print '"Inhibition weakened from " inhib_weight '" to " wt)
         )
         ( t (my_print '"Inhibition intensified from " inhib_weight '" to " wt))
   )
   (setq inhib_weight wt)
)
; DECAY
(defun decay (num)
   (cond ( (equal silent_map? t))
	 ( (= num decay_amount)
           (my_print '"Decay unchanged at " num)
         )
         ( (> num decay_amount)
           (my_print '"Decay rate increased from " decay_amount '" to " num)
         )
         ( t (my_print '"Decay rate decreased from " decay_amount '" to " num))
   )
   (setq decay_amount num)
)

; EXCIT
(defun excit (num)
   (cond ( (equal silent_map? t))
         ( (= num excit_weight)
           (my_print '"Excitation unchanged at " num)
         )
         ( (> num excit_weight)
           (my_print '"Excitation increased from " excit_weight '" to " num)
         )
         (t (my_print '"Excitation decreased from " excit_weight '" to " num))
   )
   (setq excit_weight num)
)

; DATA_EXCIT
(defun data_excit (num)
   (cond ( (equal silent_map? t))
	 ( (= num data_excit)
           (my_print '"Data excitation unchanged at " num)
         )
         ( (> num data_excit)
           (my_print '"Data excitation increased from " data_excit '" to " num)
         )
         (t (my_print '"Data excitation decreased from " data_excit '" to " num))
   )
   (setq data_excit num)
)

; SIMPLE   
(defun simple (num)
   (if (> num simpl_impact)
       (my_print '"Simplicity impact increased from " simpl_impact '" to " num)
       ; else
       (my_print '"Simplicity impact decreased from " simpl_impact '" to " num)
   )
   (setq simpl_impact num)
)

; OUTPUT 

(defun output (num)
   (cond ( (equal silent_map? t))
	 ( (= num output_threshold)
           (my_print '"Output threshold unchanged at " num)
         )
         ( (> num output_threshold)
           (my_print '"Output theshold increased from " output_threshold '" to " num)
         )
         ( t (my_print '"Decay rate decreased from " output_threshold '" to " num))
   )
   (setq output_threshold num)
)

; PROPN (for ACME)

(defun propn (num)
   (if (> num propn_uniqueness)
       (my_print '"Proposition uniqueness increased from " propn_uniqueness '" to " num)
       ; else
       (my_print '"Proposition uniqueness decreased from " 
                 propn_uniqueness '" to " num
       )
   )
   (setq propn_uniqueness num)
)

; AD-HOC

(defun ad-hoc (num)
   (cond ((< ad-hoc_factor num)
          (my_print '"Ad hoc factor increased from " ad-hoc_factor '" to " num)
          (setq ad-hoc_factor num)
         )
         ((> ad-hoc_factor num)
          (my_print '"Ad hoc factor decreased from " ad-hoc_factor '" to " num)
          (setq ad-hoc_factor num)
         )
         (t (my_print '"Ad hoc factor was already at " num))
   )
)
; ****************************************************

; DEFAULTS

(defun defaults (mode)
    (cond ((eq mode 'acme) 
           (decay .1)
           (inhib -.2)
           (excit .1)
	   (setq link_concepts_objects? t)
           (gross_on)
	   (output 0)
          )
          ((eq mode 'echo)
           (decay .05)
           (inhib -.2)
           (excit .05)
           (data_excit .1)
           (output -.99)
           (wtp '(110))
	   (dbn)
           (gross_off)
          )
          ((eq mode 'arcs) ; only preliminary
           (decay .2)
           (inhib -.2)
           (excit .05)
	   (setq link_concepts_objects? nil)
	   (setq link_strucs_concs? t)
           (gross_on)
	   (output 0)
          )
     )
)
(defun dfa () (defaults 'acme))
(defun dfe () (defaults 'echo))
(defun dfr () (defaults 'arcs))


; ****************************************************
;          Functions for examining networks.
; *****************************************************

; SHOW_UNITS displays the activation of units concerning how a
; concept or object is to be mapped.

(defun show_units (conc_or_obj)
   (mapcar #'print_activation
	   (sort (get conc_or_obj 'constraint_hyps)
		 '(lambda (les grt) (> (get les 'activation) (get grt 'activation)))
	   )
   )
)
 
(defun print_activation (unit)
     (my_print unit '" has activation: "
               (get unit 'activation)
     )
     (if *echo_loaded* (my_print '"  " (get unit 'propn)))
)
         
(defun shu (c) (length (show_units c)))

; ********************************************************

(defun prus (units) (mapcar 'pr_unit units))


; ****************************************************************
; PR_UNIT

(defun pr_unit (unit)
   (my_print '"Activation of " unit '" is " (get unit 'activation))
   (print_links unit)
)

(defun pru (unit) (pr_unit unit))

(defun prm (el) 
    (prus (get el 'constraint_hyps))
)

(defun print_link (unit linked)
   (my_print '"  Link from " linked 
                            '" (activation: "
                            (get linked 'activation)
                            '") has weight "
                            (weight_of_link_between linked unit)
    )
)

(defun compare_units (unit1 unit2)
    (my_print '"Activation of " unit1 '" is " (get unit1 'activation))
    (my_print '"Activation of " unit2 '" is " (get unit2 'activation))
    (compare_links unit1 unit2)
    (compare_links unit2 unit1)
)

(defun compare_links (unit1 unit2)
    (my_print '"Links to " unit1 '" but not to " unit2 '" are:")
    (print_links2 unit1  (set-difference (get unit1 'linked_from)
                                         (get unit2 'linked_from)
                         )       
    )
)

(defun print_links2 (unit links)
   (do ((lks links (cdr lks)))
       ((null lks ) 'done)
       (print_link unit (car lks))
   )
)

; ************************
; MAX_EXCIT calculates (approximately) the
; maximum value that the excit parameter can have without causing
; problems through too much activation going to a unit.
; BUG:  this doesn't correspond well to the values found by 
; sensitivity analyses.

(defun max_excit ()
    (do ((units (cdr all_units) (cdr units))
         (worst (car all_units))
        )
        ((null units)
         (my_print worst " is the unit with most excitatory inputs.")
         (my_print "  It has " (excitations worst) " excitatory links,")
         (my_print "  So excitation should approximately be less than "
                   (/ 1.0 (excitations worst))
         )
        )
        ;
        (if (> (excitations (car units))
               (excitations worst)
            )
            (setq worst (car units))
        )
    )
)

; EXCITATIONS is the number of excitatory links to a unit
     
(defun excitations (unit)
   (if *acme-mode* (length (get unit 'linked_from))
       ; else
       (- (length (get unit 'linked_from))
          (length (get unit 'contradicts))
       )
   )
)

; ****************************************************
; PRINT_NET prints out relevant info about each unit.
(defun print_net ()
    (mapcar #'(lambda (unit)
                  (my_print '" ")
                  (my_print '"Unit " unit)
                  (my_print '"  Activation: " (get unit 'activation))
                  (print_links unit)
               )
               (sort all_units #'string-lessp)
    )
    (my_print '"Net printed.")
    ; (print_good)
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
             (sort (get unit 'links_from) '(lambda (a b) (string-lessp (car a) (car b))))
   )
)

; ******************************************************************

; **********************************************************************
; PRINT_VALUES gives current values of key variables.

(defun print_values (acme_or_echo)
    (my_print '"Current network parameters:")
    (my_print '"   Decay: " decay_amount)
    (my_print '"   Excitation: " excit_weight)
    (my_print '"   Inhibition: " inhib_weight)
    (my_print '"   Total number of units: " (length all_units))
    (my_print '"   Total number of symmetric links: " (/ total_links 2))
    (my_print '"   Total number of links: " total_links)
    (my_print '"   Threshold for output from units: " output_threshold)
    (my_print '"   Minimum activation: " min_activation)
    (my_print '"   Maximum activation: " max_activation)
    (my_print '"   Asymptote criterion:  " asymptote)

  ; for ACME:
  (cond ((or (eq acme_or_echo 'acme) (eq acme_or_echo 'arcs))
          (my_print '"   Concept inhibition: " inhib_weight)
          (my_print '"   Object inhibition: " (* inhib_weight obj_conc_fraction))
          (my_print '"   Proposition inhibition: " (* inhib_weight propn_uniqueness))
          (my_print '"   Similarity of identical predicates: " ident_weight)
          (my_print '"   Predicate similarities:  " sim_list)
          ;(my_print '"   Similarity of synonymous predicates: " synon_weight)
          ;(my_print '"   Similarity of coordinates: " coord_weight)
          ;(my_print '"   Synonyms: " synonyms)
          ;(my_print '"   Coordinated kinds: " same_kinds)
          ;(my_print '"   Coordinated parts: " same_parts) 
         )
   ; for ECHO:
         ( (eq acme_or_echo 'echo)
           (my_print '"   Tolerance: " (- (/ excit_weight inhib_weight)))
           (my_print '"   Skepticism: " decay_amount)
           (my_print '"   Data excitation: " data_excit)
           (my_print '"   Simplicity impact: " simpl_impact)
           (my_print '"   Analogy impact: " analogy_impact)
         )
   )
)    

(defun pva () (print_values 'acme))
; ***************************************************************
; RECOMPILE

(defun recompile () 
    (compile-file '"run.l")
    (compile-file '"acme.l")
    (fasload '"run.2bin")
    (fasload '"acme.2bin")
    (my_print '"Recompiled")
)

; *******************************************************************
; DEBUG_UNITS gives info. about units at each timestep.

(defun debug_units (units)
   (defun debug_run () 
      (do ((unts units (cdr unts)))
          ((null unts) 'done)
          (my_print total_times '"  Activation of " (car unts) '" is " 
                 (get (car unts) 'activation)
          )
      )
   )
)

(defun debug_els (elements)
   (debug_units (union_map #'get_constraints elements))
)

(defun get_constraints (el) 
   (get el 'constraint_hyps)
)

; DBA DBM DBN

(defun dba () (defun debug_run () (best_analogy *struc1*)))
(defun dbm () (defun debug_run () (my_print total_times)))
(defun dbn () (defun debug_run () nil))

; ***************************************************

; DEBUG_ANA

(defvar debug_count 20)

(defun debug_ana ()
   (defun debug_run () 
       (if (< total_times debug_count)
           (best_analogy *struc1*)
          
           (my_print total_times)
       )
   )
)

; ****************************
; RA

(defun ra () (run_hyp_net 'acme))
;(defun re () (run_hyp_net 'echo))

; ******************************
; 

(defun count_links ()
   (do ((unts all_units (cdr unts))
        (result '(nil 0))
       )
       ((null unts) (my_print "Most-linked unit is " (car result)
                              " with " (second result) " links."
                    )
       )
       (if (> (length (get (car unts) 'linked_from))
              (second result)
           )
           (setq result (list (car unts)
                              (length (get (car unts) 'linked_from))
                        )
           )
       )   
   )
)

; NDF  new defaults

(defun ndf () (decay .1) (excit .1) (inhib -.2) (gross_on))
; *********************************************
;       GRAPHICS HELP:
; ********************************************

; GRAPH sets up the activation to grapher to graph designated units.

(defun graph (units)
   (if (eq *last-init* 'act)
       (reset-act units)
   )
)


; GRAPH_MAPS sets up the acme grapher to map all hypotheses
; about how the given elements are to be mapped.

(defun graph_maps (elements)
   (do ((els elements (cdr els))
        (result nil)
       )
       ((null els) 
        (if (eq *last-init* 'act)
            (and (my_print "Graphing: " result)
                 (reset-act result)
            )
        )
       )
       (setq result (union result (get (car els) 'constraint_hyps)))
   )
)

; ACME-MODE

(defun acme-mode () (setq *acme-mode* t))

; ECHO-MODE

(defun echo-mode () (setq *acme-mode* nil))

(defun make () (compile_thnet 'auto))


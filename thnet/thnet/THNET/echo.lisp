
; FILE:       net/echo.l
; PURPOSE:    explanatory coherence
; PROGRAMMER: Paul Thagard
; CREATED:     5-27-87
; UPDATED:     10-21-88

; System ECHO:  explanatory coherence by harmany (sic) optimization


(defun lec () (load "//tinman/ucla/psych/emelz/ARCS/echo.l"))


; *******************************************************************
;          Explanatory coherence as constraint satisfaction.
; *******************************************************************
; Principles:
;    If hypotheses Hi together explain data D,
;        make excitatory links between each of the Hi and D and
;        make excitatory links between each pair of the Hi.         
;        Weight is proportional to the number of Hi.
;    Make inhibitory links between each contradictory Hi and Hj.
;    Have excitatory links between higher hypotheses and hypotheses they
;        explain also.
;    Analogy also establishes excitatory links.
; ***************************************************************
;               Functions for parsing input.
; ***************************************************************
; PROPOSITION
(defun proposition (name sentence)
    (my_print name '" stands for:  " sentence)
    (put name 'propn sentence)
    (setq all_propositions (cons name all_propositions))
    (make_explan_unit name)
    (put name 'activation init_activ)
    (put name 'original_activation init_activ)
)

; ***************************************************************
; CONTRADICT 

(defun contradict (prop1 prop2)
    (my_print prop1 '" contradicts " prop2 '".")
    (make_sym_link prop1 prop2 inhib_weight)
    (put prop1 'contradicts (push prop2 (get prop1 'contradicts)))
    (put prop2 'contradicts (push prop1 (get prop2 'contradicts)))
    (setq contradictions (push (list prop1 prop2) contradictions))
)
; ***************************************************************
; DATA notes that members of a list of propositions are evidence
; nodes, each of which is then linked to the special evidence unit.
; If a member of the list is a list, with the form
; (proposition importance), then the link to the special unit
; has weight importance*data_excit.
; Note:  this should be called only after all the EXPLAIN 
; statements, so that the check for unexplained evidence will work.

(defun data (list) 
   (mapcar #'(lambda (unit_or_pair)
                 (cond ((listp unit_or_pair)
                        (setq all_data (cons (car unit_or_pair) all_data))
                        (make_sym_link 'special 
                                       (car unit_or_pair)
                                       (* data_excit (second unit_or_pair))
                        )
                       )
                      ; else:
                      (t (make_sym_link 'special unit_or_pair data_excit)
                         (setq all_data (cons unit_or_pair all_data))
                      )
                )
              )  
             list
   )
   (if check_unexplained (unexplained))  ; check for unexplained data
   (my_print '"Data are: " all_data)
)

; ADD_DATA: add data at a particular cycle.  To work, this has to be
; called at each cycle, e.g. by debug_run.

(defun add_data (cycle evidence)
    (if (= total_times cycle)
        (data evidence)
    )
)
; **************************************************************
; UNEXPLAINED check to see if there are any unexplained pieces of 
; evidence, increasing decay if there are.
; NOTE:  THIS IS BUGGY IF MORE THAN ONE DATA STATEMENT IS USED.

(defun unexplained ()
   (cond ((null all_explainers)
          (my_print '"ERROR:  Do the EXPLAIN statements before the DATA statements.")
         )
         (t (my_print '"Unexplained data:  "
                      (set-difference all_data all_explained)
            )
            (setq decay_register decay_amount)
            (decay (* decay_amount (/ (length all_data) 
                                      (length all_explained)
                                   )
                   )
            )
          )
   )
)

; ***************************************************************
; EXPLAIN sets up excitatory links noting what explains what.
; Weights depend on the number of explaining hypotheses.
; Value should be between 0 and 1.

(defun explain (list_of_explainers explanandum &optional (value 1))
   (let ((simpl_wt
           (cond ( (or (zerop simpl_impact)
                       (null (set-difference list_of_explainers all_data))
                   )
                   (* excit_weight value)
                 )
                 ; otherwise:
                 (t  (/ (* excit_weight value)
                        (expt (length (set-difference list_of_explainers all_data))
                              simpl_impact             
                        )
                     )
                 )
            )
        ))
        (my_print '"   " list_of_explainers '" explain " explanandum)
        (setq all_explainers (union list_of_explainers all_explainers))
        (setq all_explained (cons_if_new explanandum all_explained))
        (mapcar 'make_explan_unit (cons explanandum list_of_explainers))
        (put explanandum 'explainers 
             (union list_of_explainers
                    (get explanandum 'explainers)
             )
         )
         ; record what explains what
         (note_co-hyps list_of_explainers explanandum)
         ; excitatory links between explanandum and explainers:  
         (make_excit_links explanandum list_of_explainers simpl_wt)

         ; excitatory links among co-hypotheses, except data
         (make_all_excit_links (set-difference list_of_explainers all_data)
                               (* simpl_wt co-hyp-importance)
         )
   )
)

; ADD_EXPLAIN

(defun add_explain (cycle explainers explained)
   (if (= total_times cycle)
       (explain explainers explained)
   )
)


; *****************************************************************
; ENTAIL sets up a 1-way link.

(defun entail (unit1 unit2)
   (set_weight unit1 unit2 (* entail_impact excit_weight))
   (my_print '"Proposition " unit1 '" entails " unit2)
   (put unit1 'entails (push unit2 (get unit1 'entails)))
   (put unit2 'entailed-by (push unit1 (get unit2 'entailed-by)))
)
  
; ********************************************************************
; ANALOGOUS sets up links between 2 analogous hypotheses and 
; between 2 analogous pieces of evidence.

(defun analogous (hyps data)
    (cond ((and (member (car data) (get (car hyps) 'explains))
                (member (second data) (get (second hyps) 'explains))
           )
           ; then:
           (make_sym_link (car hyps) (second hyps)
                          (* excit_weight analogy_impact)
           )
           (make_sym_link (car data) (second data) 
                          (* excit_weight analogy_impact)
           )
           (my_print '"Explanation of " (car data) 
                     '" by " (car hyps)
                     '" is analogous to explanation of " (second data)
                     '" by " (second hyps)
           )
          )
          ; else:
          (t (my_print '"Error:  non-explanatory analogy."))
    )
)
; ********************************************************************
; DISANALOGOUS sets up an inhibitory link between 2 disanalogous hypotheses
; that explain 2 analogous pieces of evidence.

(defun disanalogous (hyps data)
    (cond ((and (member (car data) (get (car hyps) 'explains))
                (member (second data) (get (second hyps) 'explains))
           )
           ; then:
           (make_sym_link (car hyps) (second hyps)
                          (* inhib_weight analogy_impact)
           )
           (my_print '"Explanation of " (car data) 
                     '" by " (car hyps)
                     '" is disanalogous to explanation of " (second data)
                     '" by " (second hyps)
           )
          )
          ; else:
          (t (my_print '"Error:  non-explanatory disanalogy."))
    )
)

; *********************************************************************
; EXPLAIN_AWAY indicates that a piece of evidence is so thoroghly
; explained by an external hypothesis that it shouldn't figure
; in the current debate.

(defun explain_away (explainers explained)
    (my_print "Explaining away: " explained " by " explainers)
    (cut_links explained)
    (explain explainers explained)
)

; CUT_LINKS -- IMPORTANT NOTE: This has not been modified to use the
; new hash-table version of links, so don't be surprised if it breaks!

(defun cut_links (unit) 
   (my_print "Cutting links to: " unit)
   (do ((units (get unit 'explainers) (cdr units)))
       ((null units) 'done)
       ;
       (setf (aref link_array 
                   (get unit 'index)
                   (get (car units) 'index)
             )
             explained_away_weight
       )
       (setf (aref link_array 
                   (get (car units) 'index)
                   (get unit 'index) 
              )
             explained_away_weight
       )
   )
)

; *********************************************************************
; MAKE_EXPLAN_UNIT sets up a unit for use by network.  

(defun make_explan_unit (name)
  (unless (member name all_units) (note_unit name))
)

; ******************************************************************
; GET_EXPLAINERS

(defun get_explainers (datum)
   (get datum 'explainers)
)

; ******************************************************************
; NOTE_CO-HYPS notes the co-hypotheses and explananda of each hypothesis.

(defun note_co-hyps (explainers explained)
   (do ((hyps explainers (cdr hyps)))
       ; exit
       ((null hyps) 'done.)
       ; repeat:
       (put (car hyps) 'explains
            (cons explained (get (car hyps) 'explains))
       )
       (put (car hyps) 'co-hypotheses
            (union (get (car hyps) 'co-hypotheses)
                   (remove (car hyps) explainers)
            )
       )
       (put (car hyps) 'explanations
            (cons (list explainers explained)
                  (get (car hyps) 'explanations)
            )
       )
    )
)

; *********************************************************
; SELF_LINK links a unit to itself.

(defun self_link (unit) 
    (set_weight unit unit data_excit)
)

; **********************************************************
; RUN_EXP runs the network.

(defun run_exp ()
      (setq *acme-mode* nil)
      (note_explanations)
      ; help higher level explainers by creating links from them
      ; down to data
      (if help_hier?
          (help_hierarchy)
      )
      (run_hyp_net 'echo)
      (if (and check_unexplained
               (/= decay_amount decay_register)  ; because of unexplained
          )
          (decay decay_register)
      )
      (print_propns)
)

(defun re () (run_exp))

(defun pve () (print_values 'echo))


; **************************************************************
; HIER_ON 

(defun hier_on () 
    (cond ( help_hier?
            (my_print "Already helping hierarchical explanations.")
          )
          (t (setq help_hier? t)
             (my_print "Now helping hierarchical explanations.")
          )
     )
)

; HIER_OFF

(defun hier_off () 
    (cond ( (null help_hier?)
            (my_print "Already NOT helping hierarchical explanations.")
          )
          (t (setq help_hier? nil)
             (my_print "Now NOT helping hierarchical explanations.")
          )
    )
)

; ***************************************************************
; HELP_HIERARCHY is intended to help hypotheses high up in a 
; hierarchy by linking them directly with data.  If T explains
; H, and H explains E, then T gets linked to E.
; This function uses GET_HIER to collect a set of triples, consisting
; of (higher-hypothesis (all-co-hypotheses-used-in-expln-chain) data-explained)
; If T1 explains T2 which 
; explains T3 which explains E, where E is evidence, then T1 and
; T2 get links to E, attenuated by the number of hypotheses along
; the way.


(defun help_hierarchy ()
   (my_print "Helping hierarchical explanations.")
   (do ((triples (get_hier) (cdr triples))
        (triple nil)
       )
       ((null triples) (my_print "Hierarchical explanations helped."))
       ; repeat:
       (setq triple (car triples))
       ; (my_print "Helping " triple)
       (make_sym_link (car triple) (last_el triple)
                      (/ excit_weight
                         (expt (length (set-difference (second triple)
                                                       all_data
                                       )
                               )
                               simpl_impact             
                         )
                      )
        )
        (my_print '"Hierarchical link created between " 
                  (car triple) " and " (last_el triple)
        )
        (put (car triple) 'hier_explains
             (cons (last_el triple) (get (car triple) 'hier_explains))
        )
        (put (last_el triple) 'hier_explainers
             (cons (car triple) (get (last_el triple) 'hier_explainers))
        )
   )
)


; **********************************************************
; GET_HIER finds hierarchical relations, returning a set of
; triples.

(defun get_hier ()
   (do ((data all_data (cdr data))
        (result nil)
       )
       ((null data) result)
       ; Repeat:
       (setq result (union result 
                           (check_explanations (car data))
                    )
       )
    )
)

; **********************************************************
; CHECK_EXPLANATIONS looks at the hypotheses that explained a piece
; of data, recursing upwards for explanations of those hypotheses.

(defun check_explanations (datum)
   ; (my_print "Checking datum " datum)
   (do ((hyps (get datum 'explainers))
        (hyp nil)
        (result nil)
       )
       ((null hyps) result)
       ;
       (setq hyp (car hyps))
       ; (my_print "Checking out " hyp)
       (setq hyps (remove hyp 
                          (union hyps (get hyp 'explainers))
                  )
       )
       ; (my_print "Hyps are " hyps)
       (unless (member datum (get hyp 'explains))
               (push (list hyp (trace_explns hyp datum) datum)
                     result
               )
       )
   )
)

; *********************************************************
; TRACE_EXPLNS finds all the co-hypotheses that were used
; along a chain of explanations from a hypothesis to a datum.
; It traces out the path between H and E, and returns all the
; co-hypotheses, including the hypotheses that were used.
; Doing a depth-first search as in Winston & Horn.

(defun trace_explns (hyp datum)
   (do ((queue (list (list hyp)))
        (expansion nil)
       )
       ((equal datum (caar queue))
        (hypotheses_used (reverse (car queue)))
       )
       ; repeat:
       ; (my_print "QUEUE: " queue)
       (cond ((null queue) 
              (my_print "ERROR:  NO PATH FOUND")
              (return nil)
             )
       )
       (setq expansion (expand_exp (car queue)))
       (setq queue (cdr queue))
       (setq queue (append expansion queue))
    )
)

; **********************************************************
; EXPAND_EXP

(defun expand_exp (exp_list)
   (do ((lst (get (car exp_list) 'explains) (cdr lst))
        (result nil)
       )
       ((null lst) result)
       ;
       (unless (member (car lst) exp_list)
               (setq result 
                     (cons (cons (car lst) exp_list)
                           result
                     )
               )
        )
        ; (my_print "Expanding " result)
   )
)

; *********************************************************

; HYPOTHESES_USED returns a list of all the hypotheses used
; in a hierarchical chain of explanations.  E.g. if H1 explains H2
; which explains E, it will return the co-hypotheses of H1 and H2
; as well as those two hypotheses.  The last element in the 
; list should be a piece of evidence.

(defun hypotheses_used (expln_chain)
   (unless (member (last_el expln_chain)
                   all_data
           )
           (my_print "ERROR:  last member of chain is not data.")
   )
   (do ((hyps expln_chain (cdr hyps))
        (result (remove (last_el expln_chain) expln_chain))
       )
       ((null (cdr hyps)) result)
       ;
       (setq result
              (union result
                     (co_hyps_in_expln (car hyps) (second hyps))
              )
       )

   )
)


; **********************************************************
; LAST_EL

(defun last_el (lst)
   (car (last lst))
)
; **********************************************************
; CO_HYPS_IN_EXPLN says what hypotheses were used in the 
; explanation of P by Q.  It can handle cases where P plays a
; role in explaining Q by different other hypotheses.

(defun co_hyps_in_expln (unit1 unit2)
   (do ((explns (get unit1 'explanations) (cdr explns))
        (result nil)
       )
       ((null explns) result)
       ;
       (if (equal unit2 (last_el (car explns)))
           (setq result 
                 (union result 
                        (caar explns)
                 )
           )
       )
   )
)





; **********************************************************

; NOTE_EXPLANATIONS 
(defun note_explanations ()
   (put 'special 'activation special_activation)
   (if (> ad-hoc_factor 1) (mapcar #'punish_ad-hoc 
                                   (set-difference all_explainers
                                                   all_data
                                   )
                           )
   )
)

; **********************************************************
; PUNISH_AD-HOC weakens excitatory links to hypotheses that explain
; only one thing.

(defun punish_ad-hoc (hyp)
   (cond ( (= (length (get hyp 'explains)) 1)
           (my_print '"Hypothesis " hyp '"is ad hoc.")
           (do ((units (get hyp 'linked_from) (cdr units))
		(links (get hyp 'keys) (cdr links))
	       )
               ; exit:
               ((null units) 'done)
               ; repeat:  
               (setf (gethash (car links) link_table)
                     (/ (gethash (car links) link_table 0)
                        ad-hoc_factor
                     )
               )
           )
         )
   )
)

; **********************************************************
;                Assessing the results:
; **********************************************************

; BEST_HYP announces the hypothesis with the highest activation:

(defun best_hyp (explainers)
  (let ((winner (highest explainers 'activation)))
       (my_print '"The best hypothesis is " winner 
                 '".  Activation: " (get winner 'activation)
       )
       (my_print  '"   Co-hypotheses: " (get winner 'co-hypotheses))
       (my_print  '"   Explains: " (get winner 'explains))
       (my_print  '"   Competitors: " 
                  (union_map #'get_explainers (get winner 'explains))
       )
   )
)
       
; ******************************************************************
; PRINT_COHERE prints out the current degree of coherence.

(defun print_cohere ()
   (my_print '"Explanatory coherence of system is: "
             (goodness all_units)
   )
)

; ******************************************************************
; PRINT_PROPNS displays the activation of each explainer or explanandum

(defun print_propns ()
    (my_print '"Total times: " total_times)
    (mapcar #'print_activation (sort all_propositions #'string<))
)

; **********************************************************************
; SURVIVORS prints out those propositions above a minimum level of activation.

(defun survivors (min_activ)
    (do ((propns all_propositions (cdr propns))
         (result nil)
        )
        ; exit:
        ((null propns) 
         (my_print '"Propositions with activation above " min_activ)
         (mapcar #'print_activation (sort result #'string<))
        )
        ; repeat
        (if (> (get (car propns) 'activation) min_activ)
            (setq result (cons (car propns) result))
        )
    )
)   
; ************************************************************
;                   SIMPLICITY ASSESSMENT
; *************************************************************
; The SIMPLICITY of a unit is number of facts explained 
; divided by that number plus the number of co-hypotheses:
; ranges between 0 and 1 which is best.

(defun simplicity (unit)
   (if (null (get_explains unit)) 0
       ;else:
       (float (/ (length (get unit 'explains))
                 (+ (length (get unit 'explains))
                    (length (get unit 'co-hypotheses))
                 )
              )
       )             
   )
)
; *************************************************************
; SIMP does a simplicity analysis of a unit.

(defun simp (unit)
   (cond ((listp unit)
          (my_print "To analyse simplicity of a list, use SIMPL")
         )
         ; else:
         (t (my_print "Analysing simplicity of " unit)
            (my_print "  " unit " explains: " (get unit 'explains))
            (my_print "  " unit " contradicts: " (get unit 'contradicts))
            (my_print "  " unit " has co-hypotheses: " (get unit 'co-hypotheses))
            (my_print "  Simplicity of " unit " is " 
                      (simplicity unit)
            )
         )
   )
)
; *************************************************************
; SIMPL does a simplicity analysis of a list of units.

(defun simpl (list_of_units)
   (cond ((atom list_of_units)
          (my_print "To analyse simplicity of a unit, use SIMP")
         )
         ; else:
         (t (my_print "Analysing simplicity of " list_of_units)
            (my_print "  Size: " (length list_of_units))
            (my_print "  Explain: " 
                      (union_map #'get_explains list_of_units)
            )
            (my_print "  Mean simplicity: " (mean_simp list_of_units))
            (my_print "  Gross simplicity: " (gross_simp list_of_units))
            (my_print "  Degree of unification: " (unif list_of_units))
         )
    )
)
; *************************************************************
; GET_EXPLAINS

(defun get_explains (unit)
   (get unit 'explains)
)

; GET_CO-HYPS
(defun get_co-hyps (unit)
    (get unit 'co-hypotheses)
)
; *************************************************************

; MEAN_SIMP gives the mean simplicity of hypotheses in a list.

(defun mean_simp (lst)
   (mean (mapcar #'simplicity lst))
)

; MEAN gives the average value.

(defun mean (lst)
   (/ (apply #'+ lst)
      (length lst)
   )
)
; *************************************************************

; GROSS_SIMP  gives the overall ratio of facts explained to co-hypotheses.

(defun gross_simp (lst)
   (float (/ (length (union_map #'get_explains lst))
             (+ (length (union_map #'get_explains lst))
                (length (union_map #'get_co-hyps lst))
             )
          )
   )
)
; *************************************************************
; UNIF gives the degree of unication of a list, which is the ratio 
; of number of facts explained to number of hypotheses in the list.

(defun unif (lst)
  (float
   (/ (length (union_map #'get_explains lst))
      (+ (length (union_map #'get_explains lst))
         (length lst)
      )
   )
  )
)
; *************************************************************
; COMPARE does a simplicity comparison of two units or two lists of units.

(defun compare (s1 s2)
   (cond ((and (atom s1) (atom s2))
          (my_print "UNIT   EXPLAINS  CO-HYPOTHESES  SIMPLICITY         ACTIVATION")
          (print_simp s1)
          (print_simp s2)
         )
         ((and (listp s1) (listp s2))
          (my_print "First list is " s1)
          (my_print "Second list is " s2)
          (my_print "SIZE  EXPLAINS  CO-HYPS  MEAN-SIMP GROSS-SIMP  UNIFICATION")
          (print_simp s1)
          (print_simp s2)        
         )
        (t (my_print s1 " and " s2 " are not comparable."))
   )
)
; *************************************************************

; PRINT_SIMP

(defun print_simp (sexp)
   (if (atom sexp)
       (my_print " " sexp"        " (length (get_explains sexp))
                 "          " (length (get_co-hyps sexp))
                 "          " (simplicity sexp)
                 "       " (get sexp 'activation)
       )
       ; else list
       (my_print " " (length sexp)
                 "       " (length (union_map #'get_explains sexp))
                 "       " (length (union_map #'get_co-hyps sexp))
                 "     " (mean_simp sexp)
                 "  " (gross_simp sexp)
                 "  " (unif sexp)

       )
   )
)
; *************************************************************
; CONTRAS compares all pairs contradictory propositions.

(defun contras ()
   (mapcar #'compare_pair contradictions)
)

; COMPARE_PAIR

(defun compare_pair (pair) 
   (compare (first pair) (second pair))
)
; *************************************************************
;             FOR ASSESSING EXPLANATORY BREADTH:
; ************************************************************

; EXPL returns all the units explained by members of a list of units.

(defun expl (lst)
   (union_map #'get_explains lst)
)

; UNEXPL returns data units not explained by members of a list.

(defun unexpl (lst) 
   (set-difference all_data (expl lst))
)

; *************************************************************

(defun coe () (compile-file "echo.l") (fasload "echo.lbin"))

; END OF ECHO.L


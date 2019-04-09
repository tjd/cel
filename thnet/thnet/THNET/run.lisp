
; FILE:       run.l
; PURPOSE:    running a constraint satisfaction net
; PROGRAMMER: Paul Thagard
; CREATED:    5-18-87
; UPDATED:    3-9-88
; UPDATED:    7-26-88 - Changed clear net to make it only remove what REALLY
;                       wasn't needed.
; UPDATED:    9-28-88 - Got rid of the matrix and hash table clearing
;                       functions.

(defvar *time-cm* nil)

(defun lru () (load "//tinman/ucla/psych/emelz/ARCS/run.l"))

; *************************************************
; CLEAR_NET starts the beginning of a run by cleaning up the
; results of past runs.

(defun clear_net (&optional leave_structures)
   (unless silent_map? (my_print '"Clearing net."))
   (setq stop_run? nil)
   ; clear all property lists:
   (clear_props 'special)
   (clear_props 'pragmatic)
   (mapcar #'clear_props all_units)
   (unless silent_map? (my_print '"Units cleared.")) 
   
   (setq total_times 1)
   (setq total_links 0)
   (setq total_sym_links 0)   
   (setq all_units nil)
   (setq asymptoted_units nil)       ;
   (setq settled? nil)   

; for ACME:
   
; Remove constraint hypotheses from all objects, predicates, propositions,
; and structures, but leave the original data around. 
   (remprop_from_list all_objects 'constraint_hyps)
   (remprop_from_list all_preds 'constraint_hyps)
   (remprop_from_list all_propositions 'constraint_hyps)
   (remprop_from_list all_structures 'constraint_hyps)
; Remove strength property from all predicates (just to be safe).
   (remprop_from_list all_preds 'strength)
   (setq pragmatic_unit_made nil)     

   (setq object_units nil)
   (setq query_connections nil)
   (setq sim_list nil)

; for ARCS:

   (setq mapped_propositions nil)
   (remprop_from_list mapped_concepts 'constraint_hyps)
   (setq mapped_concepts nil)

; for ECHO:

; I don't know if these need to be revised to fit in with the new scheme
; of removing only what you ABSOLUTELY do not need.

   (mapcar #'clear_props all_explainers)
   (mapcar #'clear_props all_explained)
   (setq all_explainers nil)
   (setq all_explained nil)
   (setq contradictions nil)
   (setq all_data nil)
   (cond (leave_structures nil)
	 (t 
	  (mapcar #'clear_struc all_structures)
	  (setq all_propositions nil)
	 )
   )
   (unless silent_map? (my_print "Network cleared."))
)



; ***************************************************
;          Running the network:
; ****************************************************
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

(defun debug_run () nil)

; ****************************************************
; PRINT_RUN

(defun print_run (mode &optional verbose)
  (my_print "original print_run")
  (cond (silent_run?)
	(t 
	 ;; Update "serial" activations if in parallel mode
	 (if *staracme-loaded*                
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
  )
))
; ****************************************************
; GOODNESS is a measure of how well constraints are satisfied..
; Returns 1 if no units.  Need to divide by 2 since all
; links get counted twice.

(defun goodness (list_of_units)
  (do ((units list_of_units (cdr units))
       (value 0)
      )
      ;exit:
      ((null units) (if (null list_of_units) 1 (/ value 2)))
      ;repeat:
      (setq value 
	    (+ value 
	       (how_good_unit (car units) (get (car units) 'links_from))
            )
      )
  )
)


(defun goo (units) (/ (goodness units) (length units)))
; ****************************************************
; HOW_GOOD_UNIT calculates the goodness of a particular unit
; with respect to its associates.  List_pairs is now a 
; list of dotted pairs (unit . weight).

(defun how_good_unit (unit list_pairs)
   (do ((pairs list_pairs (cdr pairs))
        (value 0)
       )
       ((null pairs) value)
       (setq value (+ value 
                      (* (cdar pairs)  ; weight
                         (max output_threshold (get unit 'activation))
                         (max output_threshold (get (caar pairs) 'activation))
                      )
                    )
        )
   )
)
; Note:  what this does is up goodness if the activation of units
; with high weights between them is high.

(defun gu (unit) 
    (my_print '"Goodness of " unit '" is " 
              (how_good_unit unit (get unit 'links_from))
    )
)

(defun gum () (mapcar #'gu all_units))

; ****************************************************

; UPDATE_UNIT_ACTIVN updates the activation of a unit based on the
; links it has.

(defun update_unit_activn (unit)
  (let ((net_input_value (net_input unit)))
    (put unit 'new_activation
	 (min max_activation
	      (max min_activation
		   (+ (* (get unit 'activation) (- 1 decay_amount))
		      (if (> net_input_value 0)
			  (* net_input_value
			     (- max_activation (get unit 'activation))
			  )
		          ; else:
			  (* net_input_value
			     (- (get unit 'activation) min_activation)
			  )
		      )
		   )
	      )
         )
    )
  )
)

; ****************************************************
; NET_INPUT is the weighted sum of output from all input units.
; Note that links_from now contains weights, not names or indices.

(defun net_input (unit)
  (do ((links (get unit 'links_from) (cdr links))
       (result 0)
      )      
      ; exit:
      ((null links) result)
      ; repeat:
      (setq result (+ (* (cdar links)
			 (max output_threshold (get (caar links) 'activation))
                      )
		      result 
                   )
      )
  )
)

; *******************************************************************
; FIX_ACTIVATION records the new activation and notes if the unit
; has reached asymptote.

(defun fix_activation (unit)
  (cond ((and stop_settled?
	      (< (abs (- (get unit 'new_activation)
			 (get unit 'activation)
                      )
                 )
		 asymptote
              )
	      (>= total_times min_settle)
         )
	 (setq asymptoted_units (cons unit asymptoted_units))
	)
	(t (setq settled? nil))
  )
  (put unit 'activation (get unit 'new_activation))
)

; *******************************************************************
; ANNOUNCE_ASYMPTOTE informs the user of any units recently asymptoted.

(defun announce_asymptote (unit)
  (my_print "Unit " unit " reached asymptote at cycle " 
	    total_times " with activation "
	    (get unit 'new_activation) "."
  )
)

; *******************************************************************

; UPDATE_UNIT_ACTIVN_GROSS  updates the activation of a unit based on the
; links it has, using Grossberg's rule that treats excitation and
; inhibition separately.
; Uses global variables current_excit and current_inhib

(defun update_unit_activn_gross (unit)
  ; calculate excitation and inhibition.
  (if tversky?
      (excit_and_inhib_tversky unit)
      (excit_and_inhib unit)
  )
  (put unit 'new_activation
       (min max_activation
	    (max min_activation
		 (+ (* (get unit 'activation) (- 1 decay_amount))
		    (* (- max_activation (get unit 'activation)) current_excit)
		    (* (- (get unit 'activation) min_activation) current_inhib)
		 )
	    )
       )
  )
)

; *******************************************************************
; EXCIT_AND_INHIB is just like net_input, except that it keeps track
; of excitation and inhibition separately.  EXCIT_AND_INHIB_TVERSKY is
; identical except that units with negative activation can pull down
; their excitatory neighbors.

(defun excit_and_inhib (unit)
   (do ((excit 0) (inhib 0) (wt 0) (activn 0)
        (links (get unit 'links_from) (cdr links))
       )
       ; exit:
       ((null links)
        (setq current_excit excit)
        (setq current_inhib inhib)
       )
       ; repeat:
       (setq wt (cdar links))
       (setq activn (max output_threshold (get (caar links) 'activation)))
       (if (> wt 0) (setq excit (+ excit 
                                   (* wt activn)
                                )
                    )
                    ; else wt is inhibitory:
                    (setq inhib (+ inhib
                                   (* wt activn)
                                )
                    )
        )
   )
)

(defun excit_and_inhib_tversky (unit)
  (do ((excit 0) (inhib 0) (wt 0)
       (links (get unit 'links_from) (cdr links))
      )
      ; exit:
      ((null links)
       (setq current_excit excit)
       (setq current_inhib inhib)
      )
      ; repeat:
      (setq wt (caar links))
      (if (> wt 0) (setq excit (+ excit
				  (* wt (get (caar links) 'activation))
                               )
		   )
                   ; else wt is inhibitory:
	           (setq inhib (+ inhib
				  (* wt (max output_threshold
					     (get (caar links) 'activation)
					)
				  )
			      )
                   )
     )
  )
)

; end of run.l


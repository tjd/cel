
; FILE:       links.l
; PURPOSE:    setting up links for networks
; PROGRAMMER: Paul Thagard, Greg Nelson
; CREATED:    12-9-87
; UPDATED:    9-12-88 -- Changed from arrays to a hash table.
; UPDATED:    9-27-88 -- Eliminated the hash table, started using property
;                        lists to encode link information.

(defun lli () (load "//tinman/ucla/psych/emelz/ARCS/links.l"))

; **************************************************************
;             LINK CREATION
; **********************************************************
; MAKE_ALL_EXCIT_LINKS sets up excitatory links between every
; pair of a set of units.

(defun make_all_excit_links (list_of_units weight)
   (do ((units list_of_units (cdr units))
       )
       ; exit:
       ((null (cdr units)) total_links)
        ;(my_print '"Making excitatory links.  Total symmetric links: " 
        ;          (/ total_links 2))
       ; repeat:
       (make_excit_links (car units)
                         (cdr units)
                         weight
       )
   )
)

; **********************************************************
; MAKE_EXCIT_LINKS sets up excitatory links between a unit and
; each of a set of units.

(defun make_excit_links (unit other_units weight)
   (do ((units other_units (cdr units)))
      ((null units) (return 'done))
      ; repeat:
      (make_sym_link unit (car units) weight)
   )
)

; **********************************************************
; MAKE_INHIB_LINKS sets up inhibitory links among all pairs of
; a set of units.

(defun make_inhib_links (units weight)
   (do ( (unts (remove nil units) (cdr unts))
         (units_without_nils (remove nil units))
       )
       ; exit:
      ((null (cdr unts)) total_links)
       ; repeat:
      (make_inhib_links_for_unit (car unts)
                                 units_without_nils
                                 weight
      )
   )
)

; **********************************************************
; MAKE_INHIB_LINKS_FOR_UNIT does it for one unit.

(defun make_inhib_links_for_unit (unit units weight)
   (do ( (unts units (cdr unts)))
       ; exit:
       ( (null unts) total_links)
       ; action:
       (make_sym_link unit (car unts) weight)
    )
)
; **********************************************************
; MAKE_SYM_LINK sets up a symmetric link between units.
; Important to make no links from unit to itself, and don't clobber
; excitatory links with inhibitory ones.
; Excitatory links sum, but inhibitory don't.

(defun make_sym_link (unit1 unit2 weight)
  (cond (  (and (not (eq unit1 unit2))
                (>= (weight_of_link_between unit1 unit2) 0)
           )
           ; then:
           (make_link unit1 unit2 weight)
           (make_link unit2 unit1 weight)
         )
   )
)


; ***********************************************************
; MAKE_LINK sets up a 1-way link.  It adds the weight to whatever
; weight (initially 0) was on the link.

(defun make_link (unit1 unit2 weight)
  (let (old)
    (cond ((setq old (assoc unit2 (get unit1 'links_from)))
	   (nsubstitute (cons unit2 (+ weight (cdr old))) old
			(get unit1 'links_from)
	   )
	  )
	  (t
	   (put unit1 'links_from
		(acons unit2 weight (get unit1 'links_from)))
	   (setq total_links (1+ total_links))
          )
    )
  )
)

; *********************************************************
; SET_WEIGHT allows one to set a weight on a link directly.
; This looks remarkably like make_link, with the change from
; "(+ old weight)" to "weight" in line 4

(defun set_weight (unit1 unit2 weight)
  (let (old)
    (cond ((setq old (assoc unit2 (get unit1 'links_from)))
	   (nsubstitute (cons unit2 weight) old
			(get unit1 'links_from)
	   )
	  )
	  (t
	   (put unit1 'links_from
		(acons unit2 weight (get unit1 'links_from)))
	   (my_print "Weight from " unit1 " to " unit2 " set to " weight ".")
	   (setq total_links (1+ total_links))
	  )
    )
  )
)

; ****************************************************
; WEIGHT_OF_LINK_BETWEEN 

(defun weight_of_link_between (unit1 unit2)
  (or (cdr (assoc unit2 (get unit1 'links_from))) 0)
)



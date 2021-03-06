; FILE:       staracme
; PURPOSE:    Conversion and running of constraint satisfaction network
;             on Connection Machine
; PROGRAMMER: Eric Melz
; CREATED:    5-28-88
; UPDATED:    9-2-89 

(in-package '*lisp)
(setf *staracme-loaded* T)

(*proclaim '(type single-float-pvar *unit-output-prob))
(*defvar *unit-output-prob (!! 1.0))

(*proclaim '(type single-float-pvar *unit-input-prob))
(*defvar *unit-input-prob (!! 1.0))

(defvar temp 0)
;(proclaim '(type single-float min_activation))
;(defvar min_activation -.99)        ; minimum activation of units
;(proclaim '(type single-float max_activation))
;(defvar max_activation .99)         ; maximum activation of units

;(proclaim '(type single-float output_threshold))
;(defvar output_threshold 0.0)       
;(proclaim '(type single-float asymptote))
;(defvar max_activation .001)


(*defvar *unit nil!!
         "This contains a unit in each processor ")
(*proclaim '(type single-float-pvar *unit-activation))
(*defvar *unit-activation (!! 0.0)
         "This contains the activation of the unit in the processor")
(*proclaim '(type single-float-pvar *new-unit-activation))
(*defvar *new-unit-activation (!! 0.0)
         "This contains the most recent activation of the unit in the processor")
(*proclaim '(type single-float-pvar *unit-net-input))
(*defvar *unit-net-input (!! 0.0)
         "This contains the net input to the unit")
(*proclaim '(type single-float-pvar *excit-net))
(*defvar *excit-net (!! 0.0)
         "This contains the net input for all the excitatory weights")
(*proclaim '(type single-float-pvar *inhib-net))
(*defvar *inhib-net (!! 0.0)
         "This contains the net input for all inhibitory weights")
(*proclaim '(type boolean-pvar *unit-p))
(*defvar *unit-p nil!!
         "This is true when a processor contains a unit")
(*proclaim '(type boolean-pvar *link-p))
(*defvar *link-p nil!!
         "This is true when a processor contains a unit")
(*proclaim '(type single-float-pvar *link-weight))
(*defvar *link-weight (!! 0.0)
         "This contains the weight of the link")
(*proclaim '(type (pvar (unsigned-byte cm:*cube-address-length*)) *link-from))
(*defvar *link-from (!! 0)
         "This contains the processor number of the 'from' end of the link")
(*proclaim '(type (pvar (unsigned-byte cm:*cube-address-length*)) *link-to))
(*defvar *link-to (!! 0)
         "This contains the processor number of the 'to' end of the link")
(*proclaim '(type boolean-pvar *asymptote-flag))
(*defvar *asymptote-flag nil!!
	 "A processor is T when it's unit's activation has asymptoted")
(*proclaim '(type boolean-pvar *old-asymptote-flag))
(*defvar *old-asymptote-flag nil!!
	 "A copy of last cycle's *asymptote-flag pvar")

;; This is mainly to try out the *defstruct feature.  from-act and to-act
;; are the activations on the from and to ends of the links.  Good is
;; (*!! from-act to-act *link-weight).
(*defstruct goodstruc 
   (from-act 0.0 :type single-float)
   (to-act   0.0 :type single-float)
   (good     0.0 :type single-float))

(*defvar *goodstruc (make-goodstruc!!)
	 "The pvar used for an instantiation of goodstruc")
; **************************************************************************
; MAKE_PARALLEL_NET
; This function converts a network to parallel format. 
;
; Connection machine processors are divided into two sets: (1) Units, and
; (2) links.
; Each processor which contains a link has the following information recorded
; in parallel pvars:
;    The unit, with its association list (obtained from the all_units list)
;    The unit's activation
;    The unit's new activation
;    A tag used to indicate whether or not a given processor contiains a unit
;    (This is used to select the set of processors which contain units)
;
; The processors which contain links contain the following information:
;    The link's weight
;    The processor # of the unit attatched to the "to" end of the link
;    The processor # of the unit attatched to the "from" end of the link
;    A tag, used to select the set of processors which contain links

(defun make_parallel_net ()
  (*all
    (my_print "Creating parallel network")
    (let ((next-processor 0)
	  (weight 0)
	  (unit nil)
	  (unit_list all_units))
      (setf unit_list (cons 'special unit_list))
      (if (get 'pragmatic 'activation)
	  (setf unit_list (cons 'pragmatic unit_list)))
;    (*cold-boot :initial-dimensions '(128 128))
      ;; Initialize pvars
      ;; Loop for all units, including special unit and pragmatic unit
      (dotimes (i (length unit_list))
	(setq unit (nth i unit_list))
	;; put the unit in the processor corresponding to its index
;	(*setf (pref *unit i) unit)   ; This line may be commented out to memory problems
;(my_print i " " unit)
	;; put the activation of the unit in a parallel pvar
	(*setf (pref *unit-activation i) (get unit 'activation))
	(*setf (pref *new-unit-activation i) (get unit 'activation))
        ;; same with the input and output probabilities
        (*setf (pref *unit-output-prob i) 
	       (if (get unit 'output_prob) 
		   (get unit 'output_prob)
		 1.0))
	(*setf (pref *unit-input-prob i)
	       (if (get unit 'input_prob)
		   (get unit 'input_prob)
		 1.0))
	;; tag this processor to indicate that it contains a unit
	(*setf (pref *unit-p i) T)
	(when (= 0 (mod i 1000))
	  (my_print "Using processor #" i)))
      ;; create links and put them in the remaining processors
      ;; start at the processor after the last unit index
      (setq next-processor (length unit_list))
      (dolist (unit unit_list nil)
	(dolist (connected-from (get unit 'links_from) nil)
	  (setq weight (cdr connected-from))
	  ;; put this link's weight into a processor
	  (*setf (pref *link-weight next-processor) weight)
	  ;; put this link's "to" end into the the same processor
	  (*setf (pref *link-to next-processor) (position unit unit_list))
	  ;; put thie link's "from" end into the processor
	  (*setf (pref *link-from next-processor) (position (car connected-from) unit_list))
	  ;; tag this processor to indicate that it contains a link
	  (*setf (pref *link-p next-processor) T)
	  ;; move to the next available processor
	  (incf next-processor)
          ;; print message every 1000th processor
	  (when (= 0 (mod next-processor 1000))
	    (my_print "Using processor #" next-processor))
	  ) ) ) ) )



; **************************************************************************
; UPDATE_UNIT_ACTIVN!! 
; This function "simultaneously" updates the activations for all units
; in the network, using the Rumelhart & McClelland updating rule.

(defun update_unit_activn!! ()
  (*all
;    (my_print "Updating R&M network.")
    ;; Insure that the special unit does not decay
    (*setf (pref *unit-activation 0) max_activation)
    (*when *link-p
	   ;; get all unit activations on from end of link
	   (*let ((*linked-from-activation (pref!! *unit-activation *link-from)))
		 ;; compute net function and send to processors on "to" end
		 (*pset :add (*!! *link-weight *linked-from-activation)
			*unit-net-input
			*link-to)
		 ) )
    ;; update unit activations using McClelland and Rumelhart Rule
    (*when (and!! (>!! (self-address!!) (!! 0))
		  *unit-p)
	   (*set *new-unit-activation
		 (min!! (!! max_activation)
			(max!! (!! min_activation)
			       (+!! (*!! *unit-activation (!! (- 1 decay_amount)))
				    (if!! (>!! *unit-net-input (!! 0.0))
					  (*!! *unit-net-input (-!! (!! max_activation) *unit-activation))
					;else
					  (*!! *unit-net-input (-!! *unit-activation (!! min_activation))))
		      ) ) )))))


; ***********************************************************************
; UPDATE_UNIT_ACTIVN_GROSS!!
; This functions "simultaneously" updates the unit activations in the net,
; using Grossberg's update function.

; ***********************************************************************
; UPDATE_UNIT_ACTIVN_GROSS!!
; This functions "simultaneously" updates the unit activations in the net,
; using Grossberg's update function.

(defun update_unit_activn_gross!! ()
  (*all
;    (my_print "Updating Gross net.")
    ;; Insure that the special unit does not decay
    (*set *excit-net (!! 0))
    (*set *inhib-net (!! 0))
    (*setf (pref *unit-activation 0) max_activation)
    (*when *link-p
	   ;; get all unit activations on from end of link
	   (*let ((*linked-from-activation 
		    (max!! (!! output_threshold)
			   (pref!! *unit-activation *link-from :collision-mode :many-collisions)))
		  (*linked-from-output-prob 
		       (pref!! *unit-output-prob *link-from))
		  (*linked-from-output (!! 0.0)))
		 (*set *linked-from-output 
		       (if!! (>!! (*!! *linked-from-output-prob (!! 100.0))
				  (random!! 100))
			     *linked-from-activation
			     (!! 0.0)))

		 ;; compute Grossberg's net function, which treats excitatory and
		 ;; inhibitory links diffently, and send to processors on "to" end
		 ;; calculate net functions seperately for inhibitory and excitatory weights
		 (*when (>!! *link-weight (!! 0))
			(*pset :add (*!! *link-weight *linked-from-output)
			       *excit-net
			       *link-to))
		 (*when (<=!! *link-weight (!! 0))
			(*pset :add (*!! *link-weight *linked-from-output)
			       *inhib-net
			       *link-to))
		 ) )
    ;; update unit activations using Grossberg's rule
    (*when (and!! (and!! *unit-p 
			 (>!! (*!! *unit-input-prob (!! 100.0))
			      (random!! 100)))
                  (>!! (self-address!!) 
		       (!! (if (get 'pragmatic 'activation) 
				       1 0))))
            (*set *new-unit-activation
               (min!! (!! max_activation)
			(max!! (!! min_activation)
			       (+!! (*!! *unit-activation (!! (- 1 decay_amount)))
				    (*!! *excit-net (-!! (!! max_activation) *unit-activation))
				    (*!! *inhib-net (-!! *unit-activation (!! min_activation))))))))))


;; old update unit_activn_gross function (before output-prob implemented).

;; 
;; 
;; (defun update_unit_activn_gross!! ()
;;   (*all
;; ;    (my_print "Updating Gross net.")
;;     ;; Insure that the special unit does not decay
;;     (*set *excit-net (!! 0))
;;     (*set *inhib-net (!! 0))
;;     (*setf (pref *unit-activation 0) max_activation)
;;     (*when *link-p
;; 	   ;; get all unit activations on from end of link
;; 	   (*let ((*linked-from-activation 
;; 		    (max!! (!! output_threshold)
;; 			   (pref!! *unit-activation *link-from :collision-mode :many-collisions))))
;; 
;; 		 ;; compute Grossberg's net function, which treats excitatory and
;; 		 ;; inhibitory links diffently, and send to processors on "to" end
;; 		 ;; calculate net functions seperately for inhibitory and excitatory weights
;; 		 (*when (>!! *link-weight (!! 0))
;; 			(*pset :add (*!! *link-weight *linked-from-activation)
;; 			       *excit-net
;; 			       *link-to))
;; 		 (*when (<=!! *link-weight (!! 0))
;; 			(*pset :add (*!! *link-weight *linked-from-activation)
;; 			       *inhib-net
;; 			       *link-to))
;; 		 ) )
;;     ;; update unit activations using Grossberg's rule
;;     (*when (and!! *unit-p (>!! (self-address!!) 
;; 			       (!! (if (get 'pragmatic 'activation) 
;; 				       1 0))))
;; 	   (*set *new-unit-activation
;; 		 (min!! (!! max_activation)
;; 			(max!! (!! min_activation)
;; 			       (+!! (*!! *unit-activation (!! (- 1 decay_amount)))
;; 				    (*!! *excit-net (-!! (!! max_activation) *unit-activation))
;; 				    (*!! *inhib-net (-!! *unit-activation (!! min_activation))))))))))
;; 
;; 
;; 


; **********************************************************************
; FIX_PARALLEL records the new activations, updated by UPDATE_UNIT_ACTIVN!!
; this function all checks if the net has settled.

(defun fix_parallel ()
  (*all
;  (my_print "Recording new activations for parallel network.")
     ;; Check asymptotes for all units except special and pragmatic
     (let ((temp -1))
;       (declare (type (pvar (unsigned-byte cm:*cube-address-length*)) temp))
;       (declare (type single-float asymptote))
	
       (setf temp (if (get 'pragmatic 'activation) 
		      1 0))
       (*when (and!! *unit-p (>!! (self-address!!) 
				  (!! temp)))
	      ;; store the old *asymptote-flag for comparison with new
	      (*setf *old-asymptote-flag *asymptote-flag)
	      (*set *asymptote-flag
		    (<!! (abs!! (-!! *unit-activation *new-unit-activation))
			 (!! asymptote)))
	      (if (and (*and *asymptote-flag)
		       (>= total_times min_settle))
		  (setf settled? t)
		  (setf settled? NIL))
	      ;; detect newly asymptoted units
;	    (get_new_asymp!!)
	      ;; this is temporary just to test the get_good function 8/2/89
;(my_print "G : " (get_good!!))
	      (*set *unit-activation *new-unit-activation)))))


; *********************************************************************
; GET_PARALLEL_ACT updates the "serial units" by retrieving the activations
; from the "parallel units"

(defun get_parallel_act NIL
  (*all
    (let ((temp -1))
;      (declare (type (pvar (unsigned-byte cm:*cube-address-length*)) temp))
      (if (get 'pragmatic 'activation) 
	  1 0)
      (*when (and!! *unit-p (>!! (self-address!!) 
				 (!! temp)))
	     (let ((unit_list all_units))
	       ;; collect the units that have reached asymptote
	       (setf asymptoted_units nil)
	       (setf unit_list (cons 'special unit_list))
	       (if (get 'pragmatic 'activation)
		   (setf unit_list (cons 'pragmatic unit_list)))
	       (do-for-selected-processors (i)
		 (when (pref *asymptote-flag i)
		   (setf asymptoted_units
			 (cons (nth i unit_list) asymptoted_units)))
		 (setf (get (nth i unit_list) 'activation)
		       (pref *unit-activation i))))))))



; ********************************************************************
; GET_GOOD!! returns the goodness of the network.  This is calculated
; in a parallel manner, using the *goodstruc parallel structure

(defun get_good!! nil
  (*all
   (*let* ((*return-pvar (!! 0.0)))
;      (declare (type (pvar single-float) *return-pvar))
      (*when *link-p
	 (*setf (goodstruc-from-act!! *goodstruc)
	       (pref!! *unit-activation *link-from :collision-mode :many-collisions))
	 (*setf (goodstruc-to-act!! *goodstruc)
	       (pref!! *unit-activation *link-to :collision-mode :many-collisions))
	 (*setf (goodstruc-good!! *goodstruc)
	       (*!! (goodstruc-from-act!! *goodstruc)
		    (*!! (goodstruc-to-act!! *goodstruc)
			 *link-weight)))
      (*pset :add (goodstruc-good!! *goodstruc) *return-pvar (!! 1))
      ;; the resut is in processor #1 of *return-pvar.  
      ;; Return half of this, since each link is counted twice
      (/ (pref *return-pvar 1) 2)))))


; ********************************************************************
; GET_NEW_ASYMP!! retrieves, in a parallel manner, the newly asymptoted
; units.

(defun get_new_asymp!! nil
  (*all
   (*let ((*just-asymptoted-flag 
	    (and!! (or!! *old-asymptote-flag *asymptote-flag)
		   (not!! *old-asymptote-flag))))
	 (*when *just-asymptoted-flag
	    (*do-for-selected-processors (i)
	       (announce-asymptote (pref *unit i)))))))



; *********************************************************************
; COMPARE_NETS prints out the activations of all units for the
; serial network (on left) and the parallel network (on right)
; ** Note: this function is obsolete.

(defun compare_nets ()
  (my_print "Unit #     serial activation   parallel activation")
  (dotimes (i (1+ (length all_units)))
     (my_print  i "           " (get (aref name_array i) 'activation)
                  "       " (pref *unit-activation i))
) )



; *********************************************************************
; This is an optimized version of pref!! to relieve some memory problems
; When memory gets tight. get-1L uses a lot of temporary storage.

(*defun my-pref!! (pvar-expr cube-addr-pvar &optional collision-mode)
  (print-cm-room)
  (*let (dest)
;     (declare (type (pvar (defined-float 23 8)) dest))
     (format t "pvar-expr: ~a  cube-addr-pr: ~a   dest: ~a"
	     (pvar-length pvar-expr)
	     (pvar-length cube-addr-pvar)
	     (pvar-length dest))
     (cm:get (pvar-location dest) (pvar-location pvar-expr) 
		(pvar-location cube-addr-pvar) (pvar-length dest))
    dest))


; *********************************************************************
; This function was nicked from Valeriy Nenov.
     
(defun print-cm-room (&optional message)
  (format t "~%~a~aStack: ~a  Heap: ~a  Free heap: ~a  Free: ~a"
	  (if message message "")
	  (if message " " "")
	  (- cmi::*next-available-stack-maddr* cmi::*start-of-vp-stack*)
	  (cmi::amount-of-cm-memory-in-heap)
	  (cmi::amount-of-free-cm-memory-in-heap)
	  (- cmi::*maximum-stack-maddr* cmi::*next-available-stack-maddr*)
	  ))
  
  

















































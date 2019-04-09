; FILE:       transfer
; PURPOSE:    analogical transfer of propositions
; PROGRAMMER: Eric Melz
; CREATED:    12/12/90






;; *************************************************************************
;; NEW transfer routine.  Handles cws and cwsg.

;; Check if elt (a 2 item list) is in list
(defun list-member (elt lst)
  (cond ((null lst) nil)
	((and (eq (car elt) (caar lst)) (eq (cadr elt) (cadar lst))) t)
	(t (list-member elt (cdr lst)))))

;; Create a new object
(defun make_new_obj nil
  (gensym "obj"))

;; Create a new predicate
(defun make_new_pred nil
  (gensym "pred"))


;; generate a new proposition label.  If there is a consistent proposition labeling
;; scheme, the "last proposition" may be specified (e.g. '(targ 19)).  The next
;; proposition that will be generated is targ20.
(defun generate-prop-label nil
  (if *current-prop*
    (progn
       (setf *current-prop* (list (car *current-prop*) (1+ (cadr *current-prop*))))
       (make-symbol (string-append (car *current-prop*) (cadr *current-prop*))))
    (gensym "prop")))

;; generate a new object label of the form (:MAP arg) where arg is the argument
;; the the object is being transfered from.
(defun generate-obj-label (arg)
  (make-symbol (string-append "(:MAP " arg ")")))

;; prop-mapped? checks to see if a prop item has at least one mapping 
;; unit created for it 
;; and maps above threshold, OR a "skolem" (mapped) item has already been 
;; created for this item.
(defun prop-mapped? (x)
  (let ((mapping_units (get x 'constraint_hyps)))
    (or (assoc x new_arg_list)
	(and mapping_units
	     (> (get (car (highest-l mapping_units 'activation)) 'activation) 
		*prop-threshold*)))))

;; elt-mapped? checks to see if a prop item has at least one mapping 
;; unit created for it 
;; and maps above threshold, OR a "skolem" (mapped) item has already been 
;; created for this item.
(defun elt-mapped? (x)
  (let ((mapping_units (get x 'constraint_hyps)))
    (or (assoc x new_arg_list)
	(and mapping_units
	     (> (get (car (highest-l mapping_units 'activation)) 'activation) 
		*elt-threshold*)))))

;; mate returns the mate of an item, or if the item has had a token created for its
;; mate, return the token
(defun mate (x)
  (let ((mapping_units (get x 'constraint_hyps)))
    ;; if any corresponding object has been created for this item, return it
    (if (assoc x new_arg_list)
	(cdr (assoc x new_arg_list))
	;; otherwise return the highest mapped item
	(if mapping_units
	    (let ((temp (set-difference (get (car (highest-l mapping_units 'activation)) 
					     'concerns) (list x))))
	      (if temp
		  (car temp)
		  x))
	    nil))))


;; predicate-transfer performs the appropriate transfer action for unmapped
(defun predicate-transfer (pred)
  (make-symbol (string-append "(:MAP " pred ")")))

;(defun predicate-transfer (pred)
;  pred)

;; prop? returns t if item is a prop
(defun prop? (x)
  (member x all_propositions))

(defun transfer nil
  (format t "~%~%Pattern Completion results:~%")
  ;; Iterate through the propositions.  If the propositions is contained in one
  ;; of the transfer_fields, attempt to transfer it.
  (setf transfered_list nil)     ;; list of propositions that have been transfered
  (setf new_arg_list nil)        ;; list of new objects/propositions that have been created
  (setf *current-prop* *last-prop*)
  (dolist (propn (reverse all_propositions))
    (if (list-member (car (get propn 'belongs_to)) *transfer-fields*)
	(transfer-prop propn (generate-prop-label)))))


;; Core of the copy with substitution and generation routine
(defun transfer-prop (prop label)
  ;; Only do transfer if it hasn't been done before
  (if (not (member prop transfered_list))
      (progn
	(setf transfered_list (cons prop transfered_list))
	(if (prop-mapped? prop)
	    (format t "~a~%" (get (mate prop) 'message))
	    ;; else normal transfer procedure
	    (let ((args (cadr (get prop 'message)))
		  (pred (car  (get prop 'message)))
		  (transfer_pred nil)
		  (transfer_args nil)
		  (child_label nil)
		  (obj_label nil)
		  (all_mapped? t))  ;areall the elements of the prop mapped?
	      ;; transfer predicate
	      (if (elt-mapped? pred)
		  (setf transfer_pred (mate pred))
		  (progn
		    (setf all_mapped? nil)
		    (setf transfer_pred (predicate-transfer pred))))
	      ;; transfer args
	      (dolist (arg args)
;		(format t "  transferring arg ~a~%" arg)
		;; if this argument has been mapped or a substitute has been created before, return it's mate
		(if (elt-mapped? arg)
		    (setf transfer_args (append transfer_args (list (mate arg))))
		    ;; otherwise, check if arg is a prop or an obj and do special transfer routine
		    (if (prop? arg)
			;; proposition transfer routine: recursively transfer children:
			;; create a new label for the mate, insert it in the
			;; appropriate lists, and  call transfer-prop.  If the child prop
                        ;; didn't transfer, successfully, don't transfer this prop with
			;; cws
			(progn
			  (setf child_label (generate-prop-label))
			  (if (not (transfer-prop arg child_label))
			      (setf all_mapped? nil))
			  (setf transfer_args (append transfer_args (list child_label)))
			  (setf new_arg_list (cons (cons arg child_label) new_arg_list)))
			;; argument is an object instead of a proposition.  
			;; Do similar stuff as for props, except
			;; transfer-prop isn't recursively called.
			(progn
			  (setf all_mapped? nil)
  			  (setf obj_label (generate-obj-label arg))
			  (setf transfer_args (append transfer_args (list obj_label)))
			  (setf new_arg_list (cons (cons arg obj_label) new_arg_list))))))
	      ;; end of transfer routine: print out constructed propostion
	      (if (or (eq *transfer-type* 'cwsg) 
		      (and (eq *transfer-type* 'cws) all_mapped?))
		  (progn
		    (format t "(~a (" pred)
		    (dolist (arg (reverse (cdr (reverse transfer_args))))
		      (format t "~a " arg))
		    (format t "~a) ~a)~%" (car (last transfer_args)) label)))
              ;; artificially "map" the transferer proposition to the created transferee prop
              (setf new_arg_list (cons (cons prop label) new_arg_list))
	      ;; return success status for cws: if all_mapped is t, the prop
	      ;; was successfully transfered.
	      all_mapped?)))))


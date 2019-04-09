;;; FILE: semantics.lsp

(defun get_existing_preds (pred) (exists_at_all (get_associated_preds pred)))

(defun has_existing_rels (pred)
  (if (remove pred (exists_at_all (get_associated_preds pred))) pred nil)
)

(defun preds (fable) (conc_from_struc fable))

(defun all_preds ()
  (setq *print-length* nil)
  (sort
   (remove-duplicates (mapcan #'conc_from_struc all_structures))
   #'string-lessp
  )
)

; Get all of the predicates which are explicitly defined in the semantics file.
(defun explicit_preds ()
  (remove-if-not '(lambda (el) (get el 'explicit)) all_concepts)
)

(defun missing_preds (&rest files-to-check)
  "Tells you all of the predicates in the named files that are not explicitly
defined in the semantics, excluding plurals."
  (do ((files files-to-check (cdr files)))
      ((null files) nil)
      (load (car files))
  )
  (semantics)
  (sort (remove-if '(lambda (el) (get el 'singular))
		   (set-difference (all_preds) (explicit_preds))
	)
	#'string-lessp
  )
)

; Get the text of all propositions with the given predicate
(defun propns_from_pred (pred) 
  (mapcar '(lambda (el) (get el 'message))
	  (get pred 'from_propns)
  )
)

; What I want to do is to keep track of the total number of different predi-
; cates used in a given file or files, and the total number of different
; predicates in each structure.  Then I want to compare the former with the
; total of the latter, to get some sort of measure of overlap.

(defun overlap_metric (struc_list)
  (do ((totalpreds 0)
       (eachpreds 0)
       (cur_preds nil nil)
       (all_preds nil)
       (rest_struc struc_list (cdr rest_struc))
      )
      ((null rest_struc) 
       (my_print "Overlap is " (/ (* (- eachpreds totalpreds) 100.0) totalpreds) " given " eachpreds " predicates in structures and " totalpreds " total predicates.")
      )
      (setq cur_preds (conc_from_struc (car rest_struc)))
      (setq all_preds (union all_preds cur_preds))
      (setq totalpreds (length all_preds))
      (setq eachpreds (+ eachpreds (length cur_preds)))
  )
)

(defun dont_want (predicate-list)
  (setq unwanted_preds (union predicate-list unwanted_preds))
)

(defun want (predicate-list)
  (do ((preds predicate-list (cdr preds)))
      ((null preds) unwanted_preds)
      (setq unwanted_preds (remove (car preds) unwanted_preds))
  )
)

(defun firstarg (propn) (car (args_from_propn propn)))

(defun secondarg (propn) (cadr (args_from_propn propn)))

(defun ispropname (argument) 
  (and argument (member argument all_propositions))
)

(defun causality_preceding (propn)
  (append (cond ((equal (pred_from_propn propn) 'conjoin-event)
		 (append (causality_preceding (firstarg propn))
			 (causality_preceding (secondarg propn))
		 )
		)
		((ispropname (car (args_from_propn propn)))
		 (causality_preceding (firstarg propn))
		)
	  )
	  (list (get propn 'message))
  )
)

(defun causality_chain (propn)
  (if (not (member propn all_propositions))
      (return-from causality_chain "No such proposition.")
  )
  (append
   (do ((prt
	 (if (ispropname (firstarg propn))
	     (causality_preceding (firstarg propn)) nil)
	 (cdr prt)
        )
	(res nil)
       )
       ((null prt) res)
       (print (car prt))
       (setq res (append res (list (car prt))))
   )
   (terpri) (list (print (get propn 'message))) (terpri)
   (do ((prt
	 (if (ispropname (secondarg propn))
	     (causality_after (secondarg propn)) nil
	 )
	 (cdr prt)
        )
	(res nil)
       )
       ((null prt) res)
       (print (car prt))
       (setq res (append res (list (car prt))))
   )
  )
)

(defun causality_after (propn)
  (append (list (get propn 'message))
	  (if (ispropname (secondarg propn))
	      (causality_after (secondarg propn)) nil
	  )
  )
)


(defun overlapping_preds (struc1 struc2) 
  "Shows which predicates in struc2 are semantically similar to predicates in
struc1, eliminating those which do not hit at all."
  (semantics)
  (do ((lis (get struc1 'propositions) (cdr lis))
       (assocpreds nil nil)
      )
      ((null lis) 'done)
      (setq assocpreds
	    (intersection (get_associated_preds (pred_from_propn (car lis)) (tv_from_propn (car lis)))
			  (conc_from_struc struc2)
	    )
      )
      (cond (assocpreds
	     (princ (pred_from_propn (car lis)))
	     (princ ": ")
	     (princ assocpreds)
	     (terpri)
	    )
      )
  )
)


(defun multi_overlap_preds (struc1 struclis)
  "Runs overlapping_preds from struc1 to each structure in struclis"
  (semantics) ; redundant, but it makes the output cleaner if it is done here
  (do ((lis struclis (cdr lis))
      )
      ((null lis) 'done)
      (princ "Overlap between ")
      (princ struc1)
      (princ " and ")
      (princ (car lis))
      (terpri)
      (overlapping_preds struc1 (car lis))
      (terpri))
)

(defun count_overlapping_preds (struc1 struc2) 
  "Counts the number of predicates in struc1 with semantically similar predicates in struc2 (numA) and the sum of the numbers of preds in struc2 which are semantically similar to each pred in struc1 (numB)"
  (semantics)
  (do ((lis (get struc1 'propositions) (cdr lis))
       (assocpreds nil nil)
       (numA 0 (if assocpreds (+ numA 1) numA))
       (numB 0 (if assocpreds (+ numB (length assocpreds)) numB))
      )
      ((null lis) (list numA numB))
      (setq assocpreds
	    (intersection (get_associated_preds (pred_from_propn (car lis)) (tv_from_propn (car lis)))
			  (conc_from_struc struc2)
	    )
      )
      (cond (assocpreds
	     (princ (pred_from_propn (car lis)))
	     (princ ": ")
	     (princ assocpreds)
	     (terpri)
	    )
      )
  )
)


; A set of grapher functions for dealing with semantics:
(defun semg (word &optional (mode 'all))
  (start-graph 'network)
  (cond ((eq mode 'all)
	 (setq *link-labels* 'sem-relation)
	 (setq *second-label* nil)
	 (setq *link-width* nil)
	 (run-mouse word 'all-semantics)
	)
	((eq mode 'syn-ant)
	 (setq *link-labels* nil)
	 (setq *second-label* nil)
	 (setq *link-width* 'syn-ant-wid)
	 (run-mouse word 'syn-ant)
	)
	((eq mode 'super-sub)
	 (setq *link-labels* nil)
	 (setq *second-label* nil)
	 (setq *link-width* 'super-sub-wid)
	 (run-mouse word 'super-sub)
	)
  )
)

(defun all-semantics (word)
  (declare (special word))
  (apply 'append
	 (mapcar '(lambda (part)
		    (get word part)
		    )
		 '(synonyms antonyms superordinates subordinates sub-parts part-of)
	 )
  )
)

(defun sem-relation (word1 word2)
  (cond ((member word2 (get word1 'synonyms)) "SYN")
	((member word2 (get word1 'antonyms)) "ANT")
	((member word2 (get word1 'superordinates)) "SUPER")
	((member word2 (get word1 'subordinates)) "SUB")
	((member word2 (get word1 'sub-parts)) "PART")
	((member word2 (get word1 'part-of)) "WHOLE")
  )
)

(defun syn-ant (word)
  (append (get word 'synonyms) (get word 'antonyms))
)

(defun syn-ant-wid (word1 word2)
  (if (member word2 (get word1 'synonyms)) 3 1)
)

(defun super-sub (word)
  (append (get word 'subordinates) (get word 'superordinates))
)

(defun super-sub-wid (word1 word2)
  (if (member word2 (get word1 'superordinates)) 3 1)
)

(defvar desired-parts '(superordinates subordinates synonyms antonyms part-of sub-parts plural tenses))

(defun extract-entry (word)
  "Prints out the semantics entry for WORD."
  (let ((oldcase *print-case*))
    (setq *print-case* :downcase)
    (clear_predicate_props word)
    (princ (format nil "(mcon '~a~%" word))
    (princ (format nil "      '(~%"))
    (do ((parts (order-entry (symbol-plist word)) (cdr parts)))
	((null parts) nil)
	(princ (format nil "        (~a ~a)~%" (caar parts) (cadar parts)))
    )
    (princ (format nil "       )~%"))
    (princ (format nil ")~%"))
    (setq *print-case oldcase)
  )
)

(defun order-entry (entry)
  (sort
    (remove-if-not '(lambda (part) (member (car part) desired-parts))
      (do ((input entry (cddr input))
	   (output nil)
	  )
	  ((null input) output)
	  (setq output (cons (list (car input) (cadr input)) output))
      )
    )
    '(lambda (first second)
       (> (length (member (car first) desired-parts))
	  (length (member (car second) desired-parts))
       )
     )
  )
)

(defun stat (lis)
    (let ((n (length lis)) avg std var sumofsquares)
      (setq avg (/ (apply #'+ lis) n))
      (setq sumofsquares
	    (do ((lst lis (cdr lst))
		 (diff nil)
		 (sum 0))
		((null lst) sum)
		(setq diff (- (car lst) avg))
		(setq sum (+ sum (* diff diff)))
	    ))
      (setq var (/ sumofsquares (- n 1)))
      (setq std (sqrt var))
      (princ (format nil "N = ~d  Mean = ~,2f  Variance = ~,2f  Standard Dev = ~,2f" n avg var std))
      (terpri)
    )
)

(defun struc_file_stat (&rest files-to-check)
  (do ((files files-to-check (cdr files)))
      ((null files) nil)
      (load (car files))
  )
  (let ()
    (my_print "Number of propositions per structure:")
    (stat
     (mapcar '(lambda (struc) (length (propns_from_struc struc)))
	     all_structures
     )
    )
  )
)


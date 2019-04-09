
;  FILE:        misc.l
;  PURPOSE:     Miscellaneous utility functions for THNET
;  PROGRAMMER:  Paul Thagard
;  CREATED:     5-12-88
;  UPDATED:     6-16-88 - Made defuns into macros in some places.
;  UPDATED:     7-22-88 - Addition of JUSTIFY, ROUNDOFF, and SWAP utilities,
;       modification to COERCE-STRING.
;  UPDATED:     9-6-88 - Addition of YES-P, NO-P, menu utilities.  Addition of
;       READ-IN-RANGE.


; **********************************************
;        LISP dialect conversions
; Note: many of these used to be defuns, which I made into macros to
; save whatever wee bit of time I could.  6/16/88 -- Greg Nelson
; *********************************************
; For conversion from Franz to Common:

(defmacro greaterp (num1 num2) `(> ,num1 ,num2))

(defmacro lessp (num1 num2) `(< ,num1 ,num2))

(defmacro plist (atm) `(symbol-plist ,atm))

(defmacro quotient (num1 num2) `(/ ,num1 ,num2))
 
(defun times (&rest arguments) (apply '* arguments))

(defmacro diff (num1 num2) `(- ,num1 ,num2))

(defun add (&rest arguments) (apply '+ arguments))

(defmacro add1 (num) `(1+ ,num))

; Because member otherwise uses eq
(defmacro memberlist (lst1 lst2) 
  `(member ,lst1 ,lst2 :test 'equal)
)

(defmacro subsetlist (lst1 lst2) `(subsetp ,lst1 ,lst2 :test 'equal))
 
; *********************************************************
; For conversion from old Michigan lisp to Common:

(defmacro put (atm prop val) `(setf (get ,atm ,prop) ,val))

; *********************************************************
; For general conversion:

(defmacro not_equal (atm1 atm2) `(not (equal ,atm1 ,atm2)))

(defmacro my_max (num1 num2) `(max ,num1 ,num2))

(defmacro subset (l1 l2) `(subsetp ,l1 ,l2))

(defmacro remove_duplicates (lst) `(remove-duplicates ,lst :test #'equal))

; MY_PRINT prints out any number of arguments.

(defvar where_to_print *standard-output*)   ; optional output stream

(defun my_print (&rest arguments)
   (prog (args)
      (setq args arguments)
      loop
      (cond ( (null args) (terpri where_to_print) (return t)))
      (princ (car args) where_to_print)
      (setq args (cdr args))
      (go loop)
   )
)

(defun justify (spaces atm)
  "Justifies atom ATM in a string of SPACES characters."
  (let (outstr)
    (setq outstr (coerce (princ-to-string atm) 'list))
    (do ((len (length outstr) (1+ len)))
	((eq len spaces) (coerce outstr 'string))
	(setq outstr (cons #\Space outstr))
    )
  )
)

(defun roundoff (places num)
  "Rounds NUM to PLACES decimal places."
  (cond ((eq places 0) (round num))
	(t (/ (round (* num (expt 10.0 places))) (expt 10.0 places)))
  )
)

(defun swap (lis pos1 pos2)
  "Exchanges elements POS1 and POS2 of list LIS."
  (do ((pos 0 (1+ pos))
       (elt1 (elt lis pos1))
       (elt2 (elt lis pos2))
       (retlis nil)
      )
      ((eq pos (length lis)) (reverse retlis))
      (setq retlis (cons (cond ((eq pos pos1) elt2)
			       ((eq pos pos2) elt1)
			       (t (elt lis pos))
			 )
			 retlis
		   )
      )
  )
)


; ***********************************************************
; For atom making (from Marie):

; CONCAT
(defun concat (&rest concat-things)
  "Take any number of strings, atoms or numbers, smash them together,
   (after making them all into strings) and return a symbol of the result."
   (read-from-string (apply 'string-append 
		           (mapcar 'coerce-string concat-things)))
)

(defun coerce-string (thing)
  "Make a thing (number, symbol, string) into a string."
  (princ-to-string thing)
)

;; BEGIN newsym functions (similar to gensym)
(defvar *NEWSYM-PREFIX* 'c)

(defun newsym (symb)
  "Given a symbol, get it's counter and create a new symbol consisting
   of the symbol concat'ed with its number.  If symbol is nil, use 
   the current value of *NEWSYM-PREFIX*"
   (cond ((symbolp symb)
          (if (null symb) (setq symb *NEWSYM-PREFIX*))
          (let (count)
               (if (null (get symb '*newsym-counter*))
                   (setf (get symb '*newsym-counter*) 0))
               (setf (get symb '*newsym-counter*)
                     (1+ (setq count (get symb '*newsym-counter*))))
               (concat symb count)))
         (t (princ "Error: non-symbol arg to newsym ")
            (princ symb))))

; **********************************************************
; ATOM_BEGINS checks to see whether an atom begins with a
; given character.

(defun atom_begins (atm char)
   (eq (aref (coerce-string atm) 0) char) 
)

; ATOM_ENDS checks to see whether an atom ends with a given 
; character.

(defun atom_ends (atm char)
  (let ((str (coerce-string atm)))
    (eq (aref str (1- (length str))) char)))

; ATOM_INCLUDES checks to see whether an atom includes a given
; character.

(defun atom_includes (atm char)
   (prog (str index)
      (setq str (symbol-name atm))
      (setq index 0)
      loop
      (if (> (+ 1 index) (length str)) (return nil))
      (if (equal (aref str index) char) (return t))
      (setq index (+ 1 index))
      (go loop)
   )
)
         
; ************************************************************
; For convenience:

(defun q () (quit))
(defun exit () (quit))
(defun unix (str) (run-program str))
(defun lv+ () (setq *load-verbose* t))
(defun lv- () (setq *load-verbose* nil))
(defun lv++ ()
  (setq *load-verbose* t)
  (setq *record-source-files* nil)
  (discard-source-file-info)
)


; ***************************************************************

; For more informative printing:

(setq *print-level* 20)
(setq *debug-print-level* 20)
(setq *print-length* 100)
(setq *debug-print-length* 100)

;*********************************************************
;       MISCELLANEOUS FUNCTIONS
;**********************************************************

; NOTE_UNIT sets the global variables indicating a unit has been added.

(defun note_unit (unit)
  (put unit 'activation default_activation)
  (setq all_units (cons unit all_units))
)

; **************************************************************       
; UNION_LIST takes any number of arguments and returns the
; union of all of them.

(defun union_list (&rest arguments)           ; takes any number of arguments
    (remove-duplicates (apply 'append arguments))
)
; ********************************************************
; UNION_MAP takes the union of all members of a list of lists,
; where the list of lists arises from mapcarring a function.
; e.g. union_map  'cdr  '( (a b) '( 1 2 a)) = (b 2 a)
(defun union_map (fn lst)
   (apply 'union_list (mapcar fn lst))
)
;**********************************************************
; INTERSECTION_LIST takes any number of arguments and returns 
; their intersection.

(defun intersection_list (&rest arguments)
   (prog (args result)
      (setq args arguments
            result nil
      )
      loop
      (cond ( (null args) (return result)))
      (setq result (intersection (car args) result))
      (setq args (cdr args))
      (go loop)
   )
)

; ***********************************************************
; REMOVE_LIST removes all members of list1 from list2

(defun remove_list (lst1 lst2)
    (prog (ls result)
       (setq ls lst1)
       (setq result lst2)
       loop
       (cond ( (null ls) (return result)))
       (setq result (remove (car ls) result))
       (setq ls (cdr ls))
       (go loop)
    )
)
 
; *********************************************************
;  
; HIGHEST (list property)  returns that member of the list
; which has the highest value on its property list of the
; given property.  HIGHEST_L does the same, but returns a list
; when there are ties.
 
(defun highest (list property)
   (declare (special property))
   (prog (lst values)
      (setq lst list)
      (setq values (mapcar '(lambda (el)
                               (get el property)
                            )
                            list
                    )
      )
      loop
      (cond ( (null lst) (return nil)))
      (cond ( (equal (get (car lst) property)
                     (apply 'max values)
               )
               (return (car lst))
            )
       )
       (setq lst (cdr lst))
       (go loop)
  )
)

(defun highest-l (lis prop)
  (declare (special prop))
  (do ((lst lis (cdr lst))
       (high nil)
       (values (mapcar '(lambda (el) (get el prop)) lis))
      )
      ((null lst) (if (listp high) high (car high))) 
      (if (equal (get (car lst) prop) (apply 'max values))
	  (setq high (cons (car lst) high))
      )
  )
)

; *************************************************************
; PRINT_PLIST_S prints out the plists of all members of a list

(defun print_plist_s (lst)
    (mapcar 'print_plist lst)
)
 
(defun pls (lst) (mapcar 'print_plist lst))

 

; **************************************************************************
; print_plist pretty-prints out a property list.

(defun print_plist (atm)
   (prog (lst)
       (my_print '"   ")
       (my_print '"Property list of "  atm)
       (setq lst (plist atm))
       loop
       (cond ( (null lst) (return t)))
       (my_print (car lst) '":  " (cadr lst))
       (setq lst (cddr lst))
       (go loop)
   )
)
(defun pl (atm) (print_plist atm))

;
;  ************************************************************************ 
; CONS_IF_NEW adds an element if it is not already there.

(defun cons_if_new (el lst)
   (if (member el lst :test #'equal) lst
       ; else
       (cons el lst)
   )
)
;  ************************************************************************ 
;  MIN_MAX returns a value between low and high.

(defun min_max (low high num) (min (max low num) high))

; ***************************************************

; CLEAR_PROPS clears all property lists

(defun clear_props (atm)
  (setf (symbol-plist atm) nil)
)

; CLEAR_PREDICATE_PROPS clears only the properties constraint_hyps, from_propns,
; and belongs_to.  This makes it possible to retain semantic information.  This
; may need to be made more comprehensive for the purposes of ACME.

(defun clear_predicate_props (atm)
  (setf (get atm 'constraint_hyps) nil)
  (setf (get atm 'from_propns) nil)
  (setf (get atm 'belongs_to) nil)
)

; *********************************************************************
; REMPROP_FROM_LIST takes a given property off of all members of a list.

(defun remprop_from_list (lis prop)
  (declare (special prop))
  (mapcar '(lambda (atm) (remprop atm prop)) lis)
)

; *********************************************************************
; GET_FROM_LIST gets the specified property of each element of the list
; and splashes them all together.

(defun get_from_list (lst prop)
  (declare (special prop))
  (cond ((null lst) nil)
	((equal (length lst) 1) (get (car lst) prop))
	(t (remove-duplicates (apply #'append (mapcar '(lambda (el) (get el prop)) lst))))
  )
)

(defun put_on_list_if_better (lst prop val)
  (declare (special prop val))
  (cond ((null lst) nil)
	((equal (length lst) 1)
	 (if (or (not (get (car lst) prop))
		 (> val (get (car lst) prop))
	     )
	     (put (car lst) prop val)
	 )
	 lst
	)
	(t 
	 (mapcar '(lambda (el) (if (or (not (get el prop))
				       (> val (get el prop))
				   )
				   (put el prop val)
			       )
		  )
		 lst
	 )
	 lst
	)
  )
)

; ****************************************************
; EVERY_SECOND gets the first, third, fifth, etc. members
; of a list.

(defun every_second (lst)
    (prog (ls result)
      (setq ls lst
            result nil
      )
      loop
      (cond ( (null ls) (return result)))
      (setq result (cons (car ls) result))
      (setq ls (cddr ls))
      (go loop)
   )
)

; NOT_MEMBER

(defun not_member (el lst)
  (cond ( (memberlist el lst) nil )
        ( t t)
  )
)

; REMOVE_NIL_DUP 

(defun remove_nil_dup (lst)
   (remove-duplicates (remove nil lst))
)

; FLATTEN is the typical flatten function.

(defun flatten (lis)
  "Removes nestings from a list."
  (cond ((atom lis) lis)
	((listp (car lis))
	 (append (flatten (car lis)) (flatten (cdr lis)))
	)
	(t (append (list (car lis)) (flatten (cdr lis))))
  )
)

; ******************************************************************
; For convenience in obtaining user input:
; YES-P determines if a response is equivalent to "YES", "Y", etc.
; NO-P does the opposite

(defun yes-p (resp)
  (let ((*temp* (string-upcase (princ-to-string resp))))
    (or (equal *temp* "YES")
	(equal *temp* "Y")
    )
  )
)

(defun no-p (resp)
  (let ((*temp* (string-upcase (princ-to-string resp))))
    (or (equal *temp* "NO")
	(equal *temp* "N")
    )
  )
)

(defun print_menu (&rest messages)
  (let (choice)
    (my_print "Please select one of these using the following numbers:")
    (do ((i 1 (1+ i))
	 (mlist messages (cdr mlist))
	)
	((null mlist) (my_print "0  None of the above.") (my_print ""))
	(my_print i "  " (car mlist))
    )
    (setq choice (read-in-range 0 (length messages)))
    (if (eq choice 0) nil choice)
  )
)

(defun exec_menu (select &rest funcs)
  (if (and (numberp select) (< 0 select) (<= select (length funcs)))
      (eval (elt funcs (1- select)))
      (princ (format nil "You must enter a number between 1 and ~a.~%" (length funcs)))
  )
)

(defun read-in-range (min max)
  (if (<= max min)
      (my_print "That's a pretty stupid range.")
      (let ((result (read)))
	(cond ((or (not (numberp result))
		   (< result min)
		   (> result max)
	       )
	       (my_print "Please enter a number in the range " min " to " max ".")
	       (read-in-range min max)
	      )
	      (t result)
	)
      )
  )
)

(defun read-query (query)
  (terpri)
  (princ query)
  (read)
)	    

; end misc.l




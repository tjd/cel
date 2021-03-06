; FILE: consensus.lisp
; PURPOSE: model coherence-based consensus formation
; PROGRAMMER: Paul Thagard
; CREATED: September 17, 1998

; PLAN
; Data structure creation: a person with elements and 
; inputs for ECHO, DECO, etc.
; Algorithm: Achieve consensus by:
; 1. Each group member reaches coherence.
; 2. Randomly, group members meet and exchange information.
; 3. Coherence is then recalculated.
; 4. Iterate until agreement reached.
; See below consensus2 for a different algorithm.

(defvar *consensus-times* 1000 "maximum number of consensus iterations") ; to consensus

(defvar *exchange-prob* .5 "likelihood of element and input exchange")
(defvar *meeting-prob* .5 "proportion of meetings")
(defvar *important-elements* nil "units to graph")
(defvar *meeting-count* 0 "number of meetings")
(defvar *statistics* 0 "consensus statistics")

; GRAPH
(defun graph (els) (setf *important-elements* els))

; MAKE-PERSON sets up the input information for each person.
; GROUP is the consensus group to which the person belongs.
; GIVEN-EL lists the favored information - data for ECHO, goals
; for DECO, observed for IMP. 
; INPUT is the functions from ECHO etc. for creating a network
; for the person. Other properties added later: ACCEPTED, REJECTED.

(defun make-person (name group given input)
(put name 'group group)
(put group 'members (push name (get group 'members)))
(put name 'given-el given)
; (put name 'other-el other)
(put name 'input input)
name
)

; PERSONS-LIKE creates a bunch of people like the given person
; This could alternatively use setf symbol-plist

(defun persons-like (old list-of-new)
(do ((persons list-of-new (cdr persons))
(result nil)
)
((null persons) result)
; repeat
(push 
(make-person (car persons) 
(get old 'group)
(get old 'given-el)
(get old 'input)
)
result
)
)
)

; N-PERSONS makes num number of persons like a given one
; numberes starting with start.

(defun N-persons (name num start)
(persons-like name (make-names name num start))
)

; CONSENSUS is the main loop, as described in the plan above.

(defun consensus (group)
(my-print "Searching for coherence among group " group)
(my-print "Members are " (get group 'members))
(setq *weight-of-all-constraints* (sum-constraints))
; evaluate coherence for everyone
(mapcar #'person-coh (get group 'members)) 
; Repeat coherence evaluation and information exchange.
(do ((persons (get group 'members) (cdr persons))
(count 0 (1+ count))
)
; exit:
((or (= count *consensus-times*)
(group-agreed? group) 
)
(my-print "Consensus process stopped at time " count)
)
; repeat

; exchange info among persons
(my-print "Exchanging information at " count)
(exchange-information (get group 'members)) 
; evaluate coherence for everyone
(mapcar #'person-coh (get group 'members)) 
)
)

; CLEAR-GROUP clears the results of previous simulations:

(defun clear-group (group)
(mapcar #'clear-props (get group 'members))
(clear-props group)
(setf *meeting-count* 0)
)

; GROUP-AGREED? checks whether there is consensus in a group
; Consensus exists if all members of a group have the same
; sets of accepted and rejected elements.

(defun group-agreed? (group)
(do ((persons (get group 'members) (cdr persons)))
((null persons) (my-print (length (get group 'members))
" "
group 
" have reached consensus after "
*meeting-count*
" meetings."
)
't
)
; repeat
(unless (agrees-with-group? (car persons) (cdr persons))
(return nil)
)
)
)

; AGREES-WITH-GROUP? checks whether a person agrees with
; other members of the group

(defun agrees-with-group? (person group)
(do ((persons group (cdr persons)))
((null persons) 't)
;repeat
(unless (agrees-with person (car persons))
(return nil)
)
)
)

; AGREES-WITH compares the accepted and rejected for two
; persons to see if they agree.

(defun agrees-with (p1 p2)
(and (set-equal (get p1 'accepted)
(get p2 'accepted)
)
(set-equal (get p1 'rejected)
(get p2 'rejected)
)
)
)
; SET-EQUAL

(defun set-equal (lst1 lst2)
(and (subsetp lst1 lst2) (subsetp lst2 lst1))
)

; PERSON-COH runs a coherence judgment for a person, recording
; what elements were accepted or rejected, using the connectionist
; algorithm.

(defun person-coh (person)
(my-print "Maximizing coherence for " person)
(clear-net)
(mapcar #'eval (get person 'input))
(data (get person 'given-el))
(mapcar #'make-explan-unit (get person 'given-el))
(reset-act *important-elements*)
(put 'special 'activation 1.0)
(setq *weight-of-all-constraints* (sum-constraints))
(setq *all-constraints* (list-constraints *all-units*))
(connect-solution)
(put person 'accepted (fourth *connect-solution*))
(put person 'rejected (fifth *connect-solution*))
)

; EXCHANGE-INFORMATION selects pairs of persons to
; exchange information, including elements and 
; function input, modeling people in conversation.
; It is probabilistic in that:
; the persons are selected randomly
; parameter *exchange-prob* determines the
; likelihood of element and input exchange
; parameter *meeting-prob* determines the proportion
; of possible exchanges that take place

(defun exchange-information (people)
(do ((meetings (randomize (make-meetings people))
(cdr meetings)
)
)
((null meetings) 'done)
; repeat
(if (< (random 1.0) *meeting-prob*)
(have-meeting (car meetings))
)
)
)
; MAKE-MEETINGS list all possible meetings

(defun make-meetings (people)
(do ((persons people (cdr persons))
(result nil)
)
((null persons) result)
; repeat
(setf result 
(union result
(make-meetings-for (car persons) (cdr persons))
)
)
)
)

; MAKE-MEETINGS-FOR makes meetings for a particular person

(defun make-meetings-for (person people)
(do ((persons people (cdr persons))
(result nil)
)
((null persons) result)
; repeat
(setf result 
(push (list person (car persons)) result)
)
)
)

; HAVE-MEETING chooses two persons to meet, then swaps
; their information stochastically. Cf. Holland's
; crossover operation.

(defun have-meeting (pair)
(setf *meeting-count* (1+ *meeting-count*))
(my-print "Meeting " *meeting-count* " between " pair)
(add-given-from (first pair) (second pair))
(add-given-from (second pair) (first pair))
(add-inputs-from (first pair) (second pair))
(add-inputs-from (second pair) (first pair))
)

 

; ADD-GIVEN-FROM adds an element as given to a person
; stochastically. Adds from p1 to p2.

(defun add-given-from (p1 p2)
(do ((els (get p1 'given-el) (cdr els)))
((null els) 'done)
; repeat transfer
(if (and (not-member (car els) (get p2 'given-el))
(< (random 1.0) *exchange-prob*)
)
(put p2 'given-el 
(push (car els) (get p2 'given-el))
)
)
)
)
; ADD-INPUTS-FROM adds inputs to ECHO etc. to a person
; stochastically. Adds from p1 to p2.

(defun add-inputs-from (p1 p2)
(do ((inputs (get p1 'input) (cdr inputs)))
((null inputs) 'done)
; repeat transfer
(if (and (not-member (car inputs) (get p2 'input))
(< (random 1.0) *exchange-prob*)
)
(put p2 'input
(push (car inputs) (get p2 'input))
)
)
)
)

;;;;;;; FOR RUNNING EXPERIMENTS ;;;;;;;;;

; CP sets consensus parameters

(defun cp (exchange)
(setf *exchange-prob* exchange)
)

; CTIMES

(defun ctimes (num) (setf *consensus-times* num))

 

;;;;;;; CONSENSUS ALGORITHM 2
; This is more realistic than the one above.
; It checks for agreement after each exchange.
; No global exchanges.
; 1. Calculuate coherence for all people.
; 2. Check for agreement. If agreed, then stop.
; 3. Randomly choose a meeting between two scientists.
; 4. Exchange information for them.
; 5. Recalculate coherence for them.
; 6. Repeat 2 - 6.

(defun consensus2 (group)
(my-print "Searching for coherence *2* among group " group)
(my-print "Members are " (get group 'members))
(setq *weight-of-all-constraints* (sum-constraints))
; evaluate coherence for everyone
(mapcar #'person-coh (get group 'members)) 
; Repeat information exchange and coherence evaluation. and information exchange.
(do ((people (get group 'members))
(count 0 (1+ count))
(meeters nil)
)
; exit:
((or (= count *consensus-times*)
(group-agreed? group) 
)
(my-print "Consensus process stopped at time " count)
)
; repeat 
; exchange info between persons
; (my-print "Possible meeting at time " count)
(setf meeters (make-meeters people))
(cond ((not (agrees-with (car meeters) (second meeters)))
(have-meeting meeters) ; meet
; evaluate coherence for each
(mapcar #'person-coh meeters)
)
)
)
)

; MAKE-MEETERS randomly picks two people to meet

(defun make-meeters (lst)
(let ((p1 (random-member lst))
(p2 (random-member lst))
) 
(if (not-equal p1 p2)
(list p1 p2)
;
(make-meeters lst)
)
)
)

; RANDOM-MEMBER picks a randomly chosen member from a list

(defun random-member (lst)
(cdr (assoc (random (length lst))
(assoc-nums lst)
)
)
)

; LECTURE-TO allows one person to communicate with 
; numerous others. Communication is one-many, one-way, and
; stochastic

(defun lecture-to (person group)
(do ((listeners (get group 'members) (cdr listeners)))
((null listeners) 
(my-print group " listened to " person)
)
; repeat
(add-given-from person (car listeners))
(add-inputs-from person (car listeners))
)
)

 

; COLLECT-STATISTICS has lists of form:
; (<number of people> exchange-prob meeting-count)

(defun collect-statistics (group)
(push (list (length (get group 'members))
*exchange-prob*
*meeting-count*
)
*statistics*
)
)

; MAKE-NAMES makes a list of num names numbered starting with start.

(defun make-names (seed num start)
(do ((n num (1- n))
(result nil) 
(index start (1+ index))
)
((= n 0) result)
(push (catname seed index) result)
)
)

; CONSENSUS3 is just like consensus2, except that it enables
; collection of information about the number of 
; of people who agree with the eventual winner.

(defun consensus3 (group winner)
(my-print "Searching for coherence *3* among group " group)
(my-print "Members are " (get group 'members))
(setq *weight-of-all-constraints* (sum-constraints))
; evaluate coherence for everyone
(mapcar #'person-coh (get group 'members)) 
; Repeat information exchange and coherence evaluation. and information exchange.
(do ((people (get group 'members))
(count 0 (1+ count))
(meeters nil)
(agreement-stats nil)
)
; exit:
((or (= count *consensus-times*)
(group-agreed? group) 
)
(my-print "Consensus process stopped at time " count)
(my-print "Agreement progress: " agreement-stats)
)
; repeat 
; exchange info between persons
; (my-print "Possible meeting at time " count)
(setf meeters (make-meeters people))
(cond ((not (agrees-with (car meeters) (second meeters)))
(have-meeting meeters) ; meet
; evaluate coherence for each
(mapcar #'person-coh meeters)
)
)
(if (div-by *meeting-count* 10) 
(push (list *meeting-count*
(agree-count winner group)
)
agreement-stats 
)
)
)
)

 

; DIV-BY returns t if one number is devisible by the
; other without remainder

(defun div-by (num1 num2)
(integerp (/ num1 num2))
)

; AGREE-COUNT determines what percentage of a group agree
; with a given person.

(defun agree-count (person group)
(do ((persons (get group 'members) (cdr persons))
(agreeers 0)
)
((null persons)
(roundoff 3 
(float (/ agreeers (length (get group 'members))))
)
)
; repeat
(if (agrees-with (car persons) person)
(setq agreeers (1+ agreeers))
)
)
)



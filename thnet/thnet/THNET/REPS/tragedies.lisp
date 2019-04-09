
; FILE:       data/arcs/plays
; PURPOSE:    test retrieval using plays
; PROGRAMMER: David Gochfeld
; CREATED:    a long time ago
; UPDATED:    7-21-88 - "UNKNOWN" added, 7/22/88 - desire-true, desire-false,
;      believe-true, believe-false, order-to-do, attempt-to-do
;             7-25-88 - desire-xxx, believe-xxx, order-to-do, attempt-to-do
;      removed, imbedded hypothetical truth values for argument propositions 
;      added.
; UPDATED:    7-28-88 - divided into two files, histories and tragedies in one,
;      comedies and problem plays in the other.

(defun ltrag () (load "//tinman/ucla/psych/emelz/ARCS/tragedies")) 
(defvar structures_loaded nil "Keep a list of all of the make_strucs done.")
(setq structures_loaded (append structures_loaded '(hamlet macbeth
romeo-and-juliet othello julius-caesar king-lear antony-and-cleopatra
cymbeline richard-ii henry-v)))

; HAMLET

; Well, let's give it a shot

; Note: desire is a two-place proposition, where the actor wants a
;   proposition.
;       Also, old-hamlet is king, then claudius is king.  Is there
;   some way to deal with time?  Or does it not matter?  I guess it
;   doesn't really matter.

(make_struc 'hamlet 'play
	    '(characters ((king (obj-old-hamlet) true hac-1)
			  (prince (obj-hamlet) true hac-2)
			  (queen (obj-gertrude) true hac-3)
			  (man (obj-claudius) true hac-4)
			  (man (obj-polonius) true hac-5)
			  (man (obj-laertes) true hac-6)
			  (woman (obj-ophelia) true hac-7)
			  (daughter (obj-ophelia obj-polonius) true hac-8)
			  (son (obj-laertes obj-polonius) true hac-9)
			  (siblings (obj-old-hamlet obj-claudius) true hac-10)
			  (son (obj-hamlet obj-old-hamlet) true hac-11)
			 )
	     )

	    '(plot ((kill (obj-claudius obj-old-hamlet) true ha-1)
		    (marry (obj-claudius obj-gertrude) true ha-2)
		    (conjoin-event (ha-1 ha-2) true ha-3)
		    (king (obj-claudius) true ha-4)
		    (cause (ha-3 ha-4) true ha-5)
		    (upset (obj-hamlet) true ha-6)
		    (cause (ha-3 ha-6) true ha-7)
		    (distress (obj-hamlet obj-claudius) true ha-8)
		    (dead (obj-hamlet) unknown ha-9)
		    (desire (obj-claudius (ha-9 true)) true ha-10)
		    (cause (ha-8 ha-10) true ha-11)
		    (curtain (obj-arras) true ha-12)
		    (behind (obj-polonius obj-arras) true ha-13)
		    (behind (obj-claudius obj-arras) false ha-14)
		    (believe (obj-hamlet (ha-14 true)) true ha-15)
		    (kill (obj-hamlet obj-polonius) true ha-16)
		    (cause (ha-6 ha-15) true ha-17)
		    (cause (ha-15 ha-16) true ha-18)
		    (upset (obj-ophelia) true ha-19)
		    (cause (ha-16 ha-19) true ha-20)
		    (kill (obj-ophelia obj-ophelia) true ha-21)
		    (cause (ha-19 ha-21) true ha-22)
		    (upset (obj-laertes) true ha-23)
		    (cause (ha-21 ha-23) true ha-24)
		    (desire (obj-laertes (ha-9 true)) true ha-25)
		    (cause (ha-23 ha-25) true ha-26)
		    (conjoin-event (ha-10 ha-25) true ha-27)
		    (duel (obj-hamlet obj-laertes) true ha-28)
		    (cause (ha-27 ha-28) true ha-29)
		    (kill (obj-hamlet obj-laertes) true ha-30)
		    (kill (obj-laertes obj-hamlet) true ha-31)
		    (conjoin-event (ha-30 ha-31) true ha-32)
		    (cause (ha-28 ha-32) true ha-33)
		    (kill (obj-claudius obj-gertrude) true ha-34)
		    (accident (ha-34) true ha-35)
		    (cause (ha-10 ha-34) true ha-36)
		    (kill (obj-hamlet obj-claudius) true ha-37)
		    (conjoin-event (ha-6 ha-10) true ha-38)
		    (conjoin-event (ha-38 ha-34) true ha-39)
		    (cause (ha-39 ha-37) true ha-40)
		   )
	     )
)

; Claudius is given as sibling to old-hamlet because I have not yet needed a
; brother predicate and I am avoiding using one until it is truly needed.

; ignore the fact that hamlet is conditionally dead dependent on claudius'
; desire in ha-16 and laertes' desire later, but that he is not actually
; killed until both desire propositions cause the duel, which causes the
; killing.  In other words, we have to ignore time for the moment.

; Note:  proposition ha-15.  Hamlet believes that proposition ha-14 is TRUE
; regardless of the fact that ha-14 is actually false.  In this version of
; believe, the actor believes the proposition is true, although the proposition
; may in fact be false.  The latter case is when someone makes a mistake,
; as Hamlet does here.  If some one believes a proposition is false, then
; this believe can be used as (believe-false (actor proposition) true), in 
; other words, the actor DOES NOT believe the proposition is true.

; Note: proposition ha-35.  (accident (proposition))


; Macbeth
; for what it's worth

(make_struc 'macbeth 'play
	    '(characters ((thane (obj-macbeth) true mac-1)
			  (king (obj-duncan) true mac-2)
			  (woman (obj-lady-macbeth) true mac-3)
			  (nobleman (obj-macduff) true mac-4)
			  (man (obj-banquo) true mac-5)
			  (witches (obj-witches) true mac-6)
			  (married (obj-macbeth obj-lady-macbeth) true mac-7)
			  (friends (obj-macbeth obj-banquo) true mac-8)
			  (friends (obj-duncan obj-macduff) true mac-9)
			  (family-of (obj-family obj-macduff) true mac-10)
			 )
	     )
	    '(plot ((prophecy (conc-prophecy) true ma-1)
		    (say (obj-witches conc-prophecy obj-macbeth) true ma-2)
		    (give (obj-duncan obj-title obj-macbeth) true ma-3)
		    (title (obj-title) true ma-4)
		    (say (obj-macbeth conc-prophecy obj-lady-macbeth) true ma-5)
		    (kill (obj-macbeth obj-duncan) true ma-6)
		    (persuade (obj-lady-macbeth obj-macbeth ma-6) true ma-7)
		    (cause (ma-5 ma-7) true ma-8)
		    (king (obj-macbeth) true ma-9)
		    (cause (ma-6 ma-9) true ma-10)
		    (worried (obj-macbeth) true ma-11)
		    (cause (ma-1 ma-11) true ma-12)
		    (kill (obj-macbeth obj-banquo) true ma-13)
		    (cause (ma-11 ma-13) true ma-14)
		    (threaten (obj-macduff obj-macbeth) true ma-15)
		    (cause (ma-6 ma-15) true ma-16)
		    (frightened (obj-macbeth) true ma-17)
		    (cause (ma-15 ma-17) true ma-18)
		    (kill (obj-macbeth obj-family) true ma-19)
		    (cause (ma-17 ma-19) true ma-20)
		    (revenge (conc-revenge) true ma-21)
		    (want (obj-macduff conc-revenge obj-macbeth) true ma-22)
		    (cause (ma-19 ma-22) true ma-23)
		    (kill (obj-macduff obj-macbeth) true ma-24)
		    (cause (ma-22 ma-24) true ma-25)
		    (conjoin-event (ma-3 ma-9) true ma-26)
		    (conjoin-event (ma-26 ma-24) true ma-27)
		    (cause (ma-27 ma-1) true ma-28)
		   )
	     )
)

; Concerns:
;  The cause propositions in ma-8 and ma-20 are questionable.  They could just as
;   easily be -- (cause (ma-1 ma-7) true ma-8) and (cause (ma-17 ma-19) true ma-20)
;  Proposition mac-10 introduces object family and defines a relationship to, in
;   this case, macduff.  If a generic family is to be introduced, either the relationship
;   can be nil, or a random family name can be assigned.
;  Proposition ma-7 -- persuade is a type of cause relation -- obj-lady-macbeth
;   persuades obj-macbeth to do proposition ma-7.  If a need arises to persuade
;   someone that something is true, perhaps we should use convince with a similar form.
;
;  This presentation of the play is logically circular, as is the case in life with
; self-fulfilling prophecies.  Most of the events of the plot are cause by the
; prophecy (proposition ma-1 and/or ma-2), and yet, the prophecy is caused to be true
; by propositions ma-3, ma-9 and ma-24, which fulfill the various parts of the 
; prophecy (with detail left out of course).  Hopefully this is not a problem.
;  Propositions ma-21 through ma-25  -- macduff wants revenge on macbeth in proposition
; ma-22, and this is what causes him to kill macbeth.  I was trying to use a desire
; proposition, as in Hamlet, but I think this is probably better.



; Romeo and Juliet
;
; Concern: 
;   I'm not quite sure how to represent the feud, show that R+J are victims
;  of it, and state how the feud is ended.  I use a hate proposition, in
;  both directions as hate can be unidirectional (love is the same way).
;  I used this hate as cause for the fighting, but it is not causally
;  linked in the current representation to the deaths of R+J. The deaths
;  of the couple do cause the end of the feud, which is represented by
;  two end propositions.  (end (rj-1) true) means that the condition stated
;  in rj-1 (which is hate) is ended, or no longer true.  Perhaps we should
;  use falsify or something like that.  These two end propositions are 
;  conjoined and caused by the conjoined deaths of R+J.
;   Proposition rj-9 states that proposition rj-7 (the marriage) is 
;  secret.  It might become necessary to alter this so that it is someone's
;  secret or that it is secret from somebody.
;   Proposition rj-15 states that Romeo is banished FROM Venice.

(make_struc 'romeo-and-juliet 'play
	    '(characters ((man (obj-romeo) true rjc-1)
			  (woman (obj-juliet) true rjc-2)
			  (man (obj-mercutio) true rjc-3)
			  (man (obj-tybalt) true rjc-4)
			  (man (obj-paris) true rjc-5)
			  (kin (obj-romeo obj-mercutio) true rjc-6)
			  (kin (obj-juliet obj-tybalt) true rjc-7)
			  (family (obj-montagues) true rjc-8)
			  (family (obj-capulets) true rjc-9)
			  (member-of (obj-romeo obj-montagues) true rjc-10)
			  (member-of (obj-mercutio obj-montagues) true rjc-11)
			  (member-of (obj-juliet obj-capulets) true rjc-12)
			  (member-of (obj-tybalt obj-capulets) true rjc-13)
			  (city (obj-venice) true rjc-14)
			 )
	     )
	    '(plot ((hate (obj-montagues obj-capulets) true rj-1)
		    (hate (obj-capulets obj-montagues) true rj-2)
		    (conjoin-event (rj-1 rj-2) true rj-3)
		    (love (obj-romeo obj-juliet) true rj-4)
		    (love (obj-juliet obj-romeo) true rj-5)
		    (conjoin-event (rj-4 rj-5) true rj-6)
		    (marry (obj-romeo obj-juliet) true rj-7)
		    (cause (rj-6 rj-7) true rj-8)
		    (secret (rj-7) true rj-9)
		    (cause (rj-3 rj-9) true rj-10)
		    (kill (obj-tybalt obj-mercutio) true rj-11)
		    (cause (rj-3 rj-11) true rj-12)
		    (kill (obj-romeo obj-tybalt) true rj-13)
		    (cause (rj-11 rj-13) true rj-14)
		    (banished (obj-romeo obj-venice) true rj-15)
		    (marry (obj-paris obj-juliet) unknown rj-16)
		    (desire (obj-paris (rj-16 true)) true rj-17)
		    (desire (obj-juliet (rj-17 false)) true rj-18)
		    (cause (rj-9 rj-17) true rj-19)
		    (dead (obj-juliet) false rj-20)
		    (pretend (obj-juliet rj-20) true rj-21)
		    (conjoin-event (rj-17 rj-18) true rj-22)
		    (cause (rj-22 rj-21) true rj-23)
		    (believe (obj-romeo (rj-20 true)) true rj-24)
		    (kill (obj-romeo obj-romeo) true rj-25)
		    (cause (rj-24 rj-25) true rj-26)
		    (kill (obj-juliet obj-juliet) true rj-27)
		    (cause (rj-25 rj-27) true rj-28)
		    (conjoin-event (rj-25 rj-27) true rj-29)
		    (end (rj-1) true rj-30)
		    (end (rj-2) true rj-31)
		    (cause (rj-29 rj-30) true rj-32)
		    (cause (rj-29 rj-31) true rj-33)
		   )
	     )
)

; Concern: I'm using the fact that the marriage is secret (rj-9) as the
;   cause for Paris' desire to marry Juliet (rj-17) in proposition rj-19.
;   This is not strictly correct but it seems the only way to causally link
;   the feud (mutual hatred in props rj-1, 2 and 3) with the deaths of
;   R+J.
; Note: the pretend predicate in rj-21 is similar to believe -- the actor
;  pretends that the proposition is true, independent of the actual truth
;  of the proposition.


; Othello
;
; Concerns:
;   The problem here is that Iago is telling lies the whole way through,
;  and people believe him, and that is the cause of everything in the
;  play.  Yet at the end it is revealed that Iago has been lying and the
;  truth comes out.  There must be some way to represent lying, belief,
;  and then the revelation of the falsehood of the lies and the change
;  in beliefs.  This is very hard -- it probably requires some sort of 
;  representation of time.

(make_struc 'othello 'play
	    '(characters ((commander (obj-othello) true otc-1)
			  (moor (obj-othello) true otc-2)
			  (ensign (obj-iago) true otc-3)
			  (lieutenant (obj-cassio) true otc-4)
			  (man (obj-roderigo) true otc-5)
			  (friends (obj-roderigo obj-iago) true otc-6)
;			  (senator (obj-brabantio) true otc-7)
			  (woman (obj-desdamona) true otc-8)
;			  (daughter (obj-desdamona obj-brabantio) true otc-9)
			  (married (obj-othello obj-desdamona) true otc-10)
			  (woman (obj-emilia) true otc-11)
			  (married (obj-iago obj-emilia) true otc-12)
			 )
	     )
	    '(plot ((revenge (conc-revenge) true ot-1)
		    (want (obj-iago conc-revenge obj-othello) true ot-2)
		    (love (obj-desdamona obj-cassio) false ot-3)
		    (lie (obj-iago ot-3 obj-othello) true ot-4)
		    (believe (obj-othello (ot-3 true)) true ot-5)
		    (kill (obj-iago obj-cassio) true ot-6)
		    (order (obj-othello (ot-6 true)) true ot-7)
		    (lieutenant (obj-iago) true ot-8)
		    (know (obj-roderigo ot-2) true ot-9)
		    (kill (obj-iago obj-roderigo) true ot-10)
		    (cause (ot-9 ot-10) true ot-11)
		    (kill (obj-othello obj-desdamona) true ot-12)
		    (say (obj-emilia ot-4 obj-othello) true ot-13)
		    (kill (obj-iago obj-emilia) true ot-14)
		    (wound (obj-othello obj-iago) true ot-15)
		    (kill (obj-othello obj-othello) true ot-16)
		    (cause (ot-2 ot-4) true ot-17)
		    (cause (ot-4 ot-5) true ot-18)
		    (cause (ot-5 ot-7) true ot-19)
		    (cause (ot-7 ot-6) true ot-20)
		    (cause (ot-6 ot-8) true ot-21)
		    (cause (ot-5 ot-12) true ot-22)
		    (cause (ot-13 ot-14) true ot-23)
		    (cause (ot-4 ot-15) true ot-24)
		    (conjoin-event (ot-5 ot-7) true ot-25)
		    (conjoin-event (ot-12 ot-25) true ot-26)
		    (cause (ot-26 ot-16) true ot-27)
		   )
              )
)

; note: this is a really shoddy representation and leaves out many intracacies
;  and subplots of the play.  To properly represent this play would be very
;  complex and would require, among other things, a representation of time and
;  a more complex representation of deception.  Hopefully the above will serve
;  our purpose.


; Julius Caesar
;

(make_struc 'julius-caesar 'play
	    '(characters ((dictator (obj-caesar) true jcc-1)
			  (senator (obj-antonius) true jcc-2)
			  (senator (obj-brutus) true jcc-3)
			  (senator (obj-cassius) true jcc-4)
			  (friends (obj-caesar obj-antonius) true jcc-5)
			  (city (obj-rome) true jcc-6)
			  (conjoin-object (obj-brutus obj-cassius obj-senate)
					  true jcc-7)
			  (follower (obj-titinius obj-cassius) true jcc-8)
			  (citizens (obj-citizens obj-rome) true jcc-9)
			  (ally (obj-octavius obj-antonius) true jcc-10)
			 )
	     )
	    '(plot ((worship (obj-citizens obj-caesar) true jc-1)
		    (dangerous (obj-caesar) true jc-2)
		    (cause (jc-1 jc-2) true jc-3)
		    (overthrow (obj-senate obj-caesar) true jc-4)
		    (desire (obj-cassius (jc-4 true)) true jc-5)
		    (cause (jc-2 jc-5) true jc-6)
		    (exaggerate (obj-cassius jc-2 obj-brutus) true jc-7)
		    (cause (jc-5 jc-7) true jc-8)
		    (desire (obj-brutus (jc-4 true)) true jc-9)
		    (cause (jc-7 jc-9) true jc-10)
		    (conjoin-event (jc-5 jc-10) true jc-11)
		    (kill (obj-senate obj-caesar) true jc-12)
		    (cause (jc-11 jc-12) true jc-13)
		    (afraid (obj-antonius) true jc-14)
		    (cause (jc-12 jc-14) true jc-15)
		    (friends (obj-antonius obj-senate) false jc-16)
		    (pretend (obj-antonius jc-16) true jc-17)
		    (cause (jc-14 jc-17) true jc-18)
		    (believe (obj-senate (jc-16 true)) true jc-19)
		    (cause (jc-17 jc-19) true jc-20)
		    (spare (obj-senate obj-antonius) true jc-21)
		    (cause (jc-19 jc-21) true jc-22)
		    (half (obj-some-people obj-citizens) true jc-23)
		    (half (obj-other-people obj-citizens) true jc-24)
		    (loyal (obj-some-people obj-antonius) true jc-25)
		    (loyal (obj-other-people obj-senate) true jc-26)
		    (war (obj-antonius obj-senate) true jc-27)
		    (battle (obj-brutus obj-octavius obj-battle1) true jc-28)
		    (win (obj-brutus obj-battle1) true jc-29)
		    (believe (obj-cassius (jc-29 false)) true jc-30)
		    (kill (obj-cassius obj-cassius) true jc-31)
		    (cause (jc-30 jc-31) true jc-32)
		    (kill (obj-titinius obj-titinius) true jc-33)
		    (cause (jc-31 jc-33) true jc-34)
		    (battle (obj-brutus obj-antonius obj-battle2) true jc-35)
		    (win (obj-antonius obj-battle2) true jc-36)
		    (kill (obj-brutus obj-brutus) true jc-37)
		    (cause (jc-36 jc-37) true jc-38)
		    (conjoin-event (jc-31 jc-33) true jc-39)
		    (cause (jc-39 jc-36) true jc-40)
		   )
             )
)

; concerns: propositions jc-28 and 29, and propositions jc-35 and 36
;   the battle proposition is declaring an object (battle1 and battle2)
;   between the two people mentioned, and then one of the people
;   wins the battle.  I use battle in the first place to distinguish
;   from fight in that it is really the armies of the two people that are
;   fighting and in that a battle is a part of a war.


; King Lear

(make_struc 'king-lear 'play
	    '(characters ((king (obj-lear) true klc-1)
			  (daughter (obj-goneril obj-lear) true klc-2)
			  (daughter (obj-regan obj-lear) true klc-3)
			  (daughter (obj-cordelia obj-lear) true klc-4)
			  (kingdom (obj-kingdom) true klc-5)
			  (earl (obj-gloucester) true klc-6)
			  (bastard (obj-edmund) true klc-7)
			  (son (obj-edmund obj-gloucester) true klc-8)
			  (son (obj-edgar obj-gloucester) true klc-9)
			  (duke (obj-cornwall) true klc-10)
			  (married (obj-cornwall obj-regan) true klc-11)
			  (servant (obj-servant obj-cornwall) true klc-12)
			  (army (obj-army obj-kingdom) true klc-13)
			 )
	     )
	    '(plot ((half (obj-one-half obj-kingdom) true kl-1)
		    (half (obj-other-half obj-kingdom) true kl-2)
		    (love (obj-goneril obj-lear) false kl-3)
		    (believe (obj-lear (kl-3 true)) true kl-4)
		    (love (obj-regan obj-lear) false kl-5)
		    (believe (obj-lear (kl-5 true)) true kl-6)
		    (love (obj-cordelia obj-lear) true kl-7)
		    (believe (obj-lear (kl-7 false)) true kl-8)
		    (banish (obj-lear obj-cordelia) true kl-9)
		    (cause (kl-8 kl-9) true kl-10)
		    (give (obj-lear obj-one-half obj-goneril) true kl-11)
		    (cause (kl-4 kl-11) true kl-12)
		    (give (obj-lear obj-other-half obj-regan) true kl-13)
		    (cause (kl-6 kl-13) true kl-14)
		    (senile (obj-lear) true kl-15)
		    (abuse (obj-goneril obj-lear) true kl-16)
		    (abuse (obj-regan obj-lear) true kl-17)
		    (homeless (obj-lear) true kl-18)
		    (love (obj-goneril obj-edmund) true kl-19)
		    (love (obj-regan obj-edmund) true kl-20)
		    (jealous (obj-goneril obj-regan) true kl-21)
		    (jealous (obj-regan obj-goneril) true kl-22)
		    (cure (obj-cordelia obj-lear) true kl-23)
		    (repentant (obj-lear) true kl-24)
		    (wound (obj-cornwall obj-gloucester) true kl-25)
		    (blind (obj-gloucester) true kl-26)
		    (cause (kl-25 kl-26) true kl-27)
		    (kill (obj-servant obj-cornwall) true kl-28)
		    (cause (kl-25 kl-28) true kl-29)
		    (kill (obj-regan obj-servant) true kl-30)
		    (cause (kl-28 kl-30) true kl-31)
		    (die (obj-gloucester) true kl-32)
		    (kill (obj-edgar obj-edmund) true kl-33)
		    (kill (obj-goneril obj-regan) true kl-34)
		    (kill (obj-goneril obj-goneril) true kl-35)
		    (kill (obj-army obj-cordelia) true kl-36)
		    (order (obj-edmund (kl-36 true)) true kl-37)
		    (die (obj-lear) true kl-38)
		   )
	     )
)

; This is 51 propositions yet is a rather sparse representation of the plot.
; It entirely ignores the subplot of Gloucester and his sons -- edmund's lying
; about edgar and forcing edgar to flee.  it also ignores cordelia's marriage
; to the king of France, france's army attacking england, and Albany's
; cognizance of and putting an end to the treachery of edmund, goneril, and
; regan.  finally, i have not indicated any reasons for all the killing at
; the end.  ugh.

; Antony and Cleopatra

(make_struc 'antony-and-cleopatra 'play
	    '(characters ((empire (obj-rome) true acc-1)
			  (triumvir (obj-antony) true acc-2)
			  (triumvir (obj-lepidus) true acc-3)
			  (triumvir (obj-octavius) true acc-4)
			  (country (obj-egypt) true acc-5)
			  (man (obj-eros) true acc-6)
			  (follower (obj-eros obj-antony) true acc-7)
			  (queen (obj-cleopatra) true acc-8)
			  (wanton (obj-cleopatra) true acc-9)
			  (man (obj-pompeius) true acc-10)
			  (woman (obj-octavia) true acc-11)
			  (siblings (obj-octavius obj-octavia) true acc-12)
			 )
	     )
	    '(plot ((quarrel (obj-antony obj-octavius) true ac-1)
		    (love (obj-antony obj-cleopatra) true ac-2)
		    (stay-in (obj-antony obj-egypt) true ac-3)
		    (cause (ac-2 ac-3) true ac-4)
		    (attack (obj-pompeius obj-rome) true ac-5)
		    (journey (obj-antony obj-rome) true ac-6)
		    (end (ac-1) true ac-7)
		    (marry (obj-antony obj-octavia) true ac-8)
		    (truce (obj-truce) true ac-9)
		    (agree-upon (obj-pompeius obj-truce obj-rome) true ac-10)
		    (violate (obj-octavius obj-truce) true ac-11)
		    (defeat (obj-octavius obj-pompeius) true ac-12)
		    (imprison (obj-octavius obj-lepidus) true ac-13)
		    (return-to (obj-antony obj-egypt) true ac-14)
		    (war (obj-antony obj-octavius) true ac-15)
		    (ally (obj-cleopatra obj-antony) true ac-16)
		    (rule (obj-cleopatra obj-egypt) true ac-17)
		    (battle (obj-antony obj-octavius) true ac-18)
		    (betray (obj-cleopatra obj-antony) true ac-19)
		    (defeat (obj-octavius obj-antony) true ac-20)
		    (rule (obj-octavius obj-rome) true ac-21)
		    (kill (obj-cleopatra obj-cleopatra) true ac-22)
		    (believe (obj-antony (ac-22 true)) true ac-23)
		    (order (obj-antony (ac-25 true)) true ac-24)
		    (kill (obj-eros obj-antony) false ac-25)
		    (cause (ac-23 ac-24) true ac-26)
		    (kill (obj-eros obj-eros) true ac-27)
		    (cause (ac-24 ac-27) true ac-28)
		    (stab (obj-antony obj-antony) true ac-29)
		    (dying (obj-antony) true ac-30)
		    (slowly (ac-30) true ac-31)
		    (alive (obj-cleopatra) true ac-32)
		    (learn (obj-antony ac-32) true ac-33)
		    (die (obj-antony) true ac-34)
		    (become-true (ac-22) true ac-35)
		   )
	     )
)

;Note: the last proposition states that Cleopatra finally does kill herself.


; Titus Andronicus
; gorysville

;(make_struc 'titus-andronicus 'play
;	    '(characters (())))


; Cymbeline

(make_struc 'cymbeline 'play
	    '(characters ((king (obj-cymbeline) true cyc-1)
			  (queen (obj-queen) true cyc-2)
			  (daughter (obj-imogen obj-cymbeline) true cyc-3)
			  (stepson (obj-cloten obj-cymbeline) true cyc-4)
			  (man (obj-posthumus) true cyc-5)
			  (gentle (obj-posthumus) true cyc-6)
			  (nation (obj-rome) true cyc-7)
			  (man (obj-iachimo) true cyc-8)
			  (braggart (obj-iachimo) true cyc-9)
			  (servant (obj-pisanio obj-posthumus) true cyc-10)
			  (nation (obj-england) true cyc-11)
			  (man (obj-belarius) true cyc-12)
			  (boy (obj-boy) true cyc-13)
			 )
	     )
	    '(plot ((marry (obj-imogen obj-cloten) false cy-1)
		    (desire (obj-cymbeline (cy-1 true)) true cy-2)
		    (marry (obj-imogen obj-posthumus) true cy-3)
		    (secret (cy-3) true cy-4)
		    (banish (obj-cymbeline obj-posthumus) true cy-5)
		    (ring (obj-ring) true cy-6)
		    (bracelet (obj-bracelet) true cy-7)
		    (loyal (obj-posthumus obj-imogen) true cy-8)
		    (vow (obj-posthumus cy-8) true cy-9)
		    (give (obj-imogen obj-ring obj-posthumus) true cy-10)
		    (cause (cy-9 cy-10) true cy-11)
		    (give (obj-posthumus obj-bracelet obj-imogen) true cy-12)
		    (cause (cy-9 cy-12) true cy-13)
		    (journey (obj-posthumus obj-rome) true cy-14)
		    (seduce (obj-iachimo obj-imogen) false cy-15)
		    (attempt (obj-iachimo (cy-15 true)) true cy-16)
		    (steal (obj-iachimo obj-bracelet) true cy-17)
		    (lie (obj-iachimo cy-15 obj-posthumus) true cy-18)
		    (believe (obj-posthumus (cy-15 true)) true cy-19)
		    (kill (obj-pisanio obj-imogen) false cy-20)
		    (order (obj-posthumus (cy-20 true)) true cy-21)
		    (reveal (obj-pisanio cy-21 obj-imogen) true cy-22)
		    (masquerade (obj-imogen obj-boy) true cy-23)
		    (kill (obj-queen obj-imogen) false cy-24)
		    (attempt (obj-queen (cy-24 true)) true cy-25)
		    (war (obj-england obj-rome) true cy-26)
		    (follow (obj-clotten obj-imogen) true cy-27)
		    (masquerade (obj-cloten obj-posthumus) true cy-28)
		    (kill (obj-belarius obj-clotten) true cy-29)
		    (repent (obj-posthumus cy-21) true cy-30)
		    (soldier (obj-posthumus) true cy-31)
		    (desert (obj-posthumus) true cy-32)
		    (disarm (obj-posthumus obj-iachimo) true cy-33)
		    (capture (obj-rome obj-cymbeline) true cy-34)
		    (rescue (obj-belarius obj-cymbeline) true cy-35)
		    (help (obj-posthumus obj-belarius cy-35) true cy-36)
		    (win (obj-england) true cy-37)
		    (confess (obj-iachimo cy-18 obj-imogen) true cy-38)
		    (confess (obj-posthumus cy-21 obj-cymbeline) true cy-39)
		    (reveal (obj-pisanio cy-23 obj-posthumus) true cy-40)
		    (end (cy-23) true cy-41)
		    (reunited (obj-posthumus obj-imogen) true cy-42)
		    (peace (obj-england obj-rome) true cy-43)
		   )
	     )
)	    

; Concerns: 
;   prop cy-30 could also be remorseful, with or without a cause
;  proposition.


; -----------------------------------------------------------------------------
; Histories

; Richard the Second

(make_struc 'richard-II 'play
            '(characters ((king (obj-richard) true r2[Bc-1)
			  (duke (obj-bolingbroke) true r2c-2)
			  (duke (obj-mowbray) true r2c-3)
			  (duke (obj-john) true r2c-4)
			  (cousins (obj-richard obj-john) true r2c-5)
			  (son (obj-bolingbroke obj-john) true r2c-6)
			  (duke (obj-york) true r2c-7)
			  (bishop (obj-bishop) true r2c-8)
			  (courtier (obj-exton) true r2c-9)
			  (country (obj-england) true r2c-10)
			  (army (obj-army obj-england) true r2c-11)
			 )
	     )
	    '(plot ((quarrel (obj-bolingbroke obj-mowbray) true r2-1)
		    (accuse (obj-bolingbroke obj-mowbray) true r2-2)
		    (warn-against (obj-mowbray obj-richard obj-bolingbroke) true r2-3)
		    (banish (obj-richard obj-bolingbroke) true r2-4)
		    (banish (obj-richard obj-mowbray) true r2-5)
		    (criticize (obj-john obj-richard) true r2-6)
		    (funds (obj-funds) true r2-7)
		    (mishandle (obj-richard obj-funds) true r2-8)
		    (impoverish (obj-richard obj-england) true r2-9)
		    (cause (r2-8 r2-9) true r2-10)
		    (warn-of (obj-john obj-richard r2-9) true r2-11)
		    (ignore (obj-richard r2-11) true r2-12)
		    (die (obj-john) true r2-13)
		    (estate (obj-estate) true r2-14)
		    (belong-to (obj-estate obj-john) true r2-15)
		    (seize (obj-richard obj-estate) true r2-16)
		    (dissuade (obj-york obj-richard r2-16) false r2-17)
		    (attempt (obj-york (r2-17 true)) true r2-18)
		    (ignore (obj-richard obj-york r2-18) true r2-19)
		    (return-to (obj-bolingbroke obj-england) true r2-20)
		    (cause (r2-16 r2-20) true r2-21)
		    (friendless (obj-richard) true r2-22)
		    (lead (obj-bolingbroke obj-army) true r2-23)
		    (imprison (obj-bolingbroke obj-richard) true r2-24)
		    (abdicate (obj-richard) true r2-25)
		    (confess (obj-richard r2-8) true r2-26)
		    (cause (r2-24 r2-25) true r2-27)
		    (cause (r2-24 r2-26) true r2-28)
		    (denounce (obj-bishop obj-bolingbroke) true r2-29)
		    (cause (r2-24 r2-29) true r2-30)
		    (king (obj-bolingbroke) true r2-31)
		    (cause (r2-25 r2-31) true r2-32)
		    (followers (obj-conspirators obj-richard) true r2-33)
		    (assassinate (obj-conspirators obj-bolingbroke) false r2-34)
		    (plot (obj-conspirators r2-34) true r2-35)
		    (warn-of (obj-york obj-bolingbroke r2-35) true r2-36)
		    (punish (obj-bolingbroke obj-conspirators) true r2-37)
		    (cause (r2-35 r2-37) true r2-38)
		    (kill (obj-exton obj-richard) true r2-39)
		    (order (obj-bolingbroke (r2-39 true)) true r2-40)
		   )
              )
)


; Notes:
;  warn-against in r2-3 means: the first person warns the second person against
; (about) the third person.
; Concerns:
;  propositions r2-17 and r2-18: york attempts to dissuade richard from doing
; r2-16, but fails to dissuade him.  Thus, attempt means that the person
; attempts to do the proposition with the opposite truth value that the
; proposition actually has? (like lie?) That is, attempt is always a failure.
; The other problem is in the fals truth value for dissuade-- that should
; be treated as not-dissuade instead of persuade....

; Henry the Fifth

(make_struc 'henry-V 'play
	    '(characters ((king (obj-henry) true hfc-1)
			  (king (obj-charles) true hfc-2)
			  (son (obj-dauphin obj-charles) true hfc-3)
			  (daughter (obj-katherine obj-charles) true hfc-4)
			  (army (obj-english obj-england) true hfc-5)
			  (army (obj-french obj-france) true hfc-6)
			  (country (obj-england) true hfc-7)
			  (country (obj-france) true hfc-8)
			 )
	     )
	    '(plot ((throne (obj-throne) true hf-1)
		    (want (obj-henry obj-throne obj-france) true hf-2)
		    (scorn (obj-dauphin obj-henry) true hf-3)
		    (attack (obj-english obj-french) true hf-4)
		    (conjoin-event (hf-2 hf-3) true hf-5)
		    (cause (hf-5 hf-4) true hf-6)
		    (war (obj-english obj-french) true hf-7)
		    (lead (obj-henry obj-english) true hf-8)
		    (outnumber (obj-french obj-english) true hf-9)
		    (surrender (obj-english) false hf-10)
		    (demand (obj-dauphin hf-10) true hf-11)
		    (cause (hf-9 hf-11) true hf-12)
		    (encourage (obj-henry obj-english) true hf-13)
		    (defeat (obj-english obj-french) true hf-14)
		    (love (obj-henry obj-katherine) true hf-15)
		    (demand (obj-henry hf-18) true hf-16)
		    (marry (obj-katherine obj-henry) true hf-17)
		    (permit (obj-charles hf-17) true hf-18)
		    (cause (hf-14 hf-16) true hf-19)
		   )
	     )
)

; Short and sweet.
; Concerns:  perhaps the king propositions should declare what the person
;   is king of -- i.e. (king (obj-henry obj-england) true).


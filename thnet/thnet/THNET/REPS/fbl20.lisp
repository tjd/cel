
; FILE:       /u2/thnet/data/arcs/fables20
; PURPOSE:    test retrieval using fables (numbers 11-15,17-21)
; PROGRAMMER: Greg Nelson
; CREATED:    6-14-88
; UPDATED:    6-15-88
; UPDATED:    7-21-88 - Changing some morals slightly, "UNKNOWN" replacement
; UPDATED:    7-25-88 - Added hypotheticals.

; REJECT LIST
; FABLE 16

(defun lfa20 () (load "//tinman/ucla/psych/emelz/ARCS/fables20"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable11 fable12 fable13 fable14 fable15 fable17 fable18 fable19 fable20 fable21)))
		      
; FABLE 11:  One-way Traffic
; It might be interesting at some point to take out the propositions about 
; the deception (f11-18,19,20) to see what sort of difference this would
; make as far as retrieval is concerned.

(defun make_fable11 nil
  (make_struc 'fable11 'fable
	    '(story ((lion (obj-lion) true f11-1)
		     (animal (obj-animals) true f11-2)
		     (fox (obj-fox) true f11-3)
		     (cave (obj-cave) true f11-4)
		     (tracks (obj-tracks-in) true f11-5)
		     (tracks (obj-tracks-out) true f11-6)
		     (inside (obj-lion obj-cave) true f11-7)
		     (enter (obj-animals obj-cave) true f11-8)
		     (eat (obj-lion obj-animals) true f11-9)
		     (leave (obj-animals obj-cave) false f11-10)
		     (cause (f11-9 f11-10) true f11-11)
		     (see (obj-fox obj-tracks-in) true f11-12)
		     (cause (f11-8 f11-12) true f11-13)
		     (see (obj-fox obj-tracks-out) false f11-14)
		     (cause (f11-10 f11-14) true f11-15)
		     (enter (obj-fox obj-cave) false f11-16)
		     (cause (f11-14 f11-16) true f11-17)
		     (deceive (obj-lion obj-animals) true f11-18)
		     (deceive (obj-lion obj-fox) false f11-19)
		     (cause (f11-14 f11-19) true f11-20)
		    )
	     )
	    '(moral ((danger (conc-danger) true m11-1)
		     (man (obj-wise-man) true m11-2)
		     (wise (obj-wise-man) true m11-3)
		     (recognize (obj-wise-man conc-danger) true m11-4)
		     (cause (m11-3 m11-4) true m11-5)
		     (avoid (obj-wise-man conc-danger) true m11-6)
		     (cause (m11-4 m11-6) true m11-7)
		    )
             )
))

; FABLE 12:  Reaping Without Sowing
; The story and the moral, as written, seem to me dreadfully dissimilar.

(defun make_fable12 nil
  (make_struc 'fable12 'fable
	    '(story ((lion (obj-lion) true f12-1)
		     (bear (obj-bear) true f12-2)
		     (conjoin-object (obj-lion obj-bear obj-lion-and-bear) true f12-3)
		     (fawn (obj-fawn) true f12-4)
		     (fox (obj-fox) true f12-5)
		     (fight (obj-lion obj-bear) true f12-6)
		     (ill (obj-lion) true f12-7)
		     (ill (obj-bear) true f12-8)
		     (cause (f12-6 f12-7) true f12-9)
		     (cause (f12-6 f12-8) true f12-10)
		     (take (obj-fox obj-fawn obj-lion-and-bear) true f12-11)
		     (conjoin-event (f12-7 f12-8) true f12-12)
		     (cause (f12-12 f12-11) true f12-13)
		     (upset (obj-lion) true f12-14)
		     (upset (obj-bear) true f12-15)
		     (cause (f12-11 f12-14) true f12-16)
		     (cause (f12-11 f12-15) true f12-17)
		    )
	     )
	    '(moral ((people (obj-people) true m12-1)
		     (people (obj-chance-comer) true m12-2)
		     (labor (obj-labor) true m12-3)
		     (take (obj-chance-comer obj-labor obj-people) true m12-4)
		     (upset (obj-people) unknown m12-5)
		     (justified ((m12-5 true)) true m12-6)
		     (cause (m12-4 m12-6) true m12-7)
		    )
             )
))

; FABLE 13:  Taught By Experience

(defun make_fable13 nil
  (make_struc 'fable13 'fable
	    '(story ((lion (obj-lion) true f13-1)
		     (donkey (obj-donkey) true f13-2)
		     (fox (obj-fox) true f13-3)
		     (game (obj-game) true f13-4)
		     (divide (obj-donkey obj-game obj-parts) true f13-5)
		     (hate (obj-lion obj-parts) true f13-6)
		     (kill (obj-lion obj-donkey) true f13-7)
		     (learn-how (obj-fox (f13-9 true)) true f13-8)
		     (divide (obj-fox obj-game obj-new-parts) true f13-9)
		     (like (obj-lion obj-new-parts) true f13-10)
		     (alive (obj-fox) true f13-11)
		     (cause (f13-6 f13-7) true f13-12)
		     (cause (f13-7 f13-8) true f13-13)
		     (cause (f13-10 f13-11) true f13-14)
		     (cause (f13-8 f13-10) true f13-15)
		    )
	     )
	    '(moral ((misfortune (obj-misfortune) true m13-1)
		     (befall (obj-misfortune obj-others) true m13-2)
		     (wisdom (conc-wisdom) true m13-3)
		     (learn (obj-learner conc-wisdom) true m13-4)
		     (cause (m13-2 m13-4) true m13-5)
		    )
             )
))

; FABLE 14:  The Fox Out-foxed

(defun make_fable14 nil
  (make_struc 'fable14 'fable
	    '(story ((ass (obj-ass) true f14-1)
		     (fox (obj-fox) true f14-2)
		     (lion (obj-lion) true f14-3)
		     (trap (obj-trap) true f14-4)
		     (allies (obj-ass obj-fox) true f14-5)
		     (threaten (obj-lion obj-fox) true f14-6)
		     (betray (obj-fox obj-ass) true f14-7)
		     (catch (obj-trap obj-ass) true f14-8)
		     (catch (obj-lion obj-fox) true f14-9)
		     (eat (obj-lion obj-fox) true f14-10)
		     (eat (obj-lion obj-ass) true f14-11)
		     (cause (f14-7 f14-8) true f14-12)
		     (cause (f14-8 f14-9) true f14-13)
		    )
	     )
	    '(moral ((friends (obj-person obj-friend) true m14-1)
		     (plot-against (obj-person obj-friend) true m14-2)
		     (destroyed (obj-person) true m14-3)
		     (surprised-that (obj-person m14-3) true m14-4)
		     (cause (m14-2 m14-3) true m14-5)
		     (occurs-often (m14-5) true m14-6)
		    )
             )
))

; FABLE 15:  Blood Suckers

(defun make_fable15 nil
  (make_struc 'fable15 'fable
	    '(story ((fox (obj-fox) true f15-1)
		     (hedgehog (obj-hedgehog) true f15-2)
		     (river (obj-river) true f15-3)
		     (ticks (obj-ticks) true f15-4)
		     (ticks (obj-other-ticks) true f15-5)
		     (cross (obj-fox obj-river) true f15-6)
		     (torment (obj-ticks obj-fox) true f15-7)
		     (remove (obj-hedgehog obj-ticks obj-fox) false f15-8)
		     (desire (obj-fox (f15-8 true)) false f15-9)
		     (worse (obj-other-ticks obj-ticks) true f15-10)
		     (torment (obj-other-ticks obj-fox) false f15-11)
		     (cause (f15-9 f15-8) true f15-12)
		     (cause (f15-10 f15-9) true f15-13)
		     (cause (f15-8 f15-11) true f15-14)
		    )
	     )
	    '(moral ((men (obj-men) true m15-1)
		     (man (obj-demagogue) true m15-2)
		     (men (obj-other-demagogues) true m15-3)
		     (harm (obj-demagogue obj-men) false m15-4)
		     (harm (obj-other-demagogues obj-men) unknown m15-5)
		     (kill (obj-men obj-demagogue) unknown m15-6)
		     (if ((m15-6 true) (m15-5 true)) true m15-7)
		    )
             )
))

; FABLE 16:  Men and Lions
; NOT going to be entered (at least for now)

; FABLE 17:  Quality, Not Quantity
; No moral in book.

(defun make_fable17 nil
  (make_struc 'fable17 'fable
	    '(story ((vixen (obj-vixen) true f17-1)
		     (lioness (obj-lioness) true f17-2)
		     (child (obj-pups obj-vixen) true f17-3)
		     (child (obj-cub obj-lioness) true f17-4)
		     (more (obj-pups obj-cub) true f17-5)
		     (better (obj-cub obj-pups) true f17-6)
		    )
	     )
))

; FABLE 18:  Disarmed

(defun make_fable18 nil
  (make_struc 'fable18 'fable
	    '(story ((lion (obj-lion) true f18-1)
		     (farmer (obj-farmer) true f18-2)
		     (daughter (obj-daughter obj-farmer) true f18-3)
		     (teeth-of (obj-teeth obj-lion) true f18-4)
		     (claws-of (obj-claws obj-lion) true f18-5)
		     (conjoin-object (obj-teeth obj-claws obj-teeth-and-claws) true f18-6)
		     (love (obj-lion obj-daughter) true f18-7)
		     (dislike (obj-farmer obj-lion) true f18-8)
		     (fear (obj-farmer obj-teeth-and-claws) true f18-9)
		     (request (obj-farmer (f18-11 true) obj-lion) true f18-10)
		     (remove (obj-lion obj-teeth-and-claws obj-lion) true f18-11)
		     (scorn (obj-farmer obj-lion) true f18-12)
		     (marry (obj-lion obj-daughter) false f18-13)
		     (conjoin-event (f18-8 f18-9) true f18-14)
		     (cause (f18-14 f18-10) true f18-15)
		     (cause (f18-10 f18-11) true f18-16)
		     (cause (f18-11 f18-12) true f18-17)
		     (cause (f18-12 f18-13) true f18-18)
		    )
	     )
	    '(moral ((people (obj-others) true m18-1)
		     (advantage (conc-advantage) true m18-2)
		     (respect (conc-respect) true m18-3)
		     (relinquish (obj-special conc-advantage obj-others) unknown m18-4)
		     (request (obj-others (m18-4 true) obj-special) true m18-5)
		     (listen-to (obj-special obj-others) unknown m18-6)
		     (lose (obj-special conc-respect) unknown m18-7)
		     (if ((m18-6 true) (m18-4 true)) true m18-8)
		     (if ((m18-4 true) (m18-7 true)) true m18-9)
		    )
             )
))

; FABLE 19:  Third Party Profit

(defun make_fable19 nil
  (make_struc 'fable19 'fable
	    '(story ((lion (obj-lion) true f19-1)
		     (boar (obj-boar) true f19-2)
		     (vultures (obj-vultures) true f19-3)
		     (spring (obj-spring) true f19-4)
		     (dispute (obj-lion obj-boar obj-spring) true f19-5)
		     (fight (obj-lion obj-boar) true f19-6)
		     (see (obj-lion obj-vultures) true f19-7)
		     (see (obj-boar obj-vultures) true f19-8)
		     (end (f19-6) true f19-9)
		     (cause (f19-5 f19-6) true f19-10)
		     (conjoin-event (f19-7 f19-8) true f19-11)
		     (cause (f19-11 f19-9) true f19-12)
		    )
	     )
	    '(moral ((danger (obj-danger) true m19-1)
		     (strife (conc-strife) unknown m19-2)
		     (argue (obj-party-a obj-party-b) unknown m19-3)
		     (exist (obj-danger) unknown m19-4)
		     (if ((m19-3 true) (m19-2 true)) true m19-5)
		     (if ((m19-2 true) (m19-4 true)) true m19-6)
		    )
             )
))

; FABLE 20:  A Bird In The Hand
; Also see fable 73

(defun make_fable20 nil
  (make_struc 'fable20 'fable
	    '(story ((lion (obj-lion) true f20-1)
		     (hare (obj-hare) true f20-2)
		     (deer (obj-deer) true f20-3)
		     (asleep (obj-hare) true f20-4)
		     (find (obj-lion obj-hare) true f20-5)
		     (see (obj-lion obj-deer) true f20-6)
		     (chase (obj-lion obj-deer) true f20-7)
		     (awaken (obj-hare) true f20-8)
		     (catch (obj-lion obj-deer) false f20-9)
		     (catch (obj-lion obj-hare) false f20-10)
		     (cause (f20-7 f20-8) true f20-11)
		     (cause (f20-8 f20-10) ture f20-12)
		    )
	     )
	    '(moral ((men (obj-men) true m20-1)
		     (prospect (obj-lesser) true m20-2)
		     (prospect (obj-greater) true m20-3)
		     (greater (obj-greater obj-lesser) true m20-4)
		     (choose (obj-men obj-lesser) unknown m20-5)
		     (has (obj-men obj-lesser) unknown m20-6)
		     (risky (obj-greater) true m20-7)
		     (lose (obj-men obj-greater) true m20-8)
		     (if ((m20-5 true) (m20-6 true)) true m20-9)
		     (cause (m20-7 m20-8) true m20-10)
		    )
             )
))

; FABLE 21:  The Lion's Share

(defun make_fable21 nil
  (make_struc 'fable21 'fable
	    '(story ((lion (obj-lion) true f21-1)
		     (ass (obj-ass) true f21-2)
		     (prey (obj-animals) true f21-3)
		     (stronger (obj-lion obj-ass) true f21-4)
		     (divide (obj-lion obj-animals obj-thirds) true f21-5)
		     (take (obj-lion obj-thirds obj-ass) true f21-6)
		     (hungry (obj-ass) true f21-7)
		     (cause (f21-4 f21-6) true f21-8)
		     (cause (f21-6 f21-7) true f21-9)
		    )
	     )
	    '(moral ((man (obj-weaker) true m21-1)
		     (people (obj-stronger) true m21-2)
		     (stronger (obj-stronger obj-weaker) true m21-3)
		     (ally (obj-weaker obj-stronger) unknown m21-4)
		     (fool (obj-weaker) unknown m21-5)
		     (if ((m21-4 true) (m21-5 true)) true m21-6)
		     (cause (m21-3 m21-6) true m21-7)
		    )
             )
))



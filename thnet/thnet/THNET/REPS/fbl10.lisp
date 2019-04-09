
; FILE:       /u2/thnet/data/arcs/fables10
; PURPOSE:    test retrieval using fables (numbers 1-10)
; PROGRAMMER: Paul Thagard, Greg Nelson
; CREATED:    1-25-88
; UPDATED:    7-21-88 - Morals of fables 1-9 were completely rewritten, to make
;                       them match the book more closely.  "COND" changed to
;                       "UNKNOWN"
; UPDATED:    7-22-88 - "desire" changed to "desire-true"
; UPDATED:    7-25-88 - hypotheticals added


(defun lfa10 () (load "//tinman/ucla/psych/emelz/ARCS/fables10"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable1 fable2 fable3 fable4 fable5 fable6 fable7 fable8 fable9 fable10)))

; FABLE 1:  A Case for Patience

; This fable reminds me of the raccoon who, reaching in a small hole to get
; a shiny object, cannot get his paw back out because it is clasped around the
; object, but doesn't think to let go of it in order to get his paw back out

; CONCERNS:
; This is the introduction of the conditional predicate problem, the volition
; problem, the conceptual versus concrete debate, and is an actor in the
; varying length predicate problem.

(defun make_fable1 nil
  (make_struc 'fable1 'fable
	    '(story ((fox (obj-fox) true f1-1)
		     (food (obj-food) true f1-2)
		     (steal (obj-fox obj-food) true f1-3)
		     (hungry (obj-fox) unknown f1-4)
		     (depart (obj-fox) unknown f1-5)
		     (wait-for (obj-fox (f1-4 true)) unknown f1-6)
		     (if ((f1-4 true) (f1-5 true)) true f1-7)
		     (if ((f1-6 true) (f1-4 true)) true f1-8)
		     (cause (f1-3 f1-7) true f1-9)
		    )
	     )	
	    '(moral ((time (conc-time) true m1-1)
		     (problem (obj-problem) true m1-2)
		     (difficult (obj-problem) true m1-3)
		     (solve (conc-time obj-problem) true m1-4)
		    )
             )
))
		      
; FABLE 2:   Friend or Foe?
; COMMENTS:  This fable raises the question of "anything", etc.

(defun make_fable2 nil
  (make_struc 'fable2 'fable
	    '(story ((fox (obj-fox) true f2-1)
		     (bush (obj-bush) true f2-2)
		     (help (conc-help) true f2-3)
		     (want-from (obj-fox conc-help obj-bush) true f2-4)
		     (give (obj-bush conc-help anything) false f2-5)
		     (get-from (obj-fox conc-help obj-bush) false f2-6)
		     (cause (f2-5 f2-6) true f2-7)
		    )
	     )
	    '(moral ((people (obj-people) true m2-1)
		     (hurt (obj-people anything) true m2-2)
		     (help (obj-people anything) false m2-3)
		     (go-to (obj-fool obj-people) true m2-4)
		     (desire (obj-fool (m2-3 true)) true m2-5)
		     (fool (obj-fool) true m2-6)
		     (conjoin-event (m2-2 m2-3) true m2-7)
		     (cause (m2-7 m2-6) true m2-8)
		    )
	     )
))

; FABLE3:  Sour Grapes

(defun make_fable3 nil
  (make_struc 'fable3 'fable
            '(story ((fox (obj-fox) true f3-1)
		     (grapes (obj-grapes) true f3-2)
		     (want (obj-fox obj-grapes) true f3-3)
		     (get (obj-fox obj-grapes) false f3-4)
		     (decide (obj-fox (f3-6 true)) true f3-5)
		     (sour (obj-grapes) false f3-6)
		     (cause (f3-4 f3-5) true f3-7)
                    )
             )
	    '(moral ((men (obj-men) true m3-1)
		     (circumstances (conc-circumstances) true m3-2)
		     (fail (obj-men) true m3-3)
		     (incapable (obj-men) true m3-4)
		     (cause (m3-4 m3-3) true m3-5)
		     (blame-for (obj-men conc-circumstances m3-3) true m3-6)
		    )
	     )
))

; FABLE4:  Actions Speak Louder than Words
; I've simplified this by replacing "wood-cutter" with man, and trying to
; generalize the predicates.  "ask" is three place: arg1 asked arg2 for arg3

; CONCERNS:
; I wasn't sure how to represent that the reason the fox did not like the man
; (alw7) was that he said one thing (alw5) and did another (alw6), and what
; he did was more important than what he said (which, I believe, is the crux
; of the issue)
; The moral is completely dependent on the story itself for any "moral" --
; one might want to add some extra propositions to tell what is implied by the
; moral/story combination, such as:
; (scorn (obj-others obj-men) true m4-6)
; (conjoin-event (m4-3 m4-5) true m4-7)
; (cause (m4-7 m4-6) true m4-8)

(defun make_fable4 nil
  (make_struc 'fable4 'fable
	    '(story ((fox (obj-fox) true f4-1)
		     (man (obj-man) true f4-2)
		     (help (conc-help) true f4-3)
		     (ask (obj-fox obj-man conc-help) true f4-4)
		     (promise-to (obj-man conc-help obj-fox) true f4-5)
		     (give (obj-man conc-help obj-fox) false f4-6)
		     (like (obj-fox obj-man) false f4-7)
		     (conjoin-event (f4-5 f4-6) true f4-8)
		     (cause (f4-8 f4-7) true f4-9)
		    )
	     )
	    '(moral ((men (obj-men) true m4-1)
		     (virtuous (obj-men) false m4-2)
		     (rogues (obj-men) true m4-3)
		     (cause (m4-3 m4-2) true m4-4)
		     (pretend-that (obj-men (m4-2 true)) true m4-5)
		    )
	     )
))
		      
; FABLE5:  Fools Die for Want of Wisdom
; The causality in this story seems to be almost everything.  This version
; takes the role of the fox to be significant.  It shows the monkey to be
; foolish for following the fox.  The moral takes the view that it was the
; monkey's own foolishness which brought about his fate, and that the fox
; did not play any role.

(defun make_fable5 nil
  (make_struc 'fable5 'fable
	    '(story ((monkey (obj-monkey) true f5-1)
		     (fox (obj-fox) true f5-2)
		     (snare (obj-snare) true f5-3)
		     (meat (obj-meat) true f5-4)
		     (foolish (obj-monkey) true f5-5)
		     (king (obj-monkey) true f5-6)
		     (jealous (obj-fox obj-monkey) true f5-7)
		     (cause (f5-6 f5-7) true f5-8)
		     (show (obj-fox obj-meat obj-monkey) true f5-9)
		     (cause (f5-7 f5-9) true f5-10)
		     (take (obj-monkey obj-meat obj-snare) true f5-11)
		     (cause (f5-5 f5-11) true f5-12)
		     (catch (obj-snare obj-monkey) true f5-13)
		     (cause (f5-11 f5-13) true f5-14)
		    )
	     )
	    '(moral ((people (obj-people) true m5-1)
		     (act (obj-act) true m5-2)
		     (consider (obj-people obj-act) false m5-3)
		     (attempt (obj-people obj-act) true m5-4)
		     (suffer (obj-people) true m5-5)
		     (conjoin-event (m5-3 m5-4) true m5-6)
		     (cause (m5-6 m5-5) true m5-7)
		     (laugh-at (obj-others obj-people) true m5-8)
		     (cause (m5-6 m5-8) true m5-9)
		    )
	     )
))

; FABLE6:  Dead Men Tell No Tales
; CONCERNS:
; Here comes in the problem of a lack of representation for *p<->q.
; Really, (f6-6) if and only if (not (f6-7)).  I am only assuming this is
; represented by the causal relationship here, which might well be stating
; quite the opposite ( (f6-6) because (f6-7) ).

(defun make_fable6 nil
  (make_struc 'fable6 'fable
	    '(story ((fox (obj-fox) true f6-1)
		     (monkey (obj-monkey) true f6-2)
		     (tomb (obj-tomb) true f6-3)
		     (untruth (obj-lie) true f6-4)
		     (boast (obj-lie) true f6-5)
		     (say (obj-monkey obj-lie obj-fox) true f6-6)
		     (say (obj-tomb anything anything) false f6-7)
		     (cause (f6-7 f6-6) true f6-8)
		    )
             )
	    '(moral ((men (obj-men) true m6-1)
		     (impostors (obj-men) true m6-2)
		     (truth (obj-truth) true m6-3)
		     (know (obj-other obj-truth) prob m6-4)
		     (lie (obj-lie) true m6-5)
		     (boast (obj-lie) m6-6)
		     (say (obj-men obj-lie) unknown m6-7)
		     (if ((m6-4 false) (m6-7 true)) true m6-8)
		     (cause (m6-2 m6-8) true m6-9)
		    )
	     )
))


; FABLE7:  Look Before You Leap
; The fox, who is otherwise irrelevant, winds up pretty well entangled in this
; one because he convinces the goat to jump, and then, because the goat jumps,
; can leave himself.

(defun make_fable7 nil
  (make_struc 'fable7 'fable
	    '(story ((fox (obj-fox) true f7-1)
		     (goat (obj-goat) true f7-2)
		     (water-well (obj-well) true f7-3)
		     (plan (obj-plan) true f7-4)
		     (say (obj-fox (f7-7 true) obj-goat) true f7-5)
		     (think (obj-goat obj-plan) false f7-6)
		     (jump-in (obj-goat obj-well) true f7-7)
		     (leave (obj-fox obj-well) true f7-8)
		     (cause (f7-7 f7-8) true f7-9)
		     (leave (obj-goat obj-well) false f7-10)
		     (cause (f7-6 f7-10) true f7-11)
		    )
	     )
	    '(moral ((man (obj-man) true m7-1)
		     (sensible (obj-man) true m7-2)
		     (project (obj-project) true m7-3)
		     (plan-for (obj-plan obj-project) true m7-4)
		     (see (obj-man obj-plan) unknown m7-5)
		     (begin (obj-man obj-project) unknown m7-6)
		     (when (m7-5 m7-6) true m7-7)
		     (cause (m7-2 m7-7) true m7-8)
		    )
	     )
))

; FABLE8:  Cut Off Your Tails to Save My Face!

(defun make_fable8 nil
  (make_struc 'fable8 'fable
	    '(story ((fox (obj-fox) true f8-1)
		     (tail (obj-tail) true f8-2)
		     (foxes (obj-other-foxes) true f8-3)
		     (tails (obj-other-tails) true f8-4)
		     (has (obj-fox obj-tail) false f8-5)
		     (have (obj-other-foxes obj-other-tails) true f8-6)
		     (good (f8-6) true f8-7)
		     (think (obj-other-foxes f8-7) true f8-8)
		     (desire (obj-fox (f8-13 true)) true f8-9)
		     (cause (f8-5 f8-9) true f8-10)
		     (lie (obj-fox f8-7 obj-other-foxes) true f8-11)
		     (cause (f8-9 f8-11) true f8-12)
		     (persuade (obj-fox obj-other-foxes (f8-7 false)) false f8-13)
		     (conjoin-event (f8-8 f8-11) true f8-14)
		     (fail-to (obj-fox (f8-13 true)) true f8-15)
		     (cause (f8-14 f8-15) true f8-16)
		    )
	     )
	    '(moral ((neighbors-of (obj-neighbors obj-those) true m8-1)
		     (advice (obj-advice) true m8-2)
		     (offer-to (obj-those obj-advice obj-neighbors) true m8-3)
		     (benevolent (obj-those) false m8-4)
		     (cause (m8-4 m8-3) false m8-5)
		     (self-interested (obj-those) true m8-6)
		     (cause (m8-6 m8-3) true m8-7)
		     (bad (obj-those) true m8-8)
		     (conjoin-event (m8-5 m8-7) true m8-9)
		     (cause (m8-9 m8-8) true m8-10)
		    )
	     )
))

; FABLE9:  The Fox and the Mask
; This is certainly the simplest fable yet!!!
; I see some ambiguity in the original story, in that perhaps it is the fox
; who is supposedly beautiful but unintelligent, for thinking that the mask
; could even have a brain!!!

(defun make_fable9 nil
  (make_struc 'fable9 'fable
	    '(story ((fox (obj-fox) true f9-1)
		     (mask (obj-mask) true f9-2)
		     (see (obj-fox obj-mask) true f9-3)
		     (beautiful (obj-mask) true f9-4)
		     (intelligent (obj-mask) false f9-5)
		    )
	     )
	    '(moral ((men (obj-men) true m9-1)
		     (handsome (obj-men) true m9-2)
		     (dumb (obj-men) true m9-3)
		     (conjoin-event (m9-2 m9-3) true m9-4)
		     (occurs-sometimes (m9-4) true m9-5)
		    )
	     )
))

; FABLE10:  A Lesson For Fools (Le Corbeau et Le Renard)
; This one does not have a moral recorded.

(defun make_fable10 nil
  (make_struc 'fable10 'fable
	    '(story ((crow (obj-crow) true f10-1)
		     (meat (obj-meat) true f10-2)
		     (fox (obj-fox) true f10-3)
		     (flattery (conc-flattery) unknown f10-4)
		     (want-from (obj-fox obj-meat obj-crow) true f10-5)
		     (say (obj-fox conc-flattery obj-crow) true f10-6)
		     (cause (f10-5 f10-6) true f10-7)
		     (possible ((f10-9 true)) unknown f10-8)
		     (sing (obj-crow) unknown f10-9)
		     (if ((f10-9 true) (f10-8 true)) true f10-10)
		     (drop (obj-crow obj-meat) unknown f10-11)
		     (if ((f10-9 true) (f10-11 true)) true f10-12)
		     (get (obj-fox obj-meat) unknown f10-13)
		     (if ((f10-11 true) (f10-13 true)) true f10-14)
		     (cause (f10-18 f10-9) true f10-15)
		     (if ((f10-8 true) (f10-4 true)) true f10-16)
		     (vain (obj-crow) true f10-17)
		     (desire (obj-crow (f10-4 true)) true f10-18)
		     (cause (f10-17 f10-18) true f10-19)
		    )
	      )
))



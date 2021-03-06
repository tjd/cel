
; FILE:       /u2/thnet/data/arcs/fables50
; PURPOSE:    test retrieval using fables (numbers 43-52)
; PROGRAMMER: Greg Nelson
; CREATED:    7-5-88
; UPDATED:    7-21-88 - "UNKNOWN" added
; UPDATED:    7-22-88 - Finished, "DESIRE" -> "DESIRE-TRUE"
; UPDATED:    7-25-88 - Added hypotheticals.

(defun lfa50 () (load "//tinman/ucla/psych/emelz/ARCS/fables50"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable43 fable44 fable45 fable46 fable47 fable48 fable49 fable50 fable51 fable52)))

; FABLE 43:  One Is Enough

(defun make_fable43 nil
  (make_struc 'fable43 'fable
	    '(story ((sun (obj-sun) true f43-1)
		     (frogs (obj-frogs) true f43-2)
		     (rejoice-about (obj-frogs f43-5) true f43-3)
		     (sun (obj-wife) true f43-4)
		     (marry (obj-sun obj-wife) true f43-5)
		     (frog (obj-wise-frog) true f43-6)
		     (foolish (f43-3) true f43-7)
		     (say (obj-wise-frog f43-7 obj-frogs) true f43-8)
		     (beget (obj-sun obj-child) unknown f43-9)
		     (sun (obj-child) true f43-10)
		     (conjoin-event (f43-5 f43-9) true f43-11)
		     (pools (obj-pools) true f43-12)
		     (need (obj-frogs obj-pools) true f43-13)
		     (dry-up (obj-pools) unknown f43-14)
		     (bad-for ((f43-14 true) obj-frogs) unknown f43-15)
		     (if ((f43-11 true) (f43-14 true)) true f43-16)
		     (if ((f43-14 true) (f43-15 true)) true f43-17)
		     (cause (f43-13 f43-17) true f43-18)
		    )
	     )
	    '(moral ((people (obj-people) true m43-1)
		     (dumb (obj-people) true m43-2)
		     (reason (obj-reason) true m43-3)
		     (rejoice-about (obj-people obj-reason) true m43-4)
		     (wrong (obj-reason) true m43-5)
		    )
             )
))

; FABLE 44:  A Voice and Nothing More

(defun make_fable44 nil
  (make_struc 'fable44 'fable
	    '(story ((lion (obj-lion) true f44-1)
		     (frog (obj-frog) true f44-2)
		     (croak (obj-frog) true f44-3)
		     (small (obj-frog) true f44-4)
		     (believe (obj-lion (f44-4 false)) true f44-5)
		     (see (obj-lion obj-frog) true f44-6)
		     (end (f44-5) true f44-7)
		     (crush (obj-lion obj-frog) true f44-8)
		     (cause (f44-3 f44-5) true f44-9)
		     (cause (f44-6 f44-7) true f44-10)
		    )
	     )
	    '(moral ((people (obj-people) true m44-1)
		     (talk (obj-people) true m44-2)
		     (always (m44-2) true m44-3)
		     (stupid (obj-people) true m44-4)
		     (cause (m44-2 m44-4) true m44-5)
		    )
             )
))

; FABLE 45:  Making the The Punishment Fit the Crime
; COMMENTS:  The "even-though" predicate feels like a massive fudge -- how
;            do we fix this, if it is?

(defun make_fable45 nil
  (make_struc 'fable45 'fable
	    '(story ((rat (obj-rat) true f45-1)
		     (frog (obj-frog) true f45-2)
		     (befriend (obj-rat obj-frog) true f45-3)
		     (dinner (obj-dinner) true f45-4)
		     (tie-together (obj-frog obj-frog obj-rat) true f45-5)
		     (conjoin-object (obj-frog obj-rat obj-both) true f45-6)
		     (search-for (obj-both obj-dinner) true f45-7)
		     (pond (obj-pond) true f45-8)
		     (jump-in (obj-frog obj-pond) true f45-9)
		     (fall-in (obj-rat obj-pond) true f45-10)
		     (cause (f45-5 f45-6) true f45-11)
		     (conjoin-event (f45-5 f45-9) true f45-12)
		     (cause (f45-12 f45-10) true f45-13)
		     (drown (obj-rat) true f45-14)
		     (cause (f45-10 f45-14) true f45-15)
		     (float-on (obj-both obj-pond) true f45-16)
		     (cause (f45-14 f45-16) true f45-17)
		     (kite (obj-kite) true f45-18)
		     (catch (obj-kite obj-both) true f45-19)
		     (cause (f45-16 f45-19) true f45-20)
		     (eat (obj-kite obj-both) true f45-21)
		     (cause (f45-19 f45-21) true f45-22)
		    )
	     )
	    '(moral ((person (obj-person) true m45-1)
		     (dead (obj-person) true m45-2)
		     (enemy-of (obj-enemy obj-person) true m45-3)
		     (injured (obj-enemy obj-person) true m45-4)
		     (avenge (obj-person m45-4) true m45-5)
		     (even-though (m45-2 m45-5) true m45-6)
		     (justice (obj-justice) true m45-7)
		     (divine (obj-justice) true m45-8)
		     (see (obj-justice everything) true m45-9)
		     (cause (m45-9 m45-6) true m45-10)
		    )
             )
))

; FABLE 46:  Too Big For Her Skin
; COMMENTS:  This is extremely time dependent, in that someone (frog) repeats
;            the same action several times with different consequences. 
;            This makes me wonder: one of the Shakespeare plays deals with
;            this, doesn't it?  Reminds me of #21 and 36, and might hit
;            #26 and 39.

(defun make_fable46 nil
  (make_struc 'fable46 'fable
	    '(story ((frog (obj-frog) true f46-1)
		     (ox (obj-ox) true f46-2)
		     (small (obj-frog) true f46-3)
		     (huge (obj-ox) true f46-4)
		     (children-of (obj-children obj-frog) true f46-5)
		     (envy (obj-frog obj-ox) true f46-6)
		     (cause (f46-4 f46-6) true f46-7)
		     (puff-up (obj-frog) true f46-8)
		     (ask-if (obj-frog obj-children f46-3) true f46-9)
		     (persist (f46-3) true f46-10)
		     (say (obj-children f46-10 obj-frog) true f46-11)
		     (puff-up (obj-frog) true f46-12)
		     (burst (obj-frog) true f46-13)
		     (cause (f46-12 f46-13) true f46-14)
		     (die (obj-frog) true f46-15)
		     (cause (f46-13 f46-15) true f46-16)
		    )
	     )
	    '(moral ((weak (obj-weak) true m46-1)
		     (strong (obj-strong) true m46-2)
		     (imitate (obj-weak obj-strong) unknown m46-3)
		     (destroyed (obj-weak) unknown m46-4)
		     (if ((m46-3 true) (m46-4 true)) true m46-5)
		    )
             )
))

; FABLE 47:  A Lesson Learnt Too Late
; COMMENTS:  I'm not doing the moral for now because of its extreme temporal
;            dependence.

(defun make_fable47 nil
  (make_struc 'fable47 'fable
	    '(story ((bird (obj-bird) true f47-1)
		     (cage (obj-cage) true f47-2)
		     (inside (obj-bird obj-cage) true f47-3)
		     (sing (obj-bird) unknown f47-4)
		     (night (obj-time) unknown f47-5)
		     (if ((f47-5 true) (f47-4 true)) true f47-6)
		     (bat (obj-bat) true f47-7)
		     (ask-why (obj-bat obj-bird f47-6) true f47-8)
		     (sang (obj-bird) true f47-9)
		     (day (obj-other-time) true f47-10)
		     (before (obj-other-time obj-time) true f47-11)
		     (conjoin-event (f47-9 f47-10) true f47-12)
		     (cause (f47-12 f47-3) true f47-13)
		     (say (obj-bird f47-13) true f47-14)
		    )
	     )
;	    '(moral ((thing (obj-thing) true m47-1)
;		     (bad (obj-thing) unknown m47-2)
;		     (allow (obj-actor (m47-2 true)) unknown m47-3)
;		     (if ((m47-3 true) (m47-2 true)) true m47-4)
;		     (allow (obj-actor (m47-2 true)) false m47-5)
;		     (before ((m47-3 true) m47-5) true m47-6)
;		    )
;	     )
))

; FABLE 48:  Doubly Disabled

(defun make_fable48 nil
  (make_struc 'fable48 'fable
	    '(story ((mole (obj-mole) true f48-1)
		     (mother-of (obj-mother obj-mole) true f48-2)
		     (see (obj-mole anything) false f48-3)
		     (lie-about (obj-mole f48-3 obj-mother) true f48-4)
		     (frankincense (obj-frankincense) true f48-5)
		     (give-to (obj-mother obj-frankincense obj-mole) true f48-6)
		     (ask-what (obj-mother obj-mole obj-frankincense) true f48-7)
		     (pebble (obj-frankincense) false f48-8)
		     (lie-about (obj-mole f48-8 obj-mother) true f48-9)
		     (say-to (obj-mother f48-3 obj-mole) true f48-10)
		     (smell (obj-mole anything) false f48-11)
		     (say-to (obj-mother f48-11 obj-mole) true f48-12)
		    )
	     )
	    '(moral ((people (obj-people) true m48-1)
		     (act (obj-act) true m48-2)
		     (impossible (obj-act) true m48-3)
		     (do (obj-people obj-act) false m48-4)
		     (cause (m48-3 m48-4) true m48-5)
		     (test (obj-test) true m48-6)
		     (simple (obj-test) true m48-7)
		     (lie-about (obj-people m48-4 anything) true m48-8)
		     (impostors (obj-people) true m48-9)
		     (cause (m48-8 m48-9) true m48-10)
		     (show-that (obj-test m48-9) true m48-11)
		    )
             )
))

; FABLE 49:  The Imitative Instinct

(defun make_fable49 nil
  (make_struc 'fable49 'fable
	    '(story ((monkey (obj-monkey) true f49-1)
		     (fishermen (obj-fishermen) true f49-2)
		     (net (obj-net) true f49-3)
		     (river (obj-river) true f49-4)
		     (cast-into (obj-fishermen obj-net obj-river) true f49-5)
		     (watch (obj-monkey f49-5) true f49-6)
		     (leave (obj-fishermen obj-river) true f49-7)
		     (attempt (obj-monkey (f49-9 true)) true f49-8)
		     (cast-into (obj-monkey obj-net obj-river) false f49-9)
		     (entangle (obj-not obj-monkey) true f49-10)
		     (drown (obj-monkey) false f49-11)
		     (nearly ((f49-11 true)) true f49-12)
		     (cause (f49-8 f49-10) true f49-13)
		     (cause (f49-10 f49-12) true f49-14)
		    )
	     )
	    '(moral ((concern (obj-concern obj-you) false m49-1)
		     (meddle-with (obj-you obj-concern) unknown m49-2)
		     (gain (obj-you anything) false m49-3)
		     (regret (obj-you (m49-2 true)) unknown m49-4)
		     (if ((m49-2 true) (m49-4 true)) true m49-5)
		    )
             )
))

; FABLE 50:  A Clumsy Liar
; COMMENTS:  Wow -- this is incredibly intricate.  Needed to be cut down to
;            somewhat of a summary.

(defun make_fable50 nil
  (make_struc 'fable50 'fable
	    '(story ((monkey (obj-monkey) true f50-1)
		     (ship (obj-ship) true f50-2)
		     (ocean (obj-ocean) true f50-3)
		     (dolphin (obj-dolphin) true f50-4)
		     (capsize (obj-ship) true f50-5)
		     (rescue-from (obj-dolphin obj-monkey obj-ocean) true f50-6)
		     (port (obj-port) true f50-7)
		     (bring-to (obj-dolphin obj-monkey obj-port) true f50-8)
		     (lie-to (obj-monkey obj-dolphin) true f50-9)
		     (know (obj-dolphin f50-9) false f50-10)
		     (fact (obj-fact) true f50-11)
		     (know (obj-monkey obj-fact) false f50-12)
		     (lie-about (obj-monkey obj-fact obj-dolphin) true f50-13)
		     (realize (obj-dolphin f50-13) true f50-14)
		     (anger (f50-14 obj-dolphin) true f50-15)
		     (drop-in (obj-dolphin obj-monkey obj-ocean) true f50-16)
		     (cause (f50-15 f50-16) true f50-17)
		     (drown (obj-monkey) true f50-18) ; implied
		     (cause (f50-16 f50-18) true f50-19)
		    )
	     )
	    '(moral ((people (obj-people) true m50-1)
		     (truth (conc-truth) true m50-2)
		     (ignorant-of (obj-people obj-truth) true m50-3)
		     (lies (obj-lies) true m50-4)
		     (say-to (obj-people obj-lies obj-others) true m50-5)
		     (believe (obj-others obj-lies) false m50-6)
		     (think (obj-people (m50-6 true)) m50-7)
		     (stupid (m50-7) true m50-8)
		     (cause (m50-3 m50-6) true m50-9)
		    )
             )
))

; FABLE 51:  Killed By Kindness

(defun make_fable51 nil
  (make_struc 'fable51 'fable
	    '(story ((ape (obj-neglected-child) true f51-1)
		     (ape (obj-cared-for-child) true f51-2)
		     (twins (obj-neglected-child obj-cared-for-child) true f51-3)
		     (mother-of (obj-mother obj-neglected-child) true f51-4)
		     (mother-of (obj-mother obj-cared-for-child) true f51-5)
		     (care-for (obj-mother obj-cared-for-child) true f51-6)
		     (neglect (obj-mother obj-neglected-child) true f51-7)
		     (smother (obj-mother obj-cared-for-child) true f51-8)
		     (die (obj-cared-for-child) true f51-9)
		     (cause (f51-6 f51-8) true f51-10)
		     (cause (f51-8 f51-9) true f51-11)
		     (lives (obj-neglected-child) true f51-12)
		    )
	     )
	    '(moral ((forethought (conc-forethought) true m51-1)
		     (destiny (conc-destiny) true m51-2)
		     (oppose (conc-forethought conc-destiny) unknown m51-3)
		     (prevail (conc-destiny) unknown m51-4)
		     (if ((m51-3 true) (m51-4 true)) true m51-5)
		    )
             )
))

; FABLE 52:  A Blood Feud
; COMMENTS:  Like Romeo and Juliet, perhaps?

(defun make_fable52 nil
  (make_struc 'fable52 'fable
	    '(story ((snake (obj-snake) true f52-1)
		     (man (obj-man) true f52-2)
		     (child-of (obj-child obj-man) true f52-3)
		     (kill (obj-snake obj-child) true f52-4)
		     (desire (obj-man (f52-7 true)) true f52-5)
		     (attempt-to (obj-man (f52-7 true)) true f52-6)
		     (kill (obj-man obj-snake) false f52-7)
		     (rock (obj-rock) true f52-8)
		     (chip (obj-man obj-rock) true f52-9)
		     (grave-of (obj-grave obj-child) true f52-10)
		     (see (obj-snake obj-rock) true f52-11)
		     (see (obj-man obj-grave) true f52-12)
		     (conjoin-event (f52-11 f52-12) true f52-13)
		     (feud-between (obj-man obj-snake obj-feud) true f52-14)
		     (cause (f52-4 f52-5) true f52-15)
		     (cause (f52-5 f52-6) true f52-16)
		     (cause (f52-6 f52-9) true f52-17)
		     (cause (f52-9 f52-11) true f52-18)
		     (cause (f52-4 f52-12) true f52-19)
		     (cause (f52-13 f52-14) true f52-20)
		    )
	     )
	    '(moral ((quarrel (obj-quarrel) true m52-1)
		     (serious (obj-quarrel) true m52-2)
		     (resolve (obj-actors obj-quarrel) true m52-3)
		     (difficult (m52-3) true m52-4)
		     (cause (m52-2 m52-4) true m52-5)
		    )
             )
))



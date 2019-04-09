
; FILE:       /u2/thnet/data/arcs/fables90
; PURPOSE:    test retrieval using fables (numbers 84-93)
; PROGRAMMER: Greg Nelson
; CREATED:    7-24-88
; UPDATED:    7-25-88 - Added hypotheticals.

(defun lfa90 () (load "//tinman/ucla/psych/emelz/fables90"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable84 fable85 fable86 fable87 fable88 fable89 fable90 fable91 fable92 fable93)))

; FABLE 84:  Misplaced Confidence
; COMMENTS:  Strong reminder of #60 and #63.

(defun make_fable84 nil
  (make_struc 'fable84 'fable
	    '(story ((halcyon (obj-halcyon) true f84-1)
		     (solitude (conc-solitude) true f84-2)
		     (love (obj-halcyon conc-solitude) true f84-3)
		     (nest-of (obj-nest obj-halcyon) true f84-4)
		     (cliff (obj-cliff) true f84-5)
		     (place-on (obj-halcyon obj-nest obj-cliff) true f84-6)
		     (men (obj-men) true f84-7)
		     (protect-from (obj-halcyon obj-nest obj-men) true f84-8)
		     (reason-for (f84-8 f84-6) true f84-9)
		     (wave (obj-wave) true f84-10)
		     (destroy (obj-wave obj-nest) true f84-11)
		     (expect (obj-halcyon f84-11) false f84-12)
		    )
	     )
	    '(moral ((men (obj-men) true m84-1)
		     (enemies-of (obj-enemies obj-men) true m84-2)
		     (protect-from (obj-men obj-men obj-enemies) true m84-3)
		     (desire (obj-men (m84-3 true)) true m84-4)
		     (friends-of (obj-friends obj-men) true m84-5)
		     (dangerous (obj-friends) true m84-6)
		     (worse-than (obj-friends obj-enemies) true m84-7)
		     (run-to (obj-men obj-friends) true m84-8)
		     (cause (m84-4 m84-8) true m84-9)
		    )
             )
))

; FABLE 85:  The Law of Self-Preservation
; COMMENTS:  Let's see if I've got this straight:  If a slave's desire for the
;            blessings of his master causes him to neglect his relatives, it
;            is not justified for him to be blamed.  Yup...

(defun make_fable85 nil
  (make_struc 'fable85 'fable
	    '(story ((man (obj-fowler) true f85-1)
		     (pigeons (obj-pigeons) true f85-2)
		     (keep (obj-fowler obj-pigeons) true f85-3)
		     (net (obj-net) true f85-4)
		     (tie-to (obj-fowler obj-pigeons obj-net) true f85-5)
		     (pigeons (obj-wild-pigeons) true f85-6)
		     (wild (obj-wild-pigeons) true f85-7)
		     (fly-to (obj-wild-pigeons obj-net) true f85-8)
		     (cause (f85-5 f85-8) true f85-9)
		     (warn (obj-pigeons obj-wild-pigeons) false f85-10)
		     (entangle (obj-net obj-wild-pigeons) true f85-11)
		     (conjoin-event (f85-8 f85-10) true f85-12)
		     (cause (f85-12 f85-11) true f85-13)
		     (catch (obj-fowler obj-wild-pigeons) true f85-14)
		     (reproach-for (obj-wild-pigeons obj-pigeons f85-10) true f85-15)
		     (offend (obj-pigeons obj-fowler) false f85-16)
		     (prefer-to (obj-pigeons f85-10 (f85-16 true)) true f85-17)
		    )
	     )
	    '(moral ((slaves (obj-slaves) true m85-1)
		     (relatives-of (obj-relatives obj-slaves) true m85-2)
		     (neglect (obj-slaves obj-relatives) unknown m85-3)
		     (master-of (obj-master obj-slaves) true m85-4)
		     (blessings-of (obj-blessings obj-master) true m85-5)
		     (desire (obj-slaves obj-blessings) true m85-6)
		     (blame (obj-society obj-slaves) unknown m85-7)
		     (justified ((m85-7 true)) unknown m85-8)
		     (cause (m85-6 m85-3) unknown m85-9)
		     (if ((m85-9 true) (m85-8 false)) true m85-10)
		    )
             )
))

; FABLE 86:  Look Before You Leap II [The Sequel]
; COMMENTS:  See also #7.  HOW ON EARTH do you represent a "picture of"
;            something?????

(defun make_fable86 nil
  (make_struc 'fable86 'fable
	    '(story ((dove (obj-dove) true f86-1)
		     (thirsty (obj-dove) true f86-2)
		     (jug (obj-jug) true f86-3)
		     (water (obj-water) true f86-4)
		     (inside (obj-water obj-jug) true f86-5)
		     (picture-of (obj-picture obj-jug) true f86-6)
		     (real (obj-jug) false f86-7)
		     (believe (obj-dove (f86-7 true)) true f86-8)
		     (fly-at (obj-dove obj-picture) true f86-9)
		     (injure (obj-dove obj-dove) true f86-10)
		     (conjoin-event (f86-7 f86-9) true f86-11)
		     (cause (f86-11 f86-10) true f86-12)
		     (person (obj-passerby) true f86-13)
		     (catch (obj-passerby obj-dove) true f86-14)
		     (cause (f86-10 f86-14) true f86-15)
		    )
	     )
	    '(moral ((things (obj-things) true m86-1)
		     (go-for (obj-you obj-things) true m86-2)
		     (plan (obj-plan) true m86-3)
		     (without (obj-you obj-plan) true m86-4)
		     (bad (m86-2) true m86-5)
		     (cause (m86-4 m86-5) true m86-6)
		     (men (obj-men) true m86-7)
		     (passions-of (obj-passions obj-men) true m86-8)
		     (destruction (conc-destruction) true m86-9)
		     (rush-into (obj-men conc-destruction) true m86-10)
		     (cause (obj-passions m86-10) true m86-11)
		     (occurs-sometimes (m86-11) true m86-12)
		    )
             )
))

; FABLE 87:  Born To Trouble
; COMMENTS:  Confused....

(defun make_fable87 nil
  (make_struc 'fable87 'fable
	    '(story ((pigeon (obj-pigeon) true f87-1)
		     (pigeonry (obj-pigeonry) true f87-2)
		     (kept-in (obj-pigeon obj-pigeonry) true f87-3)
		     (families-of (obj-families obj-pigeon) true f87-4)
		     (large (obj-families) true f87-5)
		     (brag-about (obj-pigeon f87-5) true f87-6)
		     (crow (obj-crow) true f87-7)
		     (overhear (obj-crow f87-6) true f87-8)
		     (captive (obj-families) true f87-9)
		     (wretched (obj-families) true f87-10)
		     (cause (f87-3 f87-9) true f87-11)
		     (cause (f87-9 f87-10) true f87-12)
		     (say-to (obj-crow obj-pigeon f87-9) true f87-13)
		     (say-to (obj-crow obj-pigeon f87-10) true f87-14)
		    )
	     )
	    '(moral ((men (obj-slaves) true m87-1)
		     (slaves (obj-slaves) true m87-2)
		     (miserable (obj-slaves) true m87-3)
		     (most (m87-3) unknown m87-4)
		     (children (obj-children) true m87-5)
		     (beget (obj-slaves obj-children) unknown m87-6)
		     (if ((m87-6 true) (m87-4 true)) true m87-7)
		    )
             )
))

; FABLE 88:  Traitor's Death
; COMMENTS:  Another one?

(defun make_fable88 nil
  (make_struc 'fable88 'fable
	    '(story ((guest (obj-guest) true f88-1)
		     (birdcatcher (obj-birdcatcher) true f88-2)
		     (house-of (obj-house obj-birdcatcher) true f88-3)
		     (arrive-at (obj-guest obj-house) true f88-4)
		     (partridge (obj-partridge) true f88-5)
		     (tame (obj-partridge) true f88-6)
		     (fetch (obj-birdcatcher obj-partridge) true f88-7)
		     (intend-to (obj-birdcatcher (f88-16 true)) true f88-8)
		     (birds (obj-birds) true f88-9)
		     (nets (obj-nets) true f88-10)
		     (bring-to (obj-partridge obj-birds obj-nets) true f88-11)
		     (betray (obj-partridge obj-birds) true f88-12)
		     (cause (f88-11 f88-12) true f88-13)
		     (reproach-for (obj-partridge obj-birdcatcher f88-8) true f88-14)
		     (say-to (obj-birdcatcher f88-12 obj-partridge) true f88-15)
		     (serve-to (obj-birdcatcher obj-partridge obj-guest) true f88-16)
		     (cause (f88-12 f88-16) true f88-17)
		    )
	     )
	    '(moral ((friends-of (obj-friends obj-victims) true m88-1)
		     (treacherous (obj-friends) true m88-2)
		     (victims-of (obj-victims obj-friends) true m88-3)
		     (betray-to (obj-friends obj-victims obj-those) true m88-4)
		     (cause (m88-4 m88-3) true m88-5)
		     (hateful-to (obj-friends obj-victims) true m88-6)
		     (hateful-to (obj-friends obj-those) true m88-7)
		     (cause (m88-4 m88-6) true m88-8)
		     (cause (m88-4 m88-7) true m88-9)
		    )
             )
))

; FABLE 89:  Cherishing a Viper
; COMMENTS:  Another one?

(defun make_fable89 nil
  (make_struc 'fable89 'fable
	    '(story ((hen (obj-hen) true f89-1)
		     (serpent (obj-serpent) true f89-2)
		     (eggs-of (obj-eggs obj-serpent) true f89-3)
		     (hatch-into (obj-eggs obj-baby-serpents) true f89-4)
		     (cause (obj-hen f89-4) true f89-5)
		     (swallow (obj-swallow) true f89-6)
		     (fool (obj-hen) true f89-7)
		     (evil (obj-baby-serpents) true f89-8)
		     (harm (obj-baby-serpents obj-hen) false f89-9)
		     (grow-up (obj-baby-serpents) true f89-10)
		     (become-true (f89-9) true f89-11)
		     (after (f89-10 f89-11) true f89-12)
		     (cause (f89-12 f89-7) true f89-13)
		     (say-to (obj-swallow f89-7 obj-hen) true f89-14)
		     (say-to (obj-swallow f89-12 obj-hen) true f89-15)
		    )
	     )
	    '(moral ((treatment (conc-treatment) true m89-1)
		     (kindest (conc-treatment) true m89-2)
		     (nature (conc-nature) true m89-3)
		     (savage (conc-nature) true m89-4)
		     (make-tame (conc-treatment conc-nature) false m89-5)
		     (impossible ((m89-5 true)) true m89-6)
		    )
             )
))

; FABLE 90:  The Punishment of Selfishness 

(defun make_fable90 nil
  (make_struc 'fable90 'fable
	    '(story ((horse (obj-horse) true f90-1)
		     (ass (obj-ass) true f90-2)
		     (master-of (obj-master obj-horse) true f90-3)
		     (master-of (obj-master obj-ass) true f90-4)
		     (load-of (obj-load obj-ass) true f90-5)
		     (request (obj-ass (f90-7 true)) true f90-6)
		     (take-from (obj-horse obj-load obj-ass) false f90-7)
		     (die (obj-ass) true f90-8)
		     (cause (f90-7 f90-8) true f90-9)
		     (give-to (obj-master obj-load obj-horse) true f90-10)
		     (cause (f90-8 f90-10) true f90-11)
		     (carry (obj-horse obj-load) true f90-12)
		     (cause (f90-10 f90-12) true f90-13)
		     (carry (obj-horse obj-ass) true f90-14)
		     (cause (f90-8 f90-14) true f90-15)
		    )
	     )
	    '(moral ((strong (obj-strong) true m90-1)
		     (weak (obj-weak) true m90-2)
		     (live (obj-weak) unknown m90-3)
		     (live (obj-strong) unknown m90-4)
		     (if ((m90-3 true) (m90-4 true)) true m90-5)
		     (help (obj-strong obj-weak) unknown m90-6)
		     (if ((m90-6 true) (m90-3 true)) true m90-7)
		    )
             )
))

; FABLE 91:  Save Us in the Time of Trouble
; COMMENTS:  The moral, in English, says:  If when the time offers security
;            you relax, it is not allowed that you forget the time that did
;            not offer security.

(defun make_fable91 nil
  (make_struc 'fable91 'fable
	    '(story ((soldier (obj-soldier) true f91-1)
		     (horse (obj-horse) true f91-2)
		     (belong-to (obj-horse obj-soldier) true f91-3)
		     (war (obj-war) true f91-4)
		     (at (obj-soldier obj-war) unknown f91-5)
		     (adventures (obj-adventures) true f91-6)
		     (share-with (obj-soldier obj-adventures obj-horse) true f91-7)
		     (barley (obj-barley) true f91-8)
		     (feed (obj-soldier obj-horse obj-barley) true f91-9)
		     (while (f91-5 f91-7) true f91-10)
		     (while (f91-5 f91-9) true f91-11)
		     (slave (obj-horse) true f91-12)
		     (chaff (obj-chaff) true f91-13)
		     (feed (obj-soldier obj-horse obj-chaff) true f91-14)
		     (after (f91-5 f91-12) true f91-15)
		     (after (f91-5 f91-14) true f91-16)
		     (war (obj-new-war) true f91-17)
		     (fail (obj-horse obj-soldier) true f91-18)
		     (conjoin-event (f91-12 f91-14) true f91-19)
		     (cause (f91-19 f91-18) true f91-20)
		    )
	     )
	    '(moral ((security (conc-security) true m91-1)
		     (time (obj-day-of-affliction) true m91-2)
		     (offer (obj-day-of-affliction conc-security) false m91-3)
		     (time (obj-time-of-security) true m91-4)
		     (offer (obj-time-of-security conc-security) true m91-5)
		     (before (obj-day-of-affliction obj-time-of-security) true m91-6)
		     (relax (obj-you) unknown m91-7)
		     (when (obj-time-of-security (m91-7 true)) unknown m91-8)
		     (forget (obj-you obj-day-of-affliction) unknown m91-9)
		     (allowed ((m91-9 true)) unknown m91-10)
		     (if ((m91-8 true) (m91-10 false)) true m91-11)
		    )
             )
))

; FABLE 92:  A Bad Bargain
; COMMENTS:  I'm really screwing around with propositions vs. objects here,
;            but how else do you say that blind anger does such and such?

(defun make_fable92 nil
  (make_struc 'fable92 'fable
	    '(story ((boar (obj-boar) true f92-1)
		     (wild (obj-boar) true f92-2)
		     (horse (obj-horse) true f92-3)
		     (grass (obj-grass) true f92-4)
		     (water (obj-water) true f92-5)
		     (spoil (obj-boar obj-grass) true f92-6)
		     (muddy (obj-boar obj-water) true f92-7)
		     (huntsman (obj-huntsman) true f92-8)
		     (search-for (obj-horse obj-huntsman) true f92-9)
		     (bridle (obj-bridle) true f92-10)
		     (put-upon (obj-huntsman obj-bridle obj-horse) true f92-11)
		     (allow (obj-horse f92-11) true f92-12)
		     (desire (obj-horse (f92-15 true)) true f92-13)
		     (cause (f92-13 f92-12) true f92-14)
		     (kill (obj-huntsman obj-boar) true f92-15)
		     (manger (obj-manger) true f92-16)
		     (tie-to (obj-huntsman obj-horse obj-manger) true f92-17)
		    )
	     )
	    '(moral ((anger (conc-blind-anger) true m92-1)
		     (blind (conc-blind-anger) true m92-2)
		     (people (obj-people) true m92-3)
		     (enemies-of (obj-enemies obj-people) true m92-4)
		     (revenge-on (conc-revenge obj-enemies) true m92-5)
		     (desire (obj-people conc-revenge) true m92-6)
		     (power-of (conc-power obj-someone-else) true m92-7)
		     (place-in (obj-people obj-people conc-power) true m92-8)
		     (conjoin-event (conc-blind-anger m92-6) true m92-9)
		     (cause (m92-9 m92-8) true m92-10)
		    )
             )
))

; FABLE 93:  Feline Sophistry
; COMMENTS:  I'm not really sure which way to go on the moral here.  It is
;            ALMOST delineated.  I think I'll enter it.  See also #28.

(defun make_fable93 nil
  (make_struc 'fable93 'fable
	    '(story ((cat (obj-cat) true f93-1)
		     (cock (obj-cock) true f93-2)
		     (men (obj-men) true f93-3)
		     (excuse-for (obj-excuse (f93-16 true)) true f93-4)
		     (nuisance-to (obj-cock obj-men) false f93-5)
		     (lie-to (obj-cat f93-5 obj-cock) true f93-6)
		     (wake-up (obj-cock obj-men) true f93-7)
		     (good (f93-7) true f93-8)
		     (say-to (obj-cock f93-8 obj-cat) true f93-9)
		     (incest (conc-incest) true f93-10)
		     (commit (obj-cock conc-incest) true f93-11)
		     (good (f93-11) true f93-12)
		     (lie-to (obj-cat f93-12 obj-cock) true f93-13)
		     (say-to (obj-cock f93-12 obj-cat) true f93-14)
		     (justified (f93-16) false f93-15)
		     (eat (obj-cat obj-cock) true f93-16)
		     (even-though (f93-15 f93-16) true f93-17)
		    )
	     )
	    '(moral ((nature (conc-nature) true m93-1)
		     (evil (conc-nature) true m93-2)
		     (wrongdoing (conc-wrongdoing) true m93-3)
		     (bent-on (conc-nature conc-wrongdoing) unknown m93-4)
		     (pretext (obj-pretext) true m93-5)
		     (fair (obj-pretext) true m93-6)
		     (have (conc-nature obj-pretext) unknown m93-7)
		     (if ((m93-7 false) (m93-4 true)) true m93-8)
		     (if ((m93-7 true) (m93-4 true)) true m93-9)
		    )
             )
))





; FILE:       /u2/thnet/data/arcs/fables40
; PURPOSE:    test retrieval using fables (numbers 33-42)
; PROGRAMMER: Greg Nelson
; CREATED:    6-16-88
; UPDATED:    7-21-88 - minor changes to morals and "UNKNOWN" added
; UPDATED:    7-22-88 - "DESIRE" -> "DESIRE-TRUE"
; UPDATED:    7-25-88 - Added hypotheticals.

; REJECT LIST 32

(defun lfa40 () (load "//tinman/ucla/psych/emelz/ARCS/fables40"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable33 fable34 fable35 fable36 fable37 fable38 fable39 fable40 fable41 fable42)))

; FABLE 32:  Misplaced Confidence
; REJECTED -- This seems too temporally dependant to make it reasonable.

; FABLE 33:  Born Plunderers

(defun make_fable33 nil
  (make_struc 'fable33 'fable
	    '(story ((shepherd (obj-shepherd) true f33-1)
		     (cubs (obj-wolves) true f33-2)
		     (wolves (obj-wolves) true f33-3)
		     (sheep (obj-sheep) true f33-4)
		     (find (obj-shepherd obj-wolves) true f33-5)
		     (rear (obj-shepherd obj-wolves) true f33-6)
		     (hope (obj-shepherd (f33-8 true)) true f33-7)
		     (guard (obj-wolves obj-sheep) false f33-8)
		     (attack (obj-wolves obj-sheep) true f33-9)
		     (kill (obj-shepherd obj-wolves) true f33-10)
		     (necessary (f33-10) true f33-11)
		     (senseless (f33-6) true f33-12)
		     (cause (f33-2 f33-6) true f33-13)
		     (cause (f33-3 f33-9) true f33-14)
		     (cause (f33-9 f33-11) true f33-15)
		     (cause (f33-11 f33-12) true f33-16)
		    )
	     )
	    '(moral ((man (obj-bad-man) true m33-1)
		     (bad (obj-bad-man) true m33-2)
		     (death (conc-death) true m33-3)
		     (save-from (obj-you obj-bad-man conc-death) unknown m33-4)
		     (victim-of (obj-you obj-bad-man) unknown m33-5)
		     (powerful (obj-bad-man) unknown m33-6)
		     (if ((m33-4 true) (m33-6 true)) true m33-7)
		     (conjoin-event (m33-2 m33-6) true m33-8)
		     (if ((m33-8 true) (m33-5 true)) true m33-9)
		    )
             )
))

; FABLE 34:  Trying To Make A Silk Purse Out Of A Sow's Ear
; COMMENTS:  Almost exactly like fable 33.  Hope it retrieves.  The catch is
;            conditional on the combination of whether the other wolf actually
;            steals a sheep, whether the chase ensues, and some matters of
;            chance.  Therefore, I put it as conditional with no direct causes.

(defun make_fable34 nil
  (make_struc 'fable34 'fable
	    '(story ((shepherd (obj-shepherd) true f34-1)
		     (wolf (obj-wolf) true f34-2)
		     (cub (obj-wolf) true f34-3)
		     (dogs (obj-dogs) true f34-4)
		     (sheep (obj-sheep) true f34-5)
		     (wolf (obj-other-wolf) true f34-6)
		     (raise (obj-shepherd obj-dogs) true f34-7)
		     (raise (obj-shepherd obj-wolf) true f34-8)
		     (steal (obj-other-wolf obj-sheep) unknown f34-9)
		     (chase (obj-wolf obj-other-wolf) unknown f34-10)
		     (if ((f34-9 true) (f34-10 true)) true f34-11)
		     (catch (obj-dogs obj-other-wolf) unknown f34-12)
		     (share-with (obj-wolf obj-sheep obj-other-wolf) unknown f34-13)
		     (if ((f34-12 false) (f34-13 true)) true f34-14)
		     (kill (obj-wolf obj-sheep) unknown f34-15)
		     (eat (obj-wolf obj-sheep) unknown f34-16)
		     (if ((f34-15 true) (f34-16 true)) true f34-17)
		     (conjoin-event (f34-13 f34-15) true f34-18)
		     (discover (obj-shepherd f34-18) true f34-19)
		     (hang (obj-shepherd obj-wolf) true f34-20)
		     (cause (f34-19 f34-20) true f34-21)
		    )
	     )
	    '(moral ((man (obj-man) true m34-1)
		     (vicious (obj-man) true m34-2)
		     (good (obj-man) false m34-3)
		     (cause (m34-2 m34-3) true m34-4)
		    )
             )
))

; FABLE 35:  Delusion
; No moral.

(defun make_fable35 nil
  (make_struc 'fable35 'fable
	    '(story ((wolf (obj-wolf) true f35-1)
		     (shadow (obj-shadow) true f35-2)
		     (lion (obj-lion) true f35-3)
		     (sun (obj-sun) true f35-4)
		     (low (obj-sun) true f35-5)
		     (big (obj-shadow) true f35-6)
		     (cause (f35-5 f35-6) true f35-7)
		     (big (obj-wolf) false f35-8)
		     (believe (obj-wolf (f35-8 true)) true f35-9)
		     (cause (f35-6 f35-9) true f35-10)
		     (boast (obj-wolf) true f35-11)
		     (catch (obj-lion obj-wolf) true f35-12)
		     (cause (f35-11 f35-12) true f35-13)
		     (realize (obj-wolf f35-13) true f35-14)
		     (devour (obj-lion obj-wolf) true f35-15)
		    )
	     )
))

; FABLE 36:  A Case of Mistaken Identity
; COMMENTS:  This looks like it should be titled "A Wolf in Sheep's Clothing"
;            The predicate occurs-often is used to represent that "such play
;            acting has cost many a man his life."

(defun make_fable36 nil
  (make_struc 'fable36 'fable
	    '(story ((wolf (obj-wolf) true f36-1)
		     (shepherd (obj-shepherd) true f36-2)
		     (flock (obj-sheep) true f36-3)
		     (sheep (obj-sheep) true f36-4)
		     (sheepskin (obj-sheepskin) true f36-5)
		     (wear (obj-wolf obj-sheepskin) true f36-6)
		     (eat (obj-wolf obj-sheep) false f36-7)
		     (desire (obj-wolf (f36-7 true)) true f36-8)
		     (cause (f36-8 f36-6) true f36-9)
		     (discover (obj-shepherd f36-6) false f36-10)
		     (kill (obj-shepherd obj-wolf) true f36-11)
		     (sheep (obj-wolf) false f36-12)
		     (believe (obj-shepherd (f36-12 true)) true f36-13)
		     (cause (f36-6 f36-13) true f36-14)
		     (cause (f36-13 f36-11) true f36-15)
		    )
	     )
	    '(moral ((man (obj-man) true m36-1)
		     (trait (obj-trait) true m36-2)
		     (belongs-to (obj-trait obj-man) false m36-3)
		     (pretend (obj-man (m36-3 true)) true m36-4)
		     (trouble (conc-trouble) true m36-5)
		     (have (obj-man conc-trouble) true m36-6)
		     (cause (m36-4 m35-6) true m36-7)
		     (die (obj-man) true m36-8)
		     (cause (m36-6 m39-8) true m36-9)
		     (occurs-often (m36-9) true m36-10)
		    )
             )
))

; FABLE 37:  Second Thoughts
; COMMENTS:  This reminds me of a fable which I couldn't recall immediately,
;            but which I looked up and found was "A Companion In Fear", number
;            22.  It will be interesting to see if the match occurs to ARCS,
;            since I haven't looked at that story or representation for a week.

(defun make_fable37 nil
  (make_struc 'fable37 'fable
	    '(story ((hares (obj-hares) true f37-1)
		     (frogs (obj-frogs) true f37-2)
		     (pool (obj-pool) true f37-3)
		     (convene (obj-hares) true f37-4)
		     (fear (obj-hares obj-all-humans) true f37-5)
		     (fear (obj-hares obj-all-dogs) true f37-6)
		     (conjoin-event (f37-5 f37-6) true f37-7)
		     (desire (obj-hares (f37-10 true)) true f37-8)
		     (cause (f37-7 f37-8) true f37-9)
		     (kill (obj-hares obj-hares) false f37-10)
		     (run-to (obj-hares obj-pool) true f37-11)
		     (near (obj-frogs obj-pool) true f37-12)
		     (fear (obj-frogs obj-hares) true f37-13)
		     (cause (f37-11 f37-13) true f37-14)
		     (jump-in (obj-frogs obj-pool) true f37-15)
		     (cause (f37-13 f37-15) true f37-16)
		     (see (obj-hares f37-15) true f37-17)
		     (worse (f37-13 f37-7) true f37-18)
		     (realize (obj-hares f37-18) true f37-19)
		     (cause (f37-19 f37-10) true f37-20)
		    )
	     )
	    '(moral ((wretched (obj-wretched) true m37-1)
		     (wretched (obj-worse) true m37-2)
		     (worse (obj-worse obj-wretched) true m37-3)
		     (see (obj-wretched m37-3) true m37-4)
		     (comfort (m37-4 obj-wretched) true m37-5)
		    )
             )
))

; FABLE 38:  Ready For Action
; COMMENTS:  The moral is more detailed than the version in the fable, which
;            requires a direct command.  This might be represented as:
;            (actor (obj-actor) true m38-1)
;            (danger (conc-danger) true m38-2)
;            (near (conc-danger obj-actor) true m38-3)
;            (prepare-for (obj-actor conc-danger) true m38-4)
;            (command-to-listener ((m38-4 false)) true m38-5)

(defun make_fable38 nil
  (make_struc 'fable38 'fable
	    '(story ((boar (obj-boar) true f38-1)
		     (tusks-of (obj-tusks obj-boar) true f38-2)
		     (fox (obj-fox) true f38-3)
		     (hunter (obj-huntsman) true f38-4)
		     (reason (obj-reason) true f38-5)
		     (sharpen (obj-boar obj-tusks) true f38-6)
		     (chase (obj-huntsman obj-boar) false f38-7)
		     (cause (obj-reason f38-6) true f38-8)
		     (ask (obj-fox obj-boar obj-reason) true f38-9)
		     (if ((f38-7 true) (f38-6 false)) true f38-10)
		     (say (obj-boar f38-10 obj-fox) true f38-11)
		    )
	     )
	    '(moral ((danger (conc-danger) true m38-1)
		     (near (conc-danger obj-actor) unknown m38-2)
		     (prepared-for (obj-actor conc-danger) unknown m38-3)
		     (prepare-for (obj-actor conc-danger) unknown m38-4)
		     (if ((m38-4 true) (m38-3 true)) true m38-5)
		     (impossible ((m38-4 true)) unknown m38-6)
		     (wait-until (obj-actor (m38-2 true)) unknown m38-7)
		     (if ((m38-7 true) (m38-6 true)) true m38-8)
		     (bad ((m38-7 true)) true m38-9)
		     (cause (m38-8 m38-9) true m38-10)
		    )
             )
))

; FABLE 39:  As Good As His Word

(defun make_fable39 nil
  (make_struc 'fable39 'fable
	    '(story ((mouse (obj-mouse) true f39-1)
		     (lion (obj-lion) true f39-2)
		     (hunters (obj-hunters) true f39-3)
		     (run-across (obj-mouse obj-lion) true f39-4)
		     (wake-up (f39-4 obj-lion) true f39-5)
		     (desire (obj-lion (f39-8 true)) true f39-6)
		     (beg-that (obj-mouse obj-lion (f39-8 false)) true f39-7)
		     (eat (obj-lion obj-mouse) false f39-8)
		     (cause (f39-7 f39-8) true f39-9)
		     (promise (obj-mouse (f39-14 true) obj-lion) true f39-10)
		     (cause (f39-8 f39-10) true f39-11)
		     (capture (obj-hunters obj-lion) true f39-12)
		     (need (obj-lion (f39-14 true)) true f39-13)
		     (help (obj-mouse obj-lion) true f39-14)
		     (alive (obj-lion) true f39-15)
		     (conjoin-event (f39-10 f39-13) true f39-16)
		     (cause (f39-16 f39-14) true f39-17)
		     (cause (f39-14 f39-15) true f39-18)
		    )
	     )
	    '(moral ((man (obj-strong) true m39-1)
		     (man (obj-weak) true m39-2)
		     (weaker (obj-weak obj-strong) true m39-3)
		     (unlucky (obj-strong) unknown m39-4)
		     (help (obj-weak obj-strong) unknown m39-5)
		     (need (obj-strong (m39-5 true)) unknown m39-6)
		     (if ((m39-4 true) (m39-6 true)) true m39-7)
		    )
             )
))

; FABLE 40:  Pride Will Have A Fall

(defun make_fable40 nil
  (make_struc 'fable40 'fable
	    '(story ((mice (obj-mice) true f40-1)
		     (weasels (obj-weasels) true f40-2)
		     (at-war-with (obj-mice obj-weasels) true f40-3)
		     (battles (obj-mice obj-weasels obj-battles) true f40-4)
		     (win (obj-weasels obj-battles) true f40-5)
		     (leaders-of (obj-mouse-leaders obj-mice) true f40-6)
		     (need (obj-mice obj-mouse-leaders) true f40-7)
		     (cause (f40-7 f40-5) true f40-8)
		     (realize (obj-mice f40-7) true f40-9)
		     (elect (obj-mice obj-mouse-leaders) true f40-10)
		     (generals (obj-mouse-leaders) true f40-11)
		     (horns (obj-horns) true f40-12)
		     (wear (obj-mouse-leaders obj-horns) true f40-13)
		     (distinguish (obj-horns obj-mouse-leaders obj-mice) true f40-14)
		     (desire (obj-mouse-leaders f40-14) true f40-15)
		     (battle (obj-mice obj-weasels obj-new-battle) true f40-16)
		     (win (obj-weasels obj-new-battle) true f40-17)
		     (escape-from (obj-mice obj-weasels) true f40-18)
		     (escape-from (obj-mouse-leaders obj-weasels) false f40-19)
		     (cause (f40-13 f40-19) true f40-20)
		     (cause (f40-9 f40-10) true f40-21)
		     (cause (f40-10 f40-11) true f40-22)
		     (cause (f40-15 f40-13) true f40-23)
		    )
	     )
	    '(moral ((vain (obj-actor) true m40-1)
		     (misfortune (conc-misfortune) true m40-2)
		     (befall (conc-misfortune obj-actor) true m40-3)
		     (cause (m40-1 m40-3) true m40-4)
		     (occurs-often (m40-4) true m40-5)
		    )
             )
))

; FABLE 41:  Town Mouse And Country Mouse
; COMMENTS:  This is going to be LONG!!!

(defun make_fable41 nil
  (make_struc 'fable41 'fable
	    '(story ((mouse (obj-country-mouse) true f41-1)
		     (mouse (obj-town-mouse) true f41-2)
		     (home (obj-country-mouse obj-country) true f41-3)
		     (home (obj-town-mouse obj-town) true f41-4)
		     (friends (obj-town-mouse obj-country-mouse) true f41-5)
		     (invite (obj-country-mouse obj-town-mouse obj-country) true f41-6)
		     (food (obj-country-food) true f41-7)
		     (offer (obj-country-mouse obj-country-food obj-town-mouse) true f41-8)
		     (simple (obj-country-food) true f41-9)
		     (want-from (obj-town-mouse obj-country-food obj-country-mouse) false f41-10)
		     (cause (f41-9 f41-10) true f41-11)
		     (cause (f41-10 f41-13) true f41-12)
		     (invite (obj-town-mouse obj-country-mouse obj-town) true f41-13)
		     (food (obj-town-food) true f41-14)
		     (better (obj-town-food obj-country-food) true f41-15)
		     (want-from (obj-country-mouse obj-town-food obj-town-mouse) true f41-16)
		     (scare (obj-town obj-country-mouse) true f41-17)
		     (leave (obj-country-mouse obj-town) true f41-18)
		     (cause (f41-20 f41-18) true f41-19)
		     (prefer-to (obj-country-mouse obj-country obj-town) true f41-20)
		     (prefer-to (obj-country-mouse obj-country-food obj-town-food) true f41-21)
		     (cause (f41-20 f41-21) true f41-22)
		    )
	     )
	    '(moral ((peace (obj-simple-peace) true m41-1)
		     (simple (obj-simple-peace) true m41-2)
		     (luxurious (obj-luxurious-fear) true m41-3)
		     (fearful (obj-luxurious-fear) true m41-4)
		     (better (obj-simple-peace obj-luxurious-fear) true m41-5)
		    )
             )
))

; FABLE 42:  We Get The Rulers We Deserve

(defun make_fable42 nil
  (make_struc 'fable42 'fable
	    '(story ((frogs (obj-frogs) true f42-1)
		     (ruler-of (obj-ruler obj-frogs) true f42-2)
		     (god (obj-Zeus) true f42-3)
		     (have (obj-frogs obj-ruler) unknown f42-4)
		     (desire (obj-frogs (f42-4 true)) true f42-5)
		     (request (obj-frogs obj-ruler obj-Zeus) true f42-6)
		     (drop-in (obj-Zeus obj-wood obj-pond) true f42-7)
		     (rule (obj-wood obj-frogs) true f42-8)
		     (cause (f42-8 f42-4) true f42-9)
		     (insulted-by (obj-frogs f42-8) true f42-10)
		     (request (obj-frogs obj-ruler obj-Zeus) true f42-11)
		     (impatient-with (obj-Zeus obj-frogs) true f42-12)
		     (cause (f42-11 f42-12) true f42-13)
		     (send-to (obj-Zeus obj-snake obj-pond) true f42-14)
		     (cause (f42-12 f42-14) true f42-15)
		     (rule (obj-snake obj-frogs) true f42-16)
		     (devour (obj-snake obj-frogs) true f42-17)
		    )
	     )
	    '(moral ((ruler-of (obj-harmless-ruler obj-us) unknown m42-1)
		     (ruler-of (obj-harmful-ruler obj-us) unknown m42-2)
		     (mutually-exclusive ((m42-1 true) (m42-2 true)) true m42-3)
		     (harmless (obj-harmless-ruler) true m42-4)
		     (tyrannical (obj-harmful-ruler) true m42-5)
		     (conjoin-event (m42-4 m42-5) true m42-6)
		     (better ((m42-1 true) (m42-2 true)) true m42-7)
		     (cause (m42-6 m42-7) true m42-8)
		    )
             )
))




; FILE:       /u2/thnet/data/arcs/fables80
; PURPOSE:    test retrieval using fables (numbers 74-83)
; PROGRAMMER: Greg Nelson
; CREATED:    7-24-88
; UPDATED:    7-25-88 - Added hypotheticals.

(defun lfa80 () (load "//tinman/ucla/psych/emelz/ARCS/fables80"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable74 fable75 fable76 fable77 fable78 fable79 fable80 fable81 fable82 fable83)))

; FABLE 74:  Breach of Promise

(defun make_fable74 nil
  (make_struc 'fable74 'fable
	    '(story ((crow (obj-crow) true f74-1)
		     (snare (obj-snare) true f74-2)
		     (caught-in (obj-crow obj-snare) true f74-3)
		     (god (obj-apollo) true f74-4)
		     (incense (obj-incense) true f74-5)
		     (promise-to (obj-crow obj-apollo obj-incense) true f74-6)
		     (release-from (obj-apollo obj-crow obj-snare) true f74-7)
		     (cause (f74-6 f74-7) true f74-8)
		     (forget-that (obj-crow f74-6) true f74-9)
		     (give-to (obj-crow obj-incense obj-apollo) false f74-10)
		     (god (obj-hermes) true f74-11)
		     (catch (obj-new-trap obj-crow) true f74-12)
		     (sacrifice (obj-sacrifice) true f74-13)
		     (promise-to (obj-crow obj-hermes obj-sacrifice) true f74-14)
		     (release-from (obj-hermes obj-crow obj-new-trap) false f74-15)
		     (cause (f74-10 f74-15) true f74-16)
		    )
	     )
	    '(moral ((benefactor (obj-benefactor) true m74-1)
		     (grateful-to (obj-ingrate obj-benefactor) false m74-2)
		     (help (conc-help) true m74-3)
		     (need (obj-ingrate conc-help) true m74-4)
		     (give-to (obj-others conc-help obj-ingrate) false m74-5)
		    )
             )
))

; FABLE 75:  Right Of Asylum
; COMMENTS:  HUH?  WHAT?  What does mistletoe have to do with catching birds,
;            anyway?  And it doesn't really prefer oaks...
;            This might be a little like #11.

(defun make_fable75 nil
  (make_struc 'fable75 'fable
	    '(story ((mistletoe (obj-mistletoe) true f75-1)
		     (exist (obj-mistletoe) true f75-2)
		     (danger (obj-mistletoe) true f75-3)
		     (cause (f75-2 f75-3) true f75-4)
		     (martin (obj-martin) true f75-5)
		     (birds (obj-birds) true f75-6)
		     (threaten (obj-mistletoe obj-birds) true f75-7)
		     (realize-that (obj-martin f75-7) true f75-8)
		     (tell (obj-martin obj-birds f75-7) true f75-9)
		     (tear-down (obj-birds obj-mistletoe) unknown f75-10)
		     (request-of (obj-martin (f75-10 true) obj-birds) true f75-11)
		     (refuse-to (obj-birds (f75-10 true)) true f75-12)
		     (become-false (f75-10) true f75-13)
		     (cause (f75-12 f75-13) true f75-14)
		     (ridicule (obj-birds obj-martin) true f75-15)
		     (men (obj-men) true f75-16)
		     (beg-that (obj-martin obj-men (f75-19 false)) true f75-17)
		     (welcome (obj-men obj-martin) true f75-18)
		     (harm (obj-men obj-martin) false f75-19)
		     (cause (f75-17 f75-19) true f75-20)
		     (live-with (obj-martin obj-men) true f75-21)
		    )
	     )
	    '(moral ((danger (conc-danger) true m75-1)
		     (forsee (obj-insightful conc-danger) unknown m75-2)
		     (avoid (obj-insightful conc-danger) unknown m75-3)
		     (if ((m75-2 true) (m75-3 true)) true m75-4)
		    )
             )
))

; FABLE 76:  Fireside Sketch

(defun make_fable76 nil
  (make_struc 'fable76 'fable
	    '(story ((man (obj-man) true f76-1)
		     (parrot (obj-parrot) true f76-2)
		     (tame (obj-parrot) true f76-3)
		     (buy (obj-man obj-parrot) true f76-4)
		     (chatter (obj-parrot) true f76-5)
		     (cat (obj-cat) true f76-6)
		     (criticize-for (obj-cat obj-parrot f76-5) true f76-7)
		     (meow (obj-cat) false f76-8)
		     (allow (obj-man (f76-8 true)) false f76-9)
		     (cause (f76-9 f76-8) true f76-10)
		     (cause (f76-9 f76-7) true f76-11)
		     (go-away (obj-cat) false f76-12)
		     (suggest-to (obj-parrot (f76-12 true) obj-cat) true f76-13)
		     (dislike (obj-man f76-5) false f76-14)
		     (say-to (obj-parrot f76-14 obj-cat) true f76-15)
		    )
	     )
	    '(moral ((critic (obj-critic) true m76-1)
		     (ill-natured (obj-critic) true m76-2)
		     (fault-in (obj-fault obj-others) true m76-3)
		     (search-for (obj-critic obj-fault) true m76-4)
		     (occur-always (m76-4) true m76-5)
		     (stupid (obj-critic) true m76-6)
		     (cause (m76-4 m76-6) true m76-7)
		    )
             )
))

; FABLE 77:  Tit for Tat
; COMMENTS:  The moral proceeds the fable, and is not delineated.

(defun make_fable77 nil
  (make_struc 'fable77 'fable
	    '(story ((stork (obj-stork) true f77-1)
		     (dinner (obj-dinner) true f77-2)
		     (fox (obj-fox) true f77-3)
		     (invite-to (obj-fox obj-stork obj-dinner) true f77-4)
		     (slab (obj-slab) true f77-5)
		     (marble (obj-slab) true f77-6)
		     (soup (obj-soup) true f77-7)
		     (clear (obj-soup) true f77-8)
		     (serve-on (obj-fox obj-soup obj-slab) true f77-9)
		     (eat (obj-stork obj-soup) false f77-10)
		     (impossible ((f77-10 true)) true f77-11)
		     (dinner (obj-dinner2) true f77-12)
		     (invite-to (obj-stork obj-fox obj-dinner2) true f77-13)
		     (flagon (obj-flagon) true f77-14)
		     (pap (obj-pap) true f77-15)
		     (filled-with (obj-flagon obj-pap) true f77-16)
		     (drink-from (obj-stork obj-flagon) true f77-17)
		     (drink-from (obj-fox obj-flagon) false f77-18)
		     (impossible ((f77-18 true)) true f77-19)
		     (cause (f77-19 f77-18) true f77-20)
		    )
	     )
	    '(moral ((unkind-to (obj-you obj-someone) unknown m77-1)
		     (provoke (obj-someone obj-you) unknown m77-2)
		     (unjustified ((m77-1 true)) unknown m77-3)
		     (unless ((m77-2 true) (m77-3 true)) true m77-4)
		     (unkind-to (obj-someone obj-you) unknown m77-5)
		     (method-for (obj-method (m77-5 true)) true m77-6)
		     (if ((m77-5 true) (m77-2 true)) true m77-7)
		     (use-for (obj-you obj-method (m77-1 true)) unknown m77-8)
		     (if ((m77-2 true) (m77-8 true)) true m77-9)
		     (if ((m77-2 true) (m77-1 true)) true m77-10)
		    )
             )
))

; FABLE 78:  Nature's Punishment of Discontent
; COMMENTS:  No moral included.

(defun make_fable78 nil
  (make_struc 'fable78 'fable
	    '(story ((kites (obj-kites) true f78-1)
		     (swans (obj-swans) true f78-2)
		     (voice-of (obj-voice-of-kites obj-kites) true f78-3)
		     (voice-of (obj-voice-of-swans obj-swans) true f78-4)
		     (prettier (obj-voice-of-swans obj-voice-of-kites) true f78-5)
		     (was-false (f78-5) true f78-6)
		     (horses (obj-horses) true f78-7)
		     (neigh (obj-horses) true f78-8)
		     (envy (obj-kites obj-horses) true f78-9)
		     (cause (f78-8 f78-9) true f78-10)
		     (imitate (obj-kites obj-horses) true f78-11)
		     (sing (obj-kites) false f78-12)
		     (forget-how (obj-kites (f78-12 true)) true f78-13)
		     (cause (f78-11 f78-13) true f78-14)
		     (cause (f78-13 f78-5) true f78-15)
		     (cause (f78-13 f78-12) true f78-16)
		    )
	     )
))

; FABLE 79:  When A Man Means Business
; COMMENTS:  The moral is embedded in the comments made by the lark.  However,
;            I will stay close to the book and not put in any moral.

(defun make_fable79 nil
  (make_struc 'fable79 'fable
	    '(story ((lark (obj-lark) true f79-1)
		     (children-of (obj-children obj-lark) true f79-2)
		     (nest-of (obj-nest obj-lark) true f79-3)
		     (farmer (obj-farmer) true f79-4)
		     (field-of (obj-field obj-farmer) true f79-5)
		     (in (obj-nest obj-field) true f79-6)
		     (friends-of (obj-friends obj-farmer) true f79-7)
		     (desire (obj-farmer (f79-10 true)) true f79-8)
		     (call-together (obj-farmer obj-friends) true f79-9)
		     (reap (obj-farmer obj-field) false f79-10)
		     (help-with (obj-friends obj-farmer (f79-10 true)) false f79-11)
		     (cause (f79-11 f79-10) true f79-12)
		     (leave (obj-lark obj-field) false f79-13)
		     (know-that (obj-lark f79-10) true f79-14)
		     (cause (f79-9 f79-14) true f79-15)
		     (cause (f79-14 f79-13) true f79-16)
		     (hire (obj-farmer obj-help) true f79-17)
		     (leave (obj-lark obj-field) true f79-18)
		     (reap (obj-farmer obj-field) true f79-19)
		     (know-that (obj-lark f79-19) true f79-20)
		     (cause (f79-17 f79-20) true f79-21)
		     (cause (f79-20 f79-18) true f79-22)
		    )
	     )
))

; FABLE 80:  Swan Song

(defun make_fable80 nil
  (make_struc 'fable80 'fable
	    '(story ((man (obj-man) true f80-1)
		     (swan (obj-swan) true f80-2)
		     (voice-of (obj-voice obj-swan) true f80-3)
		     (beautiful (obj-voice) true f80-4)
		     (buy (obj-man obj-swan) true f80-5)
		     (cause (f80-4 f80-5) true f80-6)
		     (request-of (obj-man (f80-8 true) obj-swan) true f80-7)
		     (sing (obj-swan) false f80-8)
		     (death (conc-death) true f80-9)
		     (near (obj-swan conc-death) true f80-10)
		     (become-true (f80-8) true f80-11)
		     (cause (f80-10 f80-11) true f80-12)
		     (threaten (obj-man obj-swan) false f80-13)
		     (preferable-to ((f80-13 true) f80-7) true f80-14)
		     (cause (f80-12 f80-14) true f80-15)
		    )
	     )
	    '(moral ((act (obj-act) true m80-1)
		     (people (obj-people) true m80-2)
		     (favor-to (obj-act obj-you) true m80-3)
		     (perform (obj-people obj-act) unknown m80-4)
		     (desire (obj-people (m80-4 true)) unknown m80-5)
		     (force-to (obj-you obj-people (m80-4 true)) unknown m80-6)
		     (if ((m80-5 false) (m80-6 true)) unknown m80-7)
		     (occurs-sometimes ((m80-7 true)) true m80-8)
		    )
             )
))

; FABLE 81:  The Victor Vanquished
; COMMENTS:  The crow predicate conflicts here -- the rival is a *rooster, he
;            merely does the *act* of crowing.

(defun make_fable81 nil
  (make_struc 'fable81 'fable
	    '(story ((cock (obj-cock) true f81-1)
		     (cock (obj-rival) true f81-2)
		     (hens (obj-hens) true f81-3)
		     (favor-of (obj-favor obj-hens) true f81-4)
		     (fight-for (obj-cock obj-rival obj-favor) true f81-5)
		     (lose (obj-cock f81-5) true f81-6)
		     (hide (obj-cock) true f81-7)
		     (wall (obj-wall) true f81-8)
		     (climb-on (obj-rival obj-wall) true f81-9)
		     (crow (obj-rival) true f81-10)
		     (eagle (obj-eagle) true f81-11)
		     (catch (obj-eagle obj-rival) true f81-12)
		     (cause (f81-10 f81-12) true f81-13)
		     (safe (obj-cock) true f81-14)
		     (receive (obj-cock obj-favor) true f81-15)
		     (cause (f81-12 f81-15) true f81-16)
		    )
	     )
	    '(moral ((god (obj-god) true m81-1)
		     (proud (obj-proud) true m81-2)
		     (humble (obj-humble) true m81-3)
		     (resist (obj-god obj-proud) true m81-4)
		     (grace (conc-grace) true m81-5)
		     (give-to (obj-god conc-grace obj-humble) true m81-6)
		    )
             )
))

; FABLE 82:  Discretion is the Better Part of Valour
; COMMENTS:  Henry IV, Part 1, perhaps?

(defun make_fable82 nil
  (make_struc 'fable82 'fable
	    '(story ((dog (obj-dog) true f82-1)
		     (cock (obj-cock) true f82-2)
		     (friends (obj-dog obj-cock) true f82-3)
		     (tree (obj-tree) true f82-4)
		     (sleep-in (obj-cock obj-tree) true f82-5)
		     (sleep-under (obj-dog obj-tree) true f82-6)
		     (vixen (obj-vixen) true f82-7)
		     (invite (obj-vixen obj-cock) true f82-8)
		     (tell-to-do (obj-cock obj-vixen (f82-10 true)) true f82-9)
		     (seek (obj-vixen obj-dog) true f82-10)
		     (leap-upon (obj-dog obj-vixen) true f82-11)
		     (devour (obj-dog obj-vixen) true f82-12)
		    )
	     )
	    '(moral ((men (obj-wise-men) true m82-1)
		     (wise (obj-wise-men) true m82-2)
		     (enemy-of (obj-enemy obj-wise-men) true m82-3)
		     (attack (obj-enemy obj-wise-men) true m82-4)
		     (plan-of (obj-plan obj-enemy) true m82-5)
		     (spoil (obj-wise-men obj-plan) true m82-6)
		     (when (m82-4 m82-6) true m82-7)
		     (method-for ((m82-10 true) m82-6) true m82-8)
		     (stronger (obj-stronger obj-wise-men) true m82-9)
		     (send-to (obj-wise-men obj-enemy obj-stronger) true m82-10)
		    )
             )
))

; FABLE 83:  A Different Point of View
; COMMENTS:  How do you represent something like "nothing in [the house] but
;            a cock"?

(defun make_fable83 nil
  (make_struc 'fable83 'fable
	    '(story ((thieves (obj-thieves) true f83-1)
		     (house (obj-house) true f83-2)
		     (break-into (obj-thieves obj-house) true f83-3)
		     (cock (obj-cock) true f83-4)
		     (cock (obj-other-things) false f83-5)
		     (find-in (obj-thieves obj-cock obj-house) true f83-6)
		     (find-in (obj-thieves obj-other-things obj-house) false f83-7)
		     (beg-to (obj-cock obj-thieves (f83-14 false)) true f83-8)
		     (men (obj-men) true f83-9)
		     (wake (obj-cock obj-men) true f83-10)
		     (reason-for (f83-10 (f83-14 false)) true f83-11)
		     (desire (obj-thieves (f83-10 false)) true f83-12)
		     (reason-for (f83-12 (f83-14 true)) true f83-13)
		     (sacrifice (obj-thieves obj-cock) true f83-14)
		    )
	     )
	    '(moral ((men (obj-honest-men) true m83-1)
		     (honest (obj-honest-men) true m83-2)
		     (rogue (obj-rogue) true m83-3)
		     (benefit (obj-benefit obj-honest-men) true m83-4)
		     (handicap-for (obj-benefit obj-rogue) true m83-5)
		    )
             )
))



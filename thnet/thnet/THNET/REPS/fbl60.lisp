
; FILE:       /u2/thnet/data/arcs/fables60
; PURPOSE:    test retrieval using fables (numbers 53-60,62,63)
; PROGRAMMER: Greg Nelson
; CREATED:    7-22-88
; UPDATED:    7-22-88 - "DESIRE" -> "DESIRE-TRUE"
; UPDATED:    7-25-88 - Added hypotheticals.

(defun lfa60 () (load "//tinman/ucla/psych/emelz/ARCS/fables60"))
(defun makeloaded nil
  (defvar structures_loaded nil "Keep a list of all of the make_strucs done."))
(setq structures_loaded (append structures_loaded '(fable53 fable54 fable55 fable56 fable57 fable58 fable59 fable60 fable62 fable63)))

; REJECT LIST: fable61 - too long

; FABLE 53:  Evil for Good
; COMMENTS:  Perhaps like #33 and #34

(defun make_fable53 nil
  (make_struc 'fable53 'fable
	    '(story ((farmer (obj-farm-hand) true f53-1)
		     (snake (obj-snake) true f53-2)
		     (frozen (obj-snake) true f53-3)
		     (compassionate (obj-farm-hand) true f53-4)
		     (pick-up (obj-farm-hand obj-snake) true f53-5)
		     (warm-up (obj-farm-hand obj-snake) true f53-6)
		     (warm (obj-snake) true f53-7)
		     (bite (obj-snake obj-farm-hand) true f53-8)
		     (die (obj-farm-hand) true f53-9)
		    )
	     )
	    '(moral ((kindness (conc-kindness) true m53-1)
		     (great (conc-kindness) true m53-2)
		     (nature-of (conc-nature obj-actor) true m53-3)
		     (evil (conc-nature) true m53-4)
		     (change (conc-kindness conc-nature) false m53-5)
		    )
             )
))

; FABLE 54:  Cursed Above All Cattle -- Timeo Danaos et dona ferentes
; COMMENTS:  The moral is structurally very similar to the moral of #52, except
;            for the truth values and the object of the second proposition.

(defun make_fable54 nil
  (make_struc 'fable54 'fable
	    '(story ((god (obj-zeus) true f54-1)
		     (feast (obj-feast) true f54-2)
		     (marry (obj-zeus obj-wife) true f54-3)
		     (celebrate (obj-zeus f54-3) true f54-4)
		     (animals (obj-animals) true f54-5)
		     (gifts (obj-gifts) true f54-6)
		     (serpent (obj-serpent) true f54-7)
		     (rose (obj-rose) true f54-8)
		     (give-to (obj-animals obj-gifts obj-zeus) true f54-9)
		     (offer-to (obj-serpent obj-rose obj-zeus) true f54-10)
		     (take-from (obj-zeus obj-rose obj-serpent) false f54-11)
		     (evil (obj-serpent) true f54-12)
		     (cause (f54-12 f54-11) true f54-13)
		     (cause (f54-4 f54-2) true f54-14)
		     (cause (f54-4 f54-9) true f54-15)
		     (cause (f54-4 f54-10) true f54-16)
		    )
	     )
	    '(moral ((favor (obj-favor) true m54-1)
		     (evil (obj-evildoer) unknown m54-2)
		     (offer (obj-evildoer obj-favor) true m54-3)
		     (frightening (m54-3) unknown m54-4)
		     (if ((m54-2 true) (m54-4 true)) true m54-5)
		    )
             )
))

; FABLE 55:  United Against the Common Foe
; COMMENTS:  The moral relates to the fable in a rather bizarre way.  The fable
;            suggests that the weasel and snake are at fault, but seeing their
;            purpose again sets them straight.  The moral says that the
;            demagogues are doing fine, just quarreling among themselves like
;            they're supposed to (???) but when other people come along and
;            disturb them, those people are at fault for disturbing the
;            harmony. (???)  Weird...

(defun make_fable55 nil
  (make_struc 'fable55 'fable
	    '(story ((snake (obj-snake) true f55-1)
		     (weasel (obj-weasel) true f55-2)
		     (fight-with (obj-snake obj-weasel) true f55-3)
		     (mice (obj-mice) true f55-4)
		     (conjoin-object (obj-snake obj-weasel obj-snake-and-weasel) true f55-5)
		     (kill (obj-snake-and-weasel obj-mice) false f55-6)
		     (cause (f55-3 f55-6) true f55-7)
		     (walk (obj-mice) true f55-8)
		     (see (obj-snake-and-weasel f55-8) true f55-9)
		     (cause (f55-6 f55-8) true f55-10)
		     (end (f55-3) true f55-11)
		     (attack (obj-snake-and-weasel obj-mice) true f55-12)
		     (cause (f55-9 f55-11) true f55-13)
		     (cause (f55-9 f55-12) true f55-14)
		    )
	     )
	    '(moral ((people (obj-people) true m55-1)
		     (demagogue (obj-rival1) true m55-2)
		     (demagogue (obj-rival2) true m55-3)
		     (rivals (obj-rival1 obj-rival2) true m55-4)
		     (quarrel-between (obj-quarrel obj-rival1 obj-rival2) true m55-5)
		     (cause (m55-4 m55-5) true m55-6)
		     (get-involved-in (obj-people obj-quarrel) unknown m55-7)
		     (unite-with (obj-rival1 obj-rival2) unknown m55-8)
		     (attack (obj-rival1 obj-people) unknown m55-9)
		     (attack (obj-rival2 obj-people) unknown m55-10)
		     (if ((m55-7 true) (m55-8 true)) true m55-11)
		     (if ((m55-7 true) (m55-9 true)) true m55-12)
		     (if ((m55-7 true) (m55-10 true)) true m55-13)
		    )
             )
))

; FABLE 56:  Best Method of Defence

(defun make_fable56 nil
  (make_struc 'fable56 'fable
	    '(story ((snake (obj-snake) true f56-1)
		     (man (obj-man) true f56-2)
		     (people (obj-people) true f56-3)
		     (god (obj-zeus) true f56-4)
		     (tread-on (obj-man obj-snake) true f56-5)
		     (bite (obj-snake obj-man) false f56-6)
		     (tread-on (obj-people obj-snake) true f56-7)
		     (cause (f56-6 f56-7) true f56-8)
		     (say-to (obj-zeus f56-8 obj-snake) true f56-9)
		    )
	     )
	    '(moral ((assailant (obj-assailant) true m56-1)
		     (oppose (obj-assaulted obj-assailant) unknown m56-2)
		     (fear (obj-others obj-assaulted) unknown m56-3)
		     (assail (obj-others obj-assaulted) unknown m56-4)
		     (mutually-exclusive ((m56-3 true) (m56-4 true)) true m56-5)
		     (if ((m56-2 true) (m56-3 true)) true m56-6)
		    )
             )
))

; FABLE 57:  Vengeance At Any Price

(defun make_fable57 nil
  (make_struc 'fable57 'fable
	    '(story ((wasp (obj-wasp) true f57-1)
		     (snake (obj-snake) true f57-2)
		     (sting (obj-wasp obj-snake) true f57-3)
		     (occur-often (f57-3) true f57-4)
		     (wagon (obj-wagon) true f57-5)
		     (desire (obj-snake (f57-10 true)) true f57-6)
		     (crawl-under (obj-snake obj-wagon) true f57-7)
		     (kill (obj-wagon obj-snake) true f57-8)
		     (kill (obj-wagon obj-wasp) true f57-9)
		     (avenge (obj-snake f57-3) true f57-10)
		     (cause (f57-7 f57-8) true f57-11)
		     (cause (f57-7 f57-9) true f57-12)
		     (cause (f57-9 f57-10) true f57-13)
		     (die (obj-snake) true f57-14)
		     (cause (f57-8 f57-14) true f57-15)
		     (desire (obj-snake (f57-14 false)) true f57-16)
		     (prefer (obj-snake (f57-10 true) (f57-14 false)) true f57-17)
		    )
	     )
	    '(moral ((men (obj-men) true m57-1)
		     (enemies-of (obj-enemies obj-men) true m57-2)
		     (die (obj-men) unknown m57-3)
		     (die (obj-enemies) unknown m57-4)
		     (live (obj-men) unknown m57-5)
		     (if ((m57-3 true) (m57-4 true)) true m57-6)
		     (mutually-exclusive ((m57-3 true) (m57-5 true)) true m57-7)
		     (prefer (obj-men (m57-4 true) (m57-5 true)) true m57-8)
		     (occurs-sometimes (m57-8) true m57-9)
		    )
             )
))

; FABLE 58:  A Biter Bit
; COMMENTS:  Yet another story with the moral not clearly delineated.  This
;            story, in my opinion, deserves a loud "SAY... WHAT!?" -- nothing
;            is explicit, the moral is idiomatic, and it's not entirely clear
;            what it's even supposed to imply.

(defun make_fable58 nil
  (make_struc 'fable58 'fable
	    '(story ((snake (obj-snake) true f58-1)
		     (file (obj-file) true f58-2)
		     (bite (obj-snake obj-file) true f58-3)
		     (teeth-of (obj-teeth obj-snake) true f58-4)
		     (harder (obj-file obj-teeth) true f58-5)
		    )
	     )
	    '(moral ((rascal (obj-rascal) true m58-1)
		     (teeth-of (obj-rascals-teeth obj-rascal) true m58-2)
		     (bite (obj-rascal obj-sharper-bite) true m58-3)
		     (sharper (obj-sharper-bite obj-rascals-teeth) true m58-4)
		    )
             )
))

; FABLE 59:  Ill-Judged Rivalry
; COMMENTS:  The moral relies totally on the fable for some of its predicates.

(defun make_fable59 nil
  (make_struc 'fable59 'fable
	    '(story ((beast (obj-beasts) true f59-1)
		     (assembly-of (obj-assembly obj-beasts) true f59-2)
		     (monkey (obj-monkey) true f59-3)
		     (dance (obj-monkey) true f59-4)
		     (applaud-for (obj-beasts obj-monkey) true f59-5)
		     (cause (f59-4 f59-5) true f59-6)
		     (camel (obj-camel) true f59-7)
		     (jealous-of (obj-camel obj-monkey) true f59-8)
		     (cause (f59-5 f59-8) true f59-9)
		     (dance (obj-camel) true f59-10)
		     (ridiculous (obj-camel) true f59-11)
		     (cudgel (obj-beasts obj-camel) true f59-12)
		     (cause (f59-10 f59-11) true f59-13)
		     (cause (f59-11 f59-12) true f59-14)
		    )
	     )
	    '(moral ((people (obj-people) true m59-1)
		     (better (obj-betters obj-people) true m59-2)
		     (envy (obj-people obj-betters) true m59-3)
		     (compete (obj-people obj-betters) true m59-4)
		     (cause (m59-3 m59-4) true m59-5)
		     (scorn (obj-betters obj-people) true m59-6)
		     (cause (m59-4 m59-6) true m59-7)
		    )
             )
))

; FABLE 60:  Caught on the Blind Side
; COMMENTS:  The deer is blind in one eye!  Ugh, how do we represent that?

(defun make_fable60 nil
  (make_struc 'fable60 'fable
	    '(story ((deer (obj-deer) true f60-1)
		     (blind (obj-deer) true f60-2)
		     (graze (obj-deer) true f60-3)
		     (land (obj-inland) true f60-4)
		     (sea (obj-sea) true f60-5)
		     (watch (obj-deer obj-inland) true f60-6)
		     (watch (obj-deer obj-sea) false f60-7)
		     (men (obj-men) true f60-8)
		     (sail-on (obj-men obj-sea) true f60-9)
		     (see (obj-deer f60-9) false f60-10)
		     (mutually-exclusive ((f60-6 true) (f60-7 true)) true f60-11)
		     (cause (f60-2 f60-11) true f60-12)
		     (cause (f60-7 f60-10) true f60-13)
		     (shoot (obj-men obj-deer) true f60-14)
		     (cause (f60-10 f60-14) true f60-15)
		    )
	     )
	    '(moral ((expectations (conc-expectations) true m60-1)
		     (deceived (conc-expectations) true m60-2)
		     (thing (obj-feared) true m60-3)
		     (thing (obj-saviour) true m60-4)
		     (fear (obj-us obj-feared) true m60-5)
		     (help (obj-feared obj-us) true m60-6)
		     (save (obj-saviour obj-us) false m60-7)
		     (believe (obj-us (m60-7 true)) true m60-8)
		     (ruin (obj-saviour obj-us) true m60-9)
		     (conjoin-event (m60-6 m60-9) true m60-10)
		     (cause (m60-10 m60-2) true m60-11)
		     (occurs-often (m60-6) true m60-12)
		     (occurs-often (m60-9) true m60-13)
		    )
             )
))

; FABLE 61:  This is extremely long.  I'm not putting it in, for now.

; FABLE 62: A Breed of Faint-Hearts

(defun make_fable62 nil
  (make_struc 'fable62 'fable
	    '(story ((fawn (obj-fawn) true f62-1)
		     (deer (obj-deer) true f62-2)
		     (father-of (obj-deer obj-fawn) true f62-3)
		     (dogs (obj-dogs) true f62-4)
		     (larger-than (obj-deer obj-dogs) true f62-5)
		     (faster-than (obj-deer obj-dogs) true f62-6)
		     (horns-of (obj-horns obj-deer) true f62-7)
		     (flee-from (obj-deer obj-dogs) true f62-8)
		     (ask-why (obj-fawn obj-deer f62-8) true f62-9)
		     (fear (obj-deer obj-dogs) true f62-10)
		     (cause (f62-10 f62-8) true f62-11)
		     (say-to (obj-deer f62-11 obj-fawn) true f62-12)
		    )
	     )
	    '(moral ((man (obj-man) true m62-1)
		     (coward (obj-man) true m62-2)
		     (exhortation (conc-exhortation) true m62-3)
		     (much (conc-exhortation) true m62-4)
		     (end (m62-2) false m62-5)
		     (even-though (m62-4 m62-5) true m62-6)
		    )
             )
))

; FABLE 63:  The Irony of Fate
; COMMENTS:  Wow -- there is so much overlap in these fables.  See #60.  It
;            also has some hint of #35 & #40, about conceit.

(defun make_fable63 nil
  (make_struc 'fable63 'fable
	    '(story ((stag (obj-stag) true f63-1)
		     (spring (obj-spring) true f63-2)
		     (near (obj-stag obj-spring) true f63-3)
		     (reflection-of (obj-reflection obj-stag) true f63-4)
		     (see-in (obj-stag obj-reflection obj-spring) true f63-5)
		     (antlers-of (obj-antlers obj-stag) true f63-6)
		     (legs-of (obj-legs obj-stag) true f63-7)
		     (proud-of (obj-stag obj-antlers) true f63-8)
		     (dissatisfied-with (obj-stag obj-legs) true f63-9)
		     (lion (obj-lion) true f63-10)
		     (chase (obj-lion obj-stag) true f63-11)
		     (save-from (obj-legs obj-stag obj-lion) true f63-12)
		     (trees (obj-trees) true f63-13)
		     (catch (obj-trees obj-antlers) true f63-14)
		     (destroy (obj-antlers obj-stag) true f63-15)
		     (cause (f63-11 f63-12) true f63-16)
		     (cause (f63-14 f63-15) true f63-17)
		    )
	     )
	    '(moral ((danger (conc-danger) true m63-1)
		     (near (obj-us conc-danger) true m63-2)
		     (friends-of (obj-doubted obj-us) true m63-3)
		     (friends-of (obj-trusted obj-us) true m63-4)
		     (trust (obj-us obj-doubted) false m63-5)
		     (trust (obj-us obj-trusted) true m63-6)
		     (savior-of (obj-doubted obj-us) true m63-7)
		     (betray (obj-trusted obj-us) true m63-8)
		     (when (m63-2 m63-7) true m63-9)
		     (when (m63-2 m63-8) true m63-10)
		     (occurs-often (m63-9) true m63-11)
		     (occurs-often (m63-10) true m63-12)
		    )
             )
))



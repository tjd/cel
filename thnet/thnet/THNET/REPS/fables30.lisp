
; FILE:       /u2/thnet/data/arcs/fables30
; PURPOSE:    test retrieval using fables (numbers 22-31)
; PROGRAMMER: Greg Nelson
; CREATED:    6-15-88
; UPDATED:    6-16-88
; UPDATED:    7-21-88 - Cleaning up a few morals, changing to "UNKNOWN".
; UPDATED:    7-22-88 - "DESIRE" -> "DESIRE-TRUE"
; UPDATED:    7-25-88 - Added hypotheticals.

; REJECT LIST

(defun lfa30 () (load "//tinman/ucla/psych/emelz/ARCS/data/arcs/fables30"))
(defvar structures_loaded nil "Keep a list of all of the make_strucs done.")
(setq structures_loaded (append structures_loaded '(fable22 fable23 fable24 fable25 fable26 fable27 fable28 fable29 fable30 fable31)))

; FABLE 22:  A Companion in Fear
; COMMENTS:  This story suffers from a decided lack of moral!
;            What is the point of this story?	

(make_struc 'fable22 'fable
	    '(story ((lion (obj-lion) true f22-1)
		     (man (obj-prometheus) true f22-2)
		     (cock (obj-cock) true f22-3)
		     (elephant (obj-elephant) true f22-4)
		     (gnat (obj-gnat) true f22-5)
		     (fear (obj-lion obj-cock) true f22-6)
		     (blame-for (obj-lion obj-prometheus f22-6) true f22-7)
		     (fault-of (f22-6 obj-lion) true f22-8)
		     (say-to (obj-prometheus f22-8 obj-lion) true f22-9)
		     (sorrowful (obj-lion) true f22-10)
		     (fear (obj-elephant obj-gnat) true f22-11)
		     (see-that (obj-lion f22-11) true f22-12)
		     (end (f22-10) true f22-13)
		     (cause (f22-6 f22-7) true f22-14)
		     (cause (f22-8 f22-10) true f22-15)
		     (cause (f22-12 f22-13) true f22-16)
		    )
	     )
)

; FABLE 23:  The Mighty Fallen

(make_struc 'fable23 'fable
	    '(story ((lion (obj-lion) true f23-1)
		     (boar (obj-boar) true f23-2)
		     (bull (obj-bull) true f23-3)
		     (ass (obj-ass) true f23-4)
		     (revenge (conc-revenge) true f23-5)
		     (dying (obj-lion) true f23-6)
		     (attack (obj-boar obj-lion) true f23-7)
		     (attack (obj-bull obj-lion) true f23-8)
		     (attack (obj-ass obj-lion) true f23-9)
		     (ashamed (obj-lion) true f23-10)
		     (desire (obj-boar conc-revenge) true f23-11)
		     (desire (obj-bull conc-revenge) true f23-12)
		     (conjoin-event (f23-11 f23-6) true f23-13)
		     (conjoin-event (f23-12 f23-6) true f23-14)
		     (cause (f23-13 f23-7) true f23-15)
		     (cause (f23-14 f23-8) true f23-16)
		     (cause (f23-6 f23-9) true f23-17)
		     (weaker (obj-ass obj-lion) true f23-18)
		     (cause (f23-18 f23-10) true f23-19)
		    )
	     )
	    '(moral ((man (obj-man) true m23-1)
		     (coward (obj-coward) true m23-2)
		     (prestige (conc-prestige) true m23-3)
		     (lose (obj-man conc-prestige) true m23-4)
		     (abuse (obj-coward obj-man) true m23-5)
		     (cause (m23-4 m23-5) true m23-6)
		    )
             )
)

; FABLE 24:  A Respecter of Persons
; COMMENTS:  The moral of this story is of a very different type from most.
;            Rather than reiterating, it comments on the failure of humans to
;            follow the example given.

(make_struc 'fable24 'fable
	    '(story ((lion (obj-lion) true f24-1)
		     (bandit (obj-bandit) true f24-2)
		     (man (obj-traveller) true f24-3)
		     (bullock (obj-bullock) true f24-4)
		     (divide (obj-lion obj-bullock obj-parts) true f24-5)
		     (demand (obj-bandit obj-parts obj-lion) true f24-6)
		     (scold (obj-lion obj-bandit) true f24-7)
		     (depart (obj-bandit) true f24-8)
		     (fear (obj-traveller obj-lion) true f24-9)
		     (request (obj-traveller obj-parts obj-lion) false f24-10)
		     (give (obj-lion obj-parts obj-traveller) true f24-11)
		     (conjoin-event (f24-2 f24-6) true f24-12)
		     (cause (f24-12 f24-7) true f24-13)
		     (cause (f24-7 f24-8) true f24-14)
		     (good (obj-traveller) true f24-15)
		     (conjoin-event (f24-10 f24-15) true f24-16)
		     (cause (f24-16 f24-11) true f24-17)
		    )
	     )
	    '(moral ((avarice (conc-avarice) true m24-1)
		     (meekness (conc-meekness) true m24-2)
		     (treasure (obj-treasure) true m24-3)
		     (obtain (conc-avarice obj-treasure) true m24-4)
		     (obtain (conc-meekness obj-treasure) false m24-5)
		     (bad (m24-4) true m24-6)
		     (bad (m24-5) true m24-7)
		    )
             )
)

; FABLE 25:  Negotiating from Weakness
; COMMENTS:  These fables are getting considerably more abstract.  The
;            fable really does not specify what is to be shared.  This is
;            accomplished by ignoring what it is, although food seems most
;            likely given the general nature of the fables.

(make_struc 'fable25 'fable
	    '(story ((hares (obj-hares) true f25-1)
		     (lions (obj-lions) true f25-2)
		     (animals (obj-animals) true f25-3)
		     (meeting-of (obj-meeting obj-animals) f25-4)
		     (share (obj-lions obj-disputed) false f25-5)
		     (wrong (f25-5) true f25-6)
		     (say-to (obj-hares f25-6 obj-meeting) true f25-7)
		     (dangerous (obj-lions) true f25-8)
		     (become-true (f25-5) false f25-9)
		     (cause (obj-lions f25-9) true f25-10)
		     (cause (f25-8 f25-10) true f25-11)
		    )
	     )
)

; FABLE 26:  A Plotter Out-Plotted

(make_struc 'fable26 'fable
	    '(story ((lion (obj-lion) true f26-1)
		     (animal (obj-animals) true f26-2)
		     (fox (obj-fox) true f26-3)
		     (wolf (obj-wolf) true f26-4)
		     (sick (obj-lion) true f26-5)
		     (visit (obj-animals obj-lion) true f26-6)
		     (visit (obj-fox obj-lion) false f26-7)
		     (respect (obj-fox obj-lion) true f26-8)
		     (lie (obj-wolf f26-8 obj-lion) true f26-9)
		     (wolf (obj-dead-wolf) true f26-10)
		     (same-as (obj-dead-wolf obj-wolf) unknown f26-11)
		     (cure (obj-dead-wolf) false f26-12)
		     (lie (obj-fox f26-12 obj-lion) true f26-13)
		     (kill (obj-lion obj-wolf) true f26-14)
		     (become-true (f26-11) true f26-15)
		     (cause (f26-14 f26-15) true f26-16)
		     (cause (f26-5 f26-6) true f26-17)
		     (cause (f26-7 f26-9) true f26-18)
		     (cause (f26-9 f26-13) true f26-19)
		     (conjoin-event (f26-5 f26-13) true f26-20)
		     (cause (f26-20 f26-14) true f26-21)
		    )
	     )
	    '(moral ((man (obj-plotter) true m26-1)
		     (man (obj-another) true m26-2)
		     (plot-against (obj-plotter obj-another) true m26-3)
		     (destroyed (obj-plotter) true m26-4)
		     (cause (m26-3 m26-4) true m26-5)
		    )
             )
)

; FABLE 27:  The Wages Of Treachery

(make_struc 'fable27 'fable
	    '(story ((wolves (obj-wolves) true f27-1)
		     (dogs (obj-dogs) true f27-2)
		     (men (obj-men) true f27-3)
		     (sheep (obj-sheep) true f27-4)
		     (fold (obj-fold) true f27-5)
		     (inside-of (obj-sheep obj-fold) true f27-6)
		     (free (obj-wolves) true f27-7)
		     (slave-of (obj-dogs obj-men) true f27-8)
		     (guard (obj-dogs obj-sheep obj-wolves) true f27-9)
		     (suggest (obj-wolves (f27-11 true) obj-dogs) true f27-10)
		     (revolt-against (obj-dogs obj-men) true f27-11)
		     (end (f27-9) true f27-12)
		     (enter (obj-wolves obj-fold) true f27-13)
		     (kill (obj-wolves obj-dogs) true f27-14)
		     (cause (f27-8 f27-10) true f27-15)
		     (cause (f27-10 f27-11) true f27-16)
		     (cause (f27-11 f27-12) true f27-17)
		     (cause (f27-12 f27-13) true f27-18)
		     (cause (f27-13 f27-14) true f27-19)
		    )
	     )
	    '(moral ((country-of (obj-country obj-traitor) true m27-1)
		     (traitor-to (obj-traitor obj-country) true m27-2)
		     (die (obj-traitor) true m27-3)  ; implied by fable
		     (cause (m27-2 m27-3) true m27-4)
		    )
             )
)

; FABLE 28:  Always In The Wrong

(make_struc 'fable28 'fable
	    '(story ((wolf (obj-wolf) true f28-1)
		     (lamb (obj-lamb) true f28-2)
		     (river (obj-river) true f28-3)
		     (father (obj-father obj-wolf) true f28-4)
		     (desire (obj-wolf (f28-11 true)) true f28-5)
		     (muddy (obj-lamb obj-river) false f28-6)
		     (lie (obj-wolf f28-6 obj-lamb) true f28-7)
		     (insult (obj-lamb obj-father) false f28-8)
		     (lie (obj-wolf f28-8 obj-lamb) true f28-9)
		     (devour (obj-wolf obj-lamb) true f28-10)
		     (justified (f28-10) false f28-11)
		     (cause (f28-5 f28-7) true f28-12)
		     (cause (f28-5 f28-9) true f28-13)
		     (conjoin-event (f28-6 f28-8) true f28-14)
		     (cause (f28-14 f28-11) true f28-15)
		    )
	     )
	    '(moral ((man (obj-man) true m28-1)
		     (desire (obj-man (m28-5 true)) true m28-2)
		     (plead-with (obj-other obj-man) true m28-3)
		     (justified (m28-3) true m28-4)
		     (attack (obj-man obj-other) true m28-5)
		     (ignore (obj-man m28-3) true m28-6)
		     (cause (m28-2 m28-6) true m28-7)
		     (conjoin-event (m28-2 m28-7) true m28-8)
		     (cause (m28-8 m28-5) true m28-9)
		    )
             )
)

; FABLE 29:  Kindness Ill Requited

(make_struc 'fable29 'fable
	    '(story ((wolf (obj-wolf) true f29-1)
		     (heron (obj-heron) true f29-2)
		     (bone (obj-bone) true f29-3)
		     (fee (obj-fee) true f29-4)
		     (swallow (obj-wolf obj-bone) true f29-5)
		     (desire (obj-wolf (f29-8 true)) true f29-6)
		     (lie (obj-wolf (f29-9 false) obj-heron) true f29-7)
		     (remove (obj-heron obj-bone obj-wolf) true f29-8)
		     (pay (obj-wolf obj-heron obj-fee) false f29-9)
		     (request (obj-heron obj-fee obj-wolf) true f29-10)
		     (threaten (obj-wolf obj-heron) true f29-11)
		     (cause (f29-7 f29-8) true f29-12)
		     (cause (f29-9 f29-10) true f29-13)
		     (cause (f29-10 f29-11) true f29-14)
		    )
	     )
	    '(moral ((man (obj-bad-man) true m29-1)
		     (service (obj-service) true m29-2)
		     (reward (obj-reward) true m29-3)
		     (bad (obj-bad-man) true m29-4)
		     (give (obj-actor obj-service obj-bad-man) true m29-5)
		     (give (obj-bad-man obj-reward obj-actor) false m29-6)
		     (cause (m29-4 m29-6) true m29-7)
		    )
             )
)

; FABLE 30:  The Pot Calls The Kettle Black
; COMMENTS:  The moral given here, as usual, does not address the
;            content of the story.

(make_struc 'fable30 'fable
	    '(story ((wolf (obj-wolf) true f30-1)
		     (sheep (obj-sheep) true f30-2)
		     (flock (obj-flock) true f30-3)
		     (lion (obj-lion) true f30-4)
		     (steal-from (obj-wolf obj-sheep obj-flock) true f30-5)
		     (steal-from (obj-lion obj-sheep obj-wolf) true f30-6)
		     (scold (obj-wolf obj-lion) true f30-7)
		     (cause (f30-6 f30-7) true f30-8)
		     (scorn (obj-lion obj-wolf) true f30-9)
		     (conjoin-event (f30-5 f30-7) true f30-10)
		     (cause (f30-10 f30-9) true f30-11)
		    )
	     )
	    '(moral ((thieves (obj-thieves) true m30-1)
		     (lucky (obj-thieves) false m30-2)
		     (argue (obj-thieves obj-thieves) m30-3)
		     (cause (m30-2 m30-3) true m30-4)
		     (bad (m30-3) true m30-5)
	            )
	     )
)

; FABLE 31:  A Communist Dictator
; COMMENTS:  I didn't know Aesop was politically interested...

(make_struc 'fable31 'fable
	    '(story ((wolf (obj-leader) true f31-1)
		     (wolves (obj-other-wolves) true f31-2)
		     (ass (obj-ass) true f31-3)
		     (food (obj-food) true f31-4)
		     (leader-of (obj-leader obj-other-wolves) true f31-5)
		     (decree (obj-leader (f31-7 true)) true f31-6)
		     (share (obj-other-wolves obj-food) unknown f31-7)
		     (share (obj-leader obj-food) false f31-8)
		     (say (obj-ass f31-8 obj-other-wolves) true f31-9)
		     (ashamed (obj-leader) true f31-10)
		     (cause (f31-6 f31-7) true f31-11)
		     (cause (f31-8 f31-9) true f31-12)
		     (cause (f31-9 f31-10) true f31-13)
		    )
	     )
	    '(moral ((men (obj-leaders) true m31-1)
		     (laws (obj-laws) true m31-2)
		     (leaders-of (obj-leaders obj-others) true m31-3)
		     (make (obj-leaders obj-laws) true m31-4)
		     (follow (obj-leaders obj-laws) false m31-5)
		     (just (obj-leaders) false m31-6)
		     (cause (m31-5 m31-6) true m31-7)
		     (pretend (obj-leaders (m31-6 true)) true m31-8)
		    )
             )
)



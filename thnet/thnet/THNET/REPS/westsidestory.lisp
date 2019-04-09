
; FILE:		data/arcs/westsidestory
; PURPOSE:  	Romeo and Juliet analogue to test reasonableness of 
;		  retrieval in plays.
; PROGRAMMER:	David Gochfeld
; CREATED:	10-18-88 


(defun lwss () (load "//tinman/ucla/psych/emelz/ARCS/westsidestory"))
(defvar structures_loaded nil "Keep a list of all of the make_strucs done.")
(setq structures_loaded (append structures_loaded '(west-side-story)))

; West Side Story
;

(make_struc 'west-side-story 'play
            '(characters ((man (obj-tony) true wsc-1)
                          (woman (obj-maria) true wsc-2)
                          (man (obj-riff) true wsc-3)
                          (man (obj-bernardo) true wsc-4)
                          (man (obj-chico) true wsc-5)
                          (friends (obj-tony obj-riff) true wsc-6)
                          (siblings (obj-maria obj-bernardo) true wsc-7)
                          (gang (obj-jets) true wsc-8)
                          (gang (obj-sharks) true wsc-9)
                          (member-of (obj-tony obj-jets) true wsc-10)
                          (member-of (obj-riff obj-jets) true wsc-11)
                          (member-of (obj-bernardo obj-sharks) true wsc-12)
                          (member-of (obj-chico obj-sharks) true wsc-13)
                          (city (obj-new-york-city) true wsc-14)
                          (woman (obj-anita) true wsc-15)
                          (friends (obj-anita obj-maria) true wsc-16)
                         )
             )
            '(plot ((hate (obj-jets obj-sharks) true ws-1)
                    (hate (obj-sharks obj-jets) true ws-2)
                    (conjoin-event (ws-1 ws-2) true ws-3)
                    (love (obj-tony obj-maria) true ws-4)
                    (love (obj-maria obj-tony) true ws-5)
                    (conjoin-event (ws-4 ws-5) true ws-6)
                    (forbidden (ws-6) true ws-7)
                    (cause (ws-3 ws-7) true ws-8)
                    (love (obj-anita obj-bernardo) true ws-9)
                    (love (obj-bernardo obj-anita) true ws-10)
                    (kill (obj-bernardo obj-riff) true ws-11)
                    (cause (ws-3 ws-11) true ws-12)
                    (kill (obj-tony obj-bernardo) true ws-13)
                    (cause (ws-11 ws-13) true ws-14)
                    (dead (obj-maria) false ws-20)
                    (say (obj-anita (ws-20 true) obj-tony) true ws-21)
                    (conjoin-event (ws-6 ws-13) true ws-22)
                    (cause (ws-22 ws-21) true ws-23)
                    (believe (obj-tony (ws-20 true)) true ws-24)
                    (kill (obj-chico obj-tony) true ws-25)
                    (request (obj-tony ws-25) true ws-26)
                    (conjoin-event (ws-24 ws-4) true ws-27)
                    (cause (ws-27 ws-26) true ws-28)
                    (end (ws-1) true ws-30)
                    (end (ws-2) true ws-31)
                    (cause (ws-25 ws-30) true ws-32)
                    (cause (ws-25 ws-31) true ws-33)
                   )
             )
)



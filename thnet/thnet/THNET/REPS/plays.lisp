
; FILE:       data/arcs/plays
; PURPOSE:    load all plays files
; CREATED:    7-28-88
; UPDATED:    10-18-88  Now loads westsidestory as well.


; FILE:       data/arcs/comedies, data/arcs/tragedies
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

(defun lplay () (load "//tinman/ucla/psych/emelz/ARCS/plays")) 

(defun plays ()
  (load "//tinman/ucla/psych/emelz/ARCS/tragedies") ; tragedies and histories
  (load "//tinman/ucla/psych/emelz/ARCS/comedies") ; comedies and problems plays
  (load "//tinman/ucla/psych/emelz/ARCS/westsidestory") ; analog for Romeo and Juliet
)

(plays)


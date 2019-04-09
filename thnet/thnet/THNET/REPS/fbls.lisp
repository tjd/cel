

; FILE:       analogs/fable/fables
; PURPOSE:    load all of the other fable files
; PROGRAMMER: Greg Nelson
; CREATED:    6-24-88
; UPDATED:    10-18-88  Now loads sourgrapes.analog as well. (Dave Gochfeld)

; REJECT LIST
; FABLES 16,32


(defun fables ()
  (load "/home/castor/emelz/THNET/REPS/fbl10") ; Numbers 1-10
  (load "/home/castor/emelz/THNET/REPS/fbl20") ; Numbers 11-15,17-21
  (load "/home/castor/emelz/THNET/REPS/fbl30") ; Numbers 22-31
  (load "/home/castor/emelz/THNET/REPS/fbl40") ; Numbers 33-42
  (load "/home/castor/emelz/THNET/REPS/fbl50") ; Numbers 43-52
  (load "/home/castor/emelz/THNET/REPS/fbl60") ; Numbers 53-60,62,63
  (load "/home/castor/emelz/THNET/REPS/fbl70") ; Numbers 64-73
  (load "/home/castor/emelz/THNET/REPS/fbl80") ; Numbers 74-83
  (load "/home/castor/emelz/THNET/REPS/fbl90") ; Numbers 84-93
  (load "/home/castor/emelz/THNET/REPS/fblA0") ; Numbers 94-103

)

(fables)


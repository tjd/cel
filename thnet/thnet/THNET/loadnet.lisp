; FILE:  loadnet.lisp
; LOADING FILES FOR NETWORKS:

(setf *code-pathname* "/home/castor/emelz/THNET/")

(defun load_net ()

; miscellaneous:  
   (load (concatenate 'string *code-pathname* "init"))
   (load (concatenate 'string *code-pathname* "misc"))
   (load (concatenate 'string *code-pathname* "util"))

; run network:
   (load (concatenate 'string *code-pathname* "links"))
   (load (concatenate 'string *code-pathname* "run"))
   (my_print '"Net files loaded.")

; analogy:
   (load (concatenate 'string *code-pathname* "analogy"))
   (load (concatenate 'string *code-pathname* "acme"))
   (load (concatenate 'string *code-pathname* "struc"))
   (load (concatenate 'string *code-pathname* "semantics"))
   (load (concatenate 'string *code-pathname* "transfer"))
   ;; search stuff in arcsutil can be used by acme, too
   (load (concatenate 'string *code-pathname* "arcsutil"))
   (my_print "ACME files loaded.")



; grapher:
  (if load_graphics?
     (load (concatenate 'string *code-pathname* "graphics")))
;   (my_print "Graphics loaded.")


; bug fixes and extensions:
   (load (concatenate 'string *code-pathname* "patches"))

   (my_print " ")
   (init_net)
   (my_print " ")
)   


(defun load_echo
   (load (concatenate 'string *code-pathname* "echo"))
   (my_print "ECHO file loaded.") 
;  (uses variable load_arcs?, defined as nil by default)
   (if load_arcs? (load_arcs)))


(defun load_arcs ()
  (load (concatenate 'string *code-pathname* "arcs"))
  (my_print "Functions loaded.")
  (load (concatenate 'string *code-pathname* "loadsem"))
  (my_print "Semantics loaded.")
  (defaults 'arcs)
  (my_print "ARCS defaults established.")
)

(defun compile_thnet (&optional (which '(init misc util run struc acme arcs echo graphics semantics transfer patches)))
  (prog ()
    (if (member 'init which) (compile-file (concatenate 'string *code-pathname* "init.lisp")))
    (if (member 'misc which) (compile-file (concatenate 'string *code-pathname* "misc.lisp")))
    (if (member 'util which) (compile-file (concatenate 'string *code-pathname* "util.lisp")))
    (if (member 'run which) (compile-file (concatenate 'string *code-pathname* "run.lisp")))
    (if (member 'struc which) (compile-file (concatenate 'string *code-pathname* "struc.lisp")))
    (if (member 'acme which) (compile-file (concatenate 'string *code-pathname* "acme.lisp")))
    (if (member 'semantics which) (compile-file (concatenate 'string *code-pathname* "semantics.lisp")))
    (if (member 'arcs which) (compile-file (concatenate 'string *code-pathname* "arcs.lisp")))
    (if (member 'echo which) (compile-file (concatenate 'string *code-pathname* "echo.lisp")))
    (if (member 'transfer which) (compile-file (concatenate 'string *code-pathname* "transfer.lisp")))
    (if (member 'graphics which) (compile-file (concatenate 'string *code-pathname* "graphics.lisp")))
    (if (member 'patches which) (compile-file (concatenate 'string *code-pathname* "patches.lisp")))
  )
)
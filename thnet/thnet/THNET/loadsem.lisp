
(defun lsem ()
  (load "/home/castor/emelz/THNETsemantics")
)

(defun semantics (&optional init)
  (prog ()
    (if (and (null init) all_concepts) (return "Semantics_ntics already loaded."))
    (load "/home/castor/emelz/THNET/semantics_a")
    (load "/home/castor/emelz/THNET/semantics_e")
    (load "/home/castor/emelz/THNET/semantics_i")
    (load "/home/castor/emelz/THNET/semantics_n")
    (load "/home/castor/emelz/THNET/semantics_s")
    (semantics_a)
    (semantics_b)
    (semantics_c)
    (semantics_d)
    (semantics_e)
    (semantics_f)
    (semantics_g)
    (semantics_h)
    (semantics_i)
    (semantics_jk)
    (semantics_l)
    (semantics_m)
    (semantics_n)
    (semantics_o)
    (semantics_p)
    (semantics_qr)
    (semantics_s)
    (semantics_t)
    (semantics_uv)
    (semantics_wxyz)
  )
)

(defun no_semantics ()
  (mapcar #'clear_props all_concepts)
  (setq all_concepts nil)
)



